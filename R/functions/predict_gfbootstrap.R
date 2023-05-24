predict_gfbootstrap <- function(
                                gfbootstrap_combined,
                                env_domain,
                                env_biooracle_names,
                                extrap,
                                pred_importance_top,
                                env_id_col,
                                depth_range
                                ) {

 if (any(is.na(gfbootstrap_combined$gfbootstrap[[1]]))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(NA),
			env_id = list(NA),
      imp_preds = list(NA),
      sim_mat = list(NA)
    ))
 }

  env_dom <- env_domain[domain ==  gfbootstrap_combined$env_domain, data][[1]]

  if (gfbootstrap_combined$depth_cat !=  "all" ) {
			env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gfbootstrap_combined$depth_cat]]), ]
  }
	## Assume 6000 sites, 1000 gf, 30 preds
	## Size: sites * gf * (preds + 3) * float64 = 1.4GB
  predicted <- predict(object = gfbootstrap_combined$gfbootstrap[[1]],
                       newdata = env_dom[,..env_biooracle_names],
                       ## Just take points, and calculate full coefficient matrix from points
                       type = c("points"),
                       extrap = extrap)
  pred_points <- predicted$points
  data.table::setDT(pred_points)
  ## With only 20 trees, and therefore 20 samples, for fitting 28 dimensions, covariance matrices are coming out
  ## singular. Test by dropping most predictors.
  ## May even automatically add a top 80% of variance method
  imp <- importance(gfbootstrap_combined$gfbootstrap[[1]], sort = TRUE)
  if (pred_importance_top >= 1) {
    imp_preds <- names(imp)[seq.int(1,min(length(imp), pred_importance_top))]
  } else {
    imp_explained <- cumsum(imp)/sum(imp)
    ## Take all predictors below threshold, then one more
    n_preds <- sum(imp_explained < pred_importance_top) + 1
    imp_preds <- names(imp)[seq.int(1,n_preds)]
  }


		## Batch GPU version: One giant matrix, operate on subsets
		pred_wide <- data.table::dcast(
																 pred_points,
																 x_row + gf ~ pred,
																 value.var = "y",
																 subset = .(pred %in% imp_preds))
		pred_wide[, gf := NULL]
		pred_wide[, x_row := NULL]
		## size: sites * gf * imp_preds * float64 (8 bytes), still ~1.4GB

		n_x_row <- nrow(env_dom)
		n_gf <- length(gfbootstrap_combined$gfbootstrap[[1]]$gf_list)
		n_preds <- length(imp_preds)
		rm(predicted)
		rm(pred_points)
		## r x c
		## pred_wide is (x_row*gf) x (n_preds)
		## I want to batch into (x_row) x  (npreds) x (gf)
		## to align with cov
		## matrix inner veector currently starts with a pred, goes through all gfs in an x_row, starts a new x_row.
		## so vector is (from slowest to fastest changing) (pred), (x_row), (gf)
		## array goes from fastest changing to slowest. Reverse the order for array
		pred_wide_array <-array(as.matrix(pred_wide), c(n_gf, n_x_row, n_preds))
		rm(pred_wide)
		## torch assumes the leftmost dim is the slices, so leftmost needs to be x_row.
		## also, rows are variables (preds). Is it still row x col? Yes.
		pred_wide_batch <- aperm(pred_wide_array, c(2,3,1))
		rm(pred_wide_array)
		## t -> (npreds) x (x_row *gf)
		## c -> (
		##;; pred_wide_batch <- aperm(array(t(pred_wide), c(n_preds,n_preds,n_x_row)), c(3,1,2))

		## 32bit may be faster, and uses half the memory
		## per tensor element
		torch_set_default_dtype(torch_float32())
		size_dtype <- 4

				## GPU is often faster, but is not always available.
		if (Sys.getenv("TENSOR_DEVICE") == "CUDA") {
				if (torch::cuda_is_available()) {
						local_device <- torch_device("cuda")
				} else {
						stop("extrapolate_to_env.R: Cuda device requested by env var TENSOR_DEVICE, but torch::cuda_is_available() == FALSE")
				}
		} else {
				local_device <- torch_device("cpu")
		}


		
		## Size: sites * gf * preds *float32 = 0.77GB
		pred_wide_tensor <- torch_tensor(pred_wide_batch, device = local_device)
		rm(pred_wide_batch)
		
		## Size: sites * preds * float32 = 720Kb  <- Lots of gfs dropped here
		site_mean <- torch_mean(pred_wide_tensor, 3)

		overhead <-
				## fixed overhead by CUDA
				222e6 +
				## site_mean
				size_dtype * n_x_row * n_preds +
				## pred_wide_tensor
				size_dtype * n_x_row * n_gf * n_preds +
				## site_sigma
				size_dtype * n_x_row * n_preds^2 
		
		
		mem_per_site <- size_dtype * (
				## allocate subset of pred_wide_tensor
				n_gf * n_preds +
				## tmp_mean
				n_preds +
				## tmp_diff with intermerdiate then reshape
				2 * n_gf * n_preds +
				## tmp_prods, no intermediates seem to be generated
				n_gf * n_preds ^ 2 +
				## batched_cov assuming one intermediate then div 
				n_gf * n_preds * 2 
		)

		if (is.na(mem_max <- as.numeric(Sys.getenv("TENSOR_MEM_MAX", "")))) {
				n_row_batch <- n_x_row
		} else {
				n_row_batch <- floor((mem_max - overhead) / mem_per_site)
		}
		n_batches <- ceiling(n_x_row / n_row_batch)

		batch_dt <- data.table(batch_ind = rep(seq.int(n_batches), each = n_row_batch, length.out = n_x_row), x_row = seq.int(n_x_row))
		
		batch_list <- batch_dt[ , list(batch_rows = list(.SD$x_row)), by = batch_ind]

		site_sigma_list <- list()
		for (n in seq.int(n_batches)) {
				site_sigma_list[[n]] <- t_batch_cov(pred_wide_tensor[batch_list$batch_rows[[n]],,], batch_list$batch_rows[[n]], n_gf, n_preds)
		}
		site_sigma <- torch_cat(site_sigma_list, 1)
		rm(pred_wide_tensor)
		rm(site_sigma_list)
		
		site_sigma_det <- torch_slogdet(site_sigma)[[2]]

		nonsingular_det_sites <- as.logical(site_sigma_det$isfinite()$to(device = "cpu"))
		row_pairs <- data.table::CJ(i = seq.int(n_x_row)[nonsingular_det_sites], j = seq.int(n_x_row)[nonsingular_det_sites])
		## Size = (sites^2 - sites)/2 *integer(4bytes)*2 = 143Mb (250Mb before filtering
		row_pairs_filtered <- row_pairs[ i < j, ]

		## Batch into appropriate memory size chunks
		## As shown in ./bhattacharyya_dist_tensor.R, max memory is:
		## row_pairs * 4 <float32> * (6 + 4 * preds + 2 * preds ^2)
		## current simulation says 2904 bytes per row
		## 3443526 rows per batch should use ~10GB
		## Watching memory usage showed ~25GB usage
		## Adding * 3 to give better accuracy

		overhead <-
				## CUDA module overhead
				222e6 +
				## Site mean
				n_x_row * n_preds * size_dtype +
				## site sigma
				n_x_row * n_preds ^ 2 * size_dtype +
				## site sigma det
				n_x_row * size_dtype
		
		mem_per_pair <- size_dtype * (
				5 * n_preds ^ 2 +
				4 * n_preds +
				10)
		
		if (is.na(mem_max <- as.numeric(Sys.getenv("TENSOR_MEM_MAX", "")))) {
				n_row_batch <- nrow(row_pairs_filtered)
		} else {
				n_row_batch <- floor(mem_max / mem_per_pair)
		}
		n_batches <- ceiling(nrow(row_pairs_filtered) / n_row_batch)

		row_pairs_filtered[ , batch_ind := rep(seq.int(n_batches), each = n_row_batch, length.out = nrow(row_pairs_filtered))]

		row_pairs_filtered[ ,
											 bhatt_dist :=	as.numeric(bhattacharyya_dist_tensor(
													 .SD[ , .(i, j)],
													 site_mean,
													 site_sigma,
													 site_sigma_det,
													 site_mean,
													 site_sigma,
													 site_sigma_det)),
											 by = batch_ind]
		row_pairs_filtered[ , batch_ind := NULL]

		sim_mat <- torch_sparse_coo_tensor(t(as.matrix(row_pairs_filtered[,.(i,j)])),
																				 row_pairs_filtered$bhatt_dist,
																				 c(n_x_row, n_x_row))$to_dense()$to(device = "cpu")

		sim_mat <- sim_mat + sim_mat$transpose(1,2) + torch_diag(rep(1, n_x_row))
		sim_mat<- as.matrix(sim_mat)
		## implements
		## bhattacharyya_dist <- 0.125 *
				## ((t(joint_mean) %*% joint_cov_inv) %*% joint_mean) +
				## 0.5 * log(
									## exp(joint_det) /
									## (sqrt(exp(site_sigma_det[x]) * exp(site_sigma_det[y])))
							## )

		## this is all wrapped in a log, apply
		##log quotient ->
		## log(exp(joint_det)) - log(sqrt(exp(detx) * exp(dety)))
		## log e and log power ->
		## joint_det - 0.5 log(exp(detx) * exp(dety))
		## log mult ->
		## joint_det - 0.5 * ( log(exp(detx)) + log(exp(dety)))
		## log e again ->
		## joint_det - 0.5 * ( detx + dety)
		## This works because I was already taking the log
		## of each determinant with torch_slogdet
		## Numerically, using logs and addition is better.


		## Unset gpu.matrix in predicted_stats
		predicted_stats <- list(site_mean = as.matrix(site_mean$to(device = "cpu")),
														site_sigma = array(as.numeric(site_sigma$to(device = "cpu")), dim(site_sigma)),
														site_sigma_det = as.numeric(site_sigma_det$to(device = "cpu")))

		return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(predicted_stats),
			env_id = list(env_dom[,..env_id_col]),
      imp_preds = list(imp_preds),
      sim_mat = list(list(sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
    ))

}
