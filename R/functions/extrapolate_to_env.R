extrapolate_to_env <- function(
                               gfbootstrap_combined,
                               gfbootstrap_predicted,
                               gfbootstrap_cluster,
                               env_domain_plot,
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
        return(data.table(gfbootstrap_cluster[, .(env_domain, trophic, survey, depth_cat, clust_method)],
													env_pred_stats = list(NA),
													env_id = list(NA),
													imp_preds = list(NA),
													pred_sim_mat = list(NA) ##double wrap the sim mat so data.table doesn't try to print it
                          ))
    }
    
    ## Make sure env domain is ready

    env_dom <- env_domain_plot[domain ==  gfbootstrap_combined$env_domain, data][[1]]

    if (gfbootstrap_combined$depth_cat !=  "all" ) {
        env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gfbootstrap_combined$depth_cat]]), ]
    }

    predicted <- predict(object = gfbootstrap_combined$gfbootstrap[[1]],
                         newdata = env_dom[,..env_biooracle_names],
                         ## Just take points, and calculate full coefficient matrix from points
                         type = c("points"),
                         extrap = extrap)
    pred_points <- predicted$points
    data.table::setDT(pred_points)

		imp_preds <- gfbootstrap_predicted$imp_preds[[1]]
		
		## Batch GPU version: One giant matrix, operate on subsets
		## See ./predict_gfbootstrap.R for code comments
		pred_wide <- data.table::dcast(
																 pred_points,
																 x_row + gf ~ pred,
																 value.var = "y",
																 subset = .(pred %in% imp_preds))
		pred_wide[, gf := NULL]
		pred_wide[, x_row := NULL]

		n_x_row <- nrow(env_dom)
		n_gf <- length(gfbootstrap_combined$gfbootstrap[[1]]$gf_list)
		n_preds <- length(imp_preds)
		rm(predicted)
		rm(pred_points)
		pred_wide_array <-array(as.matrix(pred_wide), c(n_gf, n_x_row, n_preds))
		rm(pred_wide)
		pred_wide_batch <- aperm(pred_wide_array, c(2,3,1))
		rm(pred_wide_array)

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
		
		pred_wide_tensor <- torch_tensor(pred_wide_batch, device = local_device)
		rm(pred_wide_batch)
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
				if (mem_max <= overhead) {
						stop("extrapolate_to_env.R: Memory overheads exceed allocated memory.")
				}
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
		
		## current simulation says 2904 bytes per row
		## 3443526 rows per batch should use ~10GB
		## Watching memory usage showed ~25GB usage
		## Adding * 3 to give better accuracy

		cluster_site_mean <- torch_tensor(gfbootstrap_predicted$env_pred_stats[[1]]$site_mean, device = local_device)
		cluster_site_sigma <- torch_tensor(gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma, device = local_device)
		cluster_site_sigma_det <- torch_tensor(gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma_det, device = local_device)

		n_cluster_sites <- length(cluster_site_sigma_det)
		

		overhead <-
				## CUDA module overhead
				222e6 +
				## Site mean
				n_x_row * n_preds * size_dtype +
				## site sigma
				n_x_row * n_preds ^ 2 * size_dtype +
				## site sigma det
				n_x_row * size_dtype + 
				## Clustering sites
				n_cluster_sites * n_preds * size_dtype +
				n_cluster_sites * n_preds ^ 2 * size_dtype +
				n_cluster_sites * size_dtype
				## Distance tensor
				## n_cluster_sites * n_x_row * size_dtype

		
		mem_per_pair <- size_dtype * (
				5 * n_preds ^ 2 +
				4 * n_preds +
				10)

		site_pairs <- data.table::CJ(cluster = seq.int(nrow(gfbootstrap_predicted$env_id[[1]])),
                                 new = seq.int(n_x_row)[nonsingular_det_sites])
		
		if (is.na(mem_max <- as.numeric(Sys.getenv("TENSOR_MEM_MAX", "")))) {
				n_row_batch <- nrow(site_pairs)
		} else {
				if (mem_max <= overhead) {
						stop("extrapolate_to_env.R: Memory overheads exceed allocated memory.")
				}
				n_row_batch <- floor((mem_max - overhead) / mem_per_pair)
		}
		n_batches <- ceiling(nrow(site_pairs) / n_row_batch)


		site_pairs[ , batch_ind := rep(seq.int(n_batches), each = n_row_batch, length.out = nrow(site_pairs))]
		
		## reworking into a torch_cat style workflow,
		## so GPU->CPU only happens once at the end.
		## For very large datasets, better
		## to run much bigger batches and transfer
		## after each batch to avoid
		## using all the GPU memory storing previous batch results.
		bhatt_list <- site_pairs[ ,
						    list(bhatt_dist = list(bhattacharyya_dist_tensor(
									 .SD[ , .(cluster, new)],
									 cluster_site_mean,
									 cluster_site_sigma,
									 cluster_site_sigma_det,
									 site_mean,
									 site_sigma,
									 site_sigma_det)$to(device = "cpu"))),
									 by = batch_ind]

		bhatt_vec <- torch_cat(bhatt_list$bhatt_dist)
		
		sim_mat <- matrix(as.numeric(bhatt_vec),
											length(nonsingular_det_sites),
											nrow(gfbootstrap_predicted$env_id[[1]]))

		## Unset gpu.matrix in predicted_stats
		predicted_stats <- list(site_mean = as.matrix(site_mean$to(device = "cpu")),
														site_sigma = array(as.numeric(site_sigma$to(device = "cpu")), dim(site_sigma)),
														site_sigma_det = as.numeric(site_sigma_det$to(device = "cpu")))
		
    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
         env_pred_stats = list(predicted_stats),
				 env_id = list(env_dom[,..env_id_col]),
				 imp_preds = list(imp_preds),
				 pred_sim_mat = list(list(sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
                                  ))
}
