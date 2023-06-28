predict_gfbootstrap <- function(
                                gfbootstrap_combined,
                                env_domain,
                                env_biooracle_names,
                                extrap,
                                pred_importance_top,
                                env_id_col,
                                depth_range
                                ) {
    options(torch.cuda_allocator_reserved_rate = 0.60)
    options(torch.cuda_allocator_allocated_rate = 0.8)

 if (all(is.na(gfbootstrap_combined$gfbootstrap))) {
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

    imp <- gradientForest::importance(gfbootstrap_combined$gfbootstrap[[1]], sort = TRUE)

    if (sum(imp) == 0) {
      ## GF bootstrap object is broken
      return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
        env_pred_stats = list(NA),
        env_id = list(NA),
        imp_preds = list(NA),
        sim_mat = list(NA)
      ))
    }
    if (pred_importance_top >= 1) {
        imp_preds <- names(imp)[seq.int(1,min(length(imp), pred_importance_top))]
    } else {
        imp_explained <- cumsum(imp)/sum(imp)
        ## Take all predictors below threshold, then one more
        n_preds <- sum(imp_explained < pred_importance_top) + 1
        imp_preds <- names(imp)[seq.int(1,n_preds)]
    }


		## One very large variant was 150Gb in memory. Need to batch process
		## Here we are batching the rows passed to predict.gfbootstrap,
	  ## which cannot use GPU memory.
		ram_max <- as.numeric(Sys.getenv("TENSOR_CPU_MEM_MAX", ""))

    ## R uses doubles by default.
		## Strings are part of global string pool, so
		## usage for a string is just a pointer, especially
		## in this case where ~30 strings are used as labels.

		size_doubles <- 8
		size_int <- 4
		n_x_row <- nrow(env_dom)
		n_gf <- length(gfbootstrap_combined$gfbootstrap[[1]]$gf_list)
		n_preds_raw <- length(env_biooracle_names)
    n_preds <- length(imp_preds)

		mem_per_site <-
        ## OS RAM usage is approx 2x larger
        ## than size of R objects reported by object_size
        ## mem_used
        2 *
				## x, y, x_row, pred, gf
				n_gf * n_preds_raw * (
						size_doubles * 3 +
						size_int * 2
				) +
        ## return value site_sigma, site_mean, site_sigma_det
				## Site mean
			  n_preds * size_doubles +
				## site sigma
				n_preds ^ 2 * size_doubles +
				## site sigma det
				size_doubles


    overhead <-
      ## R background usage?
        10e9 +
        ## possible site stats if not using GPU
        + n_x_row *
        ## return value site_sigma, site_mean, site_sigma_det
				## Site mean
			  (n_preds * size_doubles +
				## site sigma
				n_preds ^ 2 * size_doubles +
				## site sigma det
				size_doubles) +
      ## return value handshake rule on sites: i, j, similarity.
      (size_int * 2 + size_doubles) * (n_x_row - 1) * (n_x_row)/2

    torch_set_default_dtype(torch_float32())
		size_dtype <- 4

    ## GPU is often faster, but is not always available.
		if (Sys.getenv("TENSOR_DEVICE") == "CUDA") {
				if (torch::cuda_is_available()) {
						local_device <- torch_device("cuda")
  					mem_max <- as.numeric(Sys.getenv("TENSOR_GPU_MEM_MAX", ""))
				} else {
						stop("extrapolate_to_env.R: Cuda device requested by env var TENSOR_DEVICE, but torch::cuda_is_available() == FALSE")
				}
		} else {
				local_device <- torch_device("cpu")
				mem_max <- as.numeric(Sys.getenv("TENSOR_CPU_MEM_MAX", ""))
		}

    env_dom_batch <- data.table::as.data.table(prepare_batch(ram_max,
                                                             n_x_row,
                                                             overhead,
                                                             mem_per_site,
                                                             ## predict.gfbootstrap has long overheads
                                                             ## and risk of OOM is much lower on CPU
                                                             max_batch_size = NA))
    pred_stats_list <- env_dom_batch[ ,
																		 cluster_sites_process(.SD$site,
																											 env_dom,
																											 env_biooracle_names,
																											 gfbootstrap_combined$gfbootstrap[[1]],
																											imp_preds,
																											mem_max,
																											local_device,
																											extrap),
										 by = batch_ind]


  site_sigma <- torch_cat(pred_stats_list$site_sigma, 1)
  site_sigma_det <- torch_cat(pred_stats_list$site_sigma_det, 1)
  site_mean <- torch_cat(pred_stats_list$site_mean, 1)


		nonsingular_det_sites <- as.logical(site_sigma_det$isfinite()$to(device = "cpu"))

    ## If all sites have determinant 0, cannot use.
    ## Not clear why, indicates matrix is not invertible
    ## Matrices do not have obvious problems like col of 0
    ## or eigs of 0, but some eigs are complex.
    ## Problem is likely computational, and I had some
    ## success inverting sigma with linalg_eigh,
    ## and calculating det from the eigh eigenvalues,
    ## but case is rare enough to leave for now.
    if (all(nonsingular_det_sites == FALSE)) {
        return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
                          env_pred_stats = list(NA),
                          env_id = list(NA),
                          imp_preds = list(NA),
                          sim_mat = list(NA)
                          ))
    }


		row_pairs <- data.table::CJ(i = seq.int(n_x_row)[nonsingular_det_sites], j = seq.int(n_x_row)[nonsingular_det_sites])
		row_pairs_filtered <- row_pairs[ i < j, ]


		overhead <-
				## CUDA module overhead
				222e6 +
				## Site mean
				n_x_row * n_preds * size_dtype +
				## site sigma
				n_x_row * n_preds ^ 2 * size_dtype +
				## site sigma det
				n_x_row * size_dtype + 
				## Distance tensor
				nrow(row_pairs_filtered) * size_dtype

		mem_per_pair <- size_dtype * (
				3 * n_preds ^ 2)

    pair_batches <- data.table::as.data.table(prepare_batch(
																		mem_max,
																		nrow(row_pairs_filtered),
																		overhead,
																		mem_per_pair,
																		max_batch_size = 30000))

		row_pairs_filtered[ , batch_ind := pair_batches$batch_ind]

		bhatt_list <-
        row_pairs_filtered[ ,
                           list(bhatt_dist = list(tryCatch(
                                    bhattacharyya_dist_tensor(
													 .SD[ , .(i, j)],
													 site_mean,
													 site_sigma,
													 site_sigma_det,
													 site_mean,
													 site_sigma,
													 site_sigma_det)$to(device = "cpu"),
                           error = function(e) e))),
                           by = batch_ind]

    ## Sometimes each site has det != 0 but
    ## the joint covariance matrix does have det == 0.
    ## Do not use branch if this is the case.
    if (!all(vapply(
             bhatt_list$bhatt_dist,
             function(x) {
                 inherits(x, "torch_tensor")
             },
             logical(1)))) {
        return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
                          env_pred_stats = list(NA),
                          env_id = list(NA),
                          imp_preds = list(NA),
                          sim_mat = list(NA)
      ))
    }

		bhatt_vec <- torch_cat(bhatt_list$bhatt_dist)
    bhatt_vec$nan_to_num_(0)


		sim_mat <- torch_sparse_coo_tensor(t(as.matrix(row_pairs_filtered[,.(i,j)])),
																				 bhatt_vec,
																				 c(n_x_row, n_x_row))$to_dense()$to(device = "cpu")

		sim_mat <- sim_mat + sim_mat$transpose(1,2) + torch_diag(rep(1, n_x_row))
		sim_mat<- as.matrix(sim_mat)

		## Can have issues with rounding errors creating similarities greater than 1
		sim_mat[sim_mat > 1] <- 1 


		## Unset gpu.matrix in predicted_stats
		predicted_stats <- list(site_mean = as.matrix(site_mean$to(device = "cpu")),
														site_sigma = array(as.numeric(site_sigma$to(device = "cpu")), dim(site_sigma)),
														site_sigma_det = as.numeric(site_sigma_det$to(device = "cpu")))

  return(data.table::setDT(list(
    env_domain = gfbootstrap_combined$env_domain,
    trophic = gfbootstrap_combined$trophic,
    survey = gfbootstrap_combined$survey,
    depth_cat = gfbootstrap_combined$depth_cat,
    env_pred_stats = list(predicted_stats),
    env_id = list(env_dom[, ..env_id_col]),
    imp_preds = list(imp_preds),
    sim_mat = list(list(sim_mat)) ## double wrap the sim mat so data.table doesn't try to print it
  )))

}
