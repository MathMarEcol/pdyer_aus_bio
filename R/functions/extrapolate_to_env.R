extrapolate_to_env <- function(
                               gfbootstrap_combined,
                               gfbootstrap_predicted,
                               env_domain_plot,
                               env_biooracle_names,
                               extrap,
                               pred_importance_top,
                               env_id_col,
                               depth_range
                               ) {


    if (all(is.na(gfbootstrap_combined$gfbootstrap))) {
        ## Upstream target decided survey was not usable.
        ## Propagating
        ##
        return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
													env_pred_stats = list(NA),
													env_id = list(NA),
													imp_preds = list(NA),
													extrap_sims = list(NA) ##double wrap the sim mat so data.table doesn't try to print it
                          ))
    }
    
    ## Make sure env domain is ready

    env_dom <- env_domain_plot[domain ==  gfbootstrap_combined$env_domain, data][[1]]

    if (gfbootstrap_combined$depth_cat !=  "all" ) {
        env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gfbootstrap_combined$depth_cat]]), ]
    }
		imp_preds <- gfbootstrap_predicted$imp_preds[[1]]

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
		
		mem_per_site <-
				## x, y, x_row, pred, gf
				n_gf * n_preds_raw * (
						size_doubles * 3 +
						size_int * 2
				)

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
									0,
									mem_per_site,
									## predict.gfbootstrap has long overheads
									## and risk of OOM is much lower on CPU
									max_batch_size = NA))

		## Long form or wide form?
		## Long form. Memory usage in long form is still much smaller than
		## memory needed to calculate each distance value, less than 1%
		## Long form makes combinations more general.

		## New bug: long form can end up exceeding R vector limits
		## of 2^31-1. Data.table does not yet support long vectors internally.
		## Returning list of values.

		## However, I don't see why this code is hitting the limits.
		##
		similarity_long <- env_dom_batch[ ,
																		 list(site_pairs = list(new_sites_process(.SD$site,
																											 env_dom,
																											 env_biooracle_names,
																											 gfbootstrap_combined$gfbootstrap[[1]],
																											 gfbootstrap_predicted,
																											imp_preds,
																											mem_max,
																											local_device,
																											extrap))),
										 by = batch_ind]

		
		## sim_mat <- matrix(similarity_long$bhatt_vec,
											## n_x_row,
											## nrow(gfbootstrap_predicted$env_id[[1]]))

		## Unset gpu.matrix in predicted_stats
		## predicted_stats <- list(site_mean = as.matrix(site_mean$to(device = "cpu")),
		## 												site_sigma = array(as.numeric(site_sigma$to(device = "cpu")), dim(site_sigma)),
		## 												site_sigma_det = as.numeric(site_sigma_det$to(device = "cpu")))
		
    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
         ## env_pred_stats = list(predicted_stats),
				 env_id = list(env_dom[,..env_id_col]),
				 imp_preds = list(imp_preds),
				 extrap_sims = list(similarity_long) 
                                  ))
}


