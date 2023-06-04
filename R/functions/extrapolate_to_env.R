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
				) +
        ## return value new, cluster, bhatt
        (size_int * 2 + size_doubles) * nrow(gfbootstrap_predicted$sim_mat[[1]][[1]])

    overhead <-
        ## R background usage?
        10e9 +
        ## return value new, cluster, bhatt
        (size_int * 2 + size_doubles) * nrow(gfbootstrap_predicted$sim_mat[[1]][[1]]) * n_x_row

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
    max_batch_size = NA
  ))

  ## Long form or wide form?
  ## Long form. Memory usage in long form is still much smaller than
  ## memory needed to calculate each distance value, less than 1%
  ## Long form makes combinations more general.

  ## New bug: long form can end up exceeding R vector limits
  ## of 2^31-1. Data.table does not yet support long vectors internally.
  ## Returning list of values.

  ## However, I don't see why this code is hitting the limits.
  ##

  ## pryr::mem_used() = 1.3GB for 10th row
  ## top reports 2.4GB used and 9.8GB virtual
  ## Initial jump to 43GB real, 60GB virtual and virtual stops at 43GB
  ## drops to 23GB and gradually creeps up by small amounts.
  ## I think the creep is $to(cpu) in the bhatt vector.
  ## Sudden jump at the end to 41GB in two spikes, then drop to 20GB
  ## Net growth is 13GB total according to mem_change
  ## Generating return value created a copy, adding another 13GB.
    ## copy was removed by gc().

    ## Does the code create 13GB of data, copy it,
    ## then leave it lying around?
    ## ~8GB of background usage is also present.
    ## 13GB object in R leads to 30GB jump in RAM
    ## No, 15GB object. Similarity_long was 13GB for batch 1.
    ## 8GB background, 15GB predicted object, 13GB return object: 36GB

    ## return jump to 54GB, drop to 48GB, then 37GB
    ## second batch starts with 46GB RAM
    ## 58GB jump drop to 50GB
    ## mem_used is only 22.9GB
    ## however, even after GC, 43GB RAM
    ## Similarity_long is well defined,
    ## ordered by cluster against each new site.
    ## It does form matrix blocks, where each block is
    ## a set of rows against all columns.
    ## A transpose would allow the distances to be concatenated.

    ## Trying with half ram allocation
    ## 3 batches
    ## 3.5GB initial
    ## Jump to 19GB, peak 21GB. GPU just eats 5GB ram anyway
    ##   15.5GB jump, 7.5GB plus return of 6.5GB? Pretty close, 1.5GB missing
    ## Jump to 27GB after batch, drop to 18 again
    ## Jump to 37 with new batch, drop to 30
    ##   10GB old return and initial, some uncollected GC, then new 18GB. Again 7.5 plus 6.5, and 3GB missing.
    ## Post batch jump to 44, drop to 35
    ## New small batch only a few GB increase
    ## Final ram 45GB

    ## Adding overhead for final object, and 10GB background usage.
    ## expecting final usage to stay within 45GB

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

    return(data.table::setDT(list(
        env_domain = gfbootstrap_combined$env_domain,
        trophic = gfbootstrap_combined$trophic,
        survey = gfbootstrap_combined$survey,
        depth_cat = gfbootstrap_combined$depth_cat,
    ## env_pred_stats = list(predicted_stats),
    env_id = list(env_dom[, ..env_id_col]),
    imp_preds = list(imp_preds),
    extrap_sims = list(similarity_long)
  )))
}


