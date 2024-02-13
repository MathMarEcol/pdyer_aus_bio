extrapolate_to_env <- function(
                               gfbootstrap_combined,
                               gfbootstrap_predicted,
                               env_domain,
                               env_biooracle_names,
                               extrap,
                               pred_importance_top,
                               env_id_col,
                               depth_range
                               ) {

    options(torch.cuda_allocator_reserved_rate = 0.60)
    options(torch.cuda_allocator_allocated_rate = 0.8)



    if (all(is.na(gfbootstrap_predicted$env_id))) {
        ## Upstream target decided survey was not usable.
        ## Propagating
        ##
        return(data.table(gfbootstrap_predicted[, .(env_domain, res_gf, res_clust, trophic, survey, depth_cat)],
                          env_year = env_biooracle_names$env_year,
                          env_pathway = env_biooracle_names$env_pathway,
													env_pred_stats = list(NA),
													env_id = list(NA),
													imp_preds = list(NA),
													extrap_sims = list(NA) ##double wrap the sim mat so data.table doesn't try to print it
                          ))
    }

    gfbootstrap_combined$gfbootstrap <- list(qs::qread(gfbootstrap_combined$gfbootstrap[[1]]))
    
    ## Make sure env domain is ready

  names_to_present <- function(n) {
    str_remove(n, "RCP[0-9]{2}_[0-9]{4}_")
  }

    
    env_names <- env_biooracle_names$env_biooracle_names[[1]]
    env_names <- names_to_present(env_names)
    env_dom <- env_domain[domain ==  gfbootstrap_combined$env_domain &
                               res == gfbootstrap_combined$res_gf &
                               env_year == env_biooracle_names$env_year &
                               env_pathway == env_biooracle_names$env_pathway
                             , data][[1]]

    ## GF was fitted with present day env layers
    ## and attempts to match predictor names.
    ## Need to rename future layers for GF.
    names(env_dom) <- names_to_present(names(env_dom))


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
  n_preds_raw <- length(env_names)

  ## Testing again with row 104
  ## mem_per_site is smaller, 151168
  ## OOMed though
  ## targeting 10GB expected usage
  ## 66152 rows
  ## Pre
  ## mem_used 869Mb
  ## Top 1472Mb
  ## During
  ## top jumped to 9.8, crept up to 11.8GB, then jumped to 17.9GB at the end
  ## The creep is expected, similarities are moved back to cpu in batches.
  ## Creep was about 1GB larger than expected, I see I was missing the doubling.
  ## Post
  ## mem_used 5.97GB as expected.
  ## top 17.9GB
  ## top is bigger than expected.
  ## Expectation. 1.5GB initial,
  ## Expected 11+1.5, about 12.5GB. Bigger by 5.4GB
  ## Explainable by a copy of the return, but size R size, not top size.
  ## In the case of row 104, return value is much larger than memory per site.

  ## predict(gf_object) creates an object. Maybe it copies it at some point
  ## It does, it converts a list to a data.frame. Hence the x2
  ## Fixed in gfbootstrap and extrap
  ## Prior to fix, expected 4.9GB RAM for predict object, maybe more
  ## predict probably makes at least one other copy.
  ##


  ## Trying again.
  ## Overhead allocated 36GB for return values.
  ## predicted object should eat 4.9GB, but be 2.45GB in R
  ## Batch return should be 5.1GB in R, may eat 10GB RAM due to copies

  ## expecting:
  ## 1.5GB + 5GB -> 6.5GB top for predcited object
  ## 6.5GB + 10GB for batch return value

  ## Predicted object alone jumped to 11.2GB. Explained by
  ## predicted object being 4* expected size. 3x available for GC
  ## After using predicted object to load up GPU, gc() reclaimed
  ## 11.2 - 8.9GB -> 2.3GB.

  ## site_pairs and pair_batches are very large objects.
  ## RAM 8.9 -> 16.1GB, 7.2GB.
  ## Above expected 6.3GB
  ## So peak usage. Is it
  ## 4 * predicted scores, 4*sites*n_gf * n_preds_raw * (
  ##   size_doubles * 3 +
  ##     size_int * 2
  ## )? 10GB
  ## 2 * site_pairs and pair_batches and bhatt_list.
  ## sites * old sites * (size_int * 5 + size_doubles)i? 17.8GB

  ## New logic, find max.
  ## currently, mem_per_site is dominated by prediction object.
  ##

  ## 17.4GB  max RAM for bhatt vec
  ## Targeting 20GB usage
  ## 8.9GB
  ## 16.1GB

  ## targeting 13.85GB
  ## 1.4GB
  ## 8.2GB with slight peak. Perfectly on 2* expected size plus prior usage
  ## gc() to 5.4GB, 1x exprected size plus prior is 4.96
  ## setkeyv pushed memory to 6.2GB, still within 2* expected size
  ## gc drops to 4.2GB
  ## pred stats pushed back to 6.84GB
  ## dropping predicted object and related returns to 4.7GB
  ##

  ## Going into bhatt section with
  ## 4.7GB in top
  ## 8.2GB after site_pairs. Exactly 1x expected object size plus prior!
  ## 13.3GB after pair_batches 1.5x expected size. Some copying?
  ## 15GB after adding batch_ind to site_pairs. Exactly as expected.
  ## Size is consistent with 6 copies of each int col
  ## 1 reclaimed by GC 13.3GB
  ## bhatt increased to 15GB, int col? Float32!
  ## dropping batch_ind and adding new indexes took 2 int cols extra ram
  ## gc dropped to 13.3 again.
  ## adding the bhatt_vec jumped to 20.2GB, 6.9GB increase
  ## Could have been 7.2, which would be 4*, float to double, then copy the doibl.

  ## total useage per row
  ## let pred size be
  ## (
  ##     ## x, y, x_row, pred, gf
  ##     n_gf * n_preds_raw * (
  ##         size_doubles * 3 +
  ##         size_int * 2
  ## )
    ## let pair_col_int size be nrow(gfbootstrap_predicted$sim_mat[[1]][[1]]) * size_int
    pred_obj_size <- n_gf * n_preds_raw * (
          size_doubles * 3 +
            size_int * 2
    )
    pair_col_int_size <- nrow(gfbootstrap_predicted$sim_mat[[1]][[1]]) * size_int

    ## Trialling new mem_per_site logic. Expecting peak 32.8GB plus overhead
    ## of one of 17GB, 14GB or 24GB.
    ## So peak of 48, 46, 56.
    ## peak at 22 so far in first batch
    ## 27 at end of first batchz
    ## 28 at start of second batch
    ## peak of 33 at end of second batch.

    ## removing Gc from new_sites_process.
    ## started with 12
    ## first batch peak 24
    ## first batch end peak 31GB
    ## second batch start peak
    ## seond batch final peak 32GB.
    ## No batching
    ## 13GB to 46GB.
    ## Hmm, fixed?
    ## Testing on row 105
    ## pre top 11GB
    ## batch 1 peak at 39GB
    ## batch 2 peak 41GB
    ## Good. Slightly over estimated, but that is ok.
    mem_per_site <-  pred_obj_size *
    ## generate pred
    (2 +
      ## setkey and pred stats
     1) +
      pair_col_int_size *
        ## site_pairs
        (2 +
          ## pair_batches
          3 +
          ## batch into site_pairs
          1 +
          ## returned bhatt col
          1 +
          ## bhatt_col into double, and copy into site_pair
          4)

    
    ## mem_per_site <- max(
    ##   ## OS RAM usage is approx 2x larger
    ##   ## than size of R objects reported by object_size
    ##   ## mem_used
    ##   4 * (
    ##     ## x, y, x_row, pred, gf
    ##     n_gf * n_preds_raw * (
    ##       size_doubles * 3 +
    ##         size_int * 2
    ##     )),
    ##       ## return value new, cluster, bhatt
    ##       (size_int * 5 + size_doubles) * nrow(gfbootstrap_predicted$sim_mat[[1]][[1]])
    ##   )

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
																											 env_names,
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

    gc()
    torch::cuda_empty_cache()
    return(data.table::setDT(list(
                           env_domain = gfbootstrap_combined$env_domain,
                           env_year = env_biooracle_names$env_year,
                           env_pathway = env_biooracle_names$env_pathway,
                           res_gf = gfbootstrap_combined$res_gf,
                           res_clust = gfbootstrap_combined$res_clust,
                           trophic = gfbootstrap_combined$trophic,
                           survey = gfbootstrap_combined$survey,
                           depth_cat = gfbootstrap_combined$depth_cat,
                           ## env_pred_stats = list(predicted_stats),
                           env_id = list(env_dom[, ..env_id_col]),
                           imp_preds = list(imp_preds),
                           extrap_sims = list(similarity_long)
  )))
}


