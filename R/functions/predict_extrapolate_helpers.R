## Returns a list of vectors with
## batch indicies and site indices
## Uses env var "TENSOR_MEM_MAX" to decide batch sizes
## max_mem max memory size to use (in bytes) If NA, use max_batch_size.
## n_sites is number of elements being batched
## overhead is expected memory already in use (in bytes)
## mem_per_site is expected memory usage per site (in bytes)
## max_batch_size sets upper limit on batch size. If NA, no max
##
## Use max_batch_size for tuning.
## If batches are too small, loop overheads and CPU-GPU comms
## dominate, slowing down run.
## If batches are too large, memory instability occurs.
## Find lowest batch_size where batch run time is linear with batch size.
## OR find lowest batch_size where increasing batch size does not decrease
## total run time.
prepare_batch <- function(
													mem_max,
													n_sites,
													overhead,
													mem_per_site,
													max_batch_size
													) {
		if (is.na(mem_max))  {
				n_row_batch <- n_sites
		} else {
				if (mem_max <= overhead) {
						stop("extrapolate_to_env.R: Memory overheads exceed allocated memory.")
				}
				n_row_batch <- floor((mem_max - overhead) / mem_per_site)
				if(!is.na(max_batch_size) && (n_row_batch > max_batch_size)) n_row_batch <- max_batch_size
		}
		n_batches <- ceiling(n_sites / n_row_batch)

		batch_ind <- list(batch_ind = rep(seq.int(n_batches), each = n_row_batch, length.out = n_sites), site = seq.int(n_sites))

}

## Process env for clustering

cluster_sites_process <- function(x_rows,
                                  env_dom,
                                  env_biooracle_names,
                                  gf_object,
                                  imp_preds,
                                  mem_max,
                                  local_device,
                                  extrap) {
  predicted <- predict(
    object = gf_object,
    newdata = env_dom[x_rows, ..env_biooracle_names],
    ## Just take points, and calculate full coefficient matrix from points
    type = c("points"),
    extrap = extrap,
    avoid_copy  = TRUE
  )
  ## Skip casting, go straight to tensor.
  setDT(predicted)
  predicted[, x := NULL]
  ## Setting key orders rows appropriately
  ## The returned object from predict.gfbootstrap
  ## changes along (from slowest to fastest)
  ## gf, pred, xrow.
  ## But tensor needs
  ## xrow, pred, gf
  setkeyv(predicted, c("x_row", "pred", "gf"))
  ## Needed for batching
  ## predicted$points[, x_row := NULL]
  ## predicted$points[, pred := NULL]
  predicted[, gf := NULL]
  ## 32bit may be faster, and uses half the memory
  ## per tensor element
  torch_set_default_dtype(torch_float32())
  size_dtype <- 4

  n_gf <- length(gf_object$gf_list)
  n_preds <- length(imp_preds)
  n_x_row <- length(x_rows)

  mem_per_site <- size_dtype * (
    ## site_mean
    n_preds +
      ## pred_wide_tensor
      n_gf * n_preds +
      ## site_sigma
      n_preds^2
  )

  ## fixed overhead by CUDA
  overhead <- 222e6


  pred_batches <- data.table::as.data.table(prepare_batch(
    mem_max,
    n_x_row,
    overhead,
    mem_per_site,
    max_batch_size = NA
  ))
  ## These were wrong. Smaller batches processed less rows.
  ## 5000 x 1000 7s 10s 12s 15s
  ## 10000 x 1000  17.64s 15s
  ## 20000 x 1000 39s 51s 51s

  ## Now with 80000 rows to process.
  ## 100 x 100 1.12m (not 1:12) 50s
  ## 1000 x 1000 58s
  ## 2000 x 1000 51s
  ##   2000 x 2000 25s 59ssensitive to inner batches. Stars aligned for first run
  ##   4000 x 2000 55s
  ##   4000 x 4000 57s
  ## 5000 x 1000 55s
  ## 10000 x 1000 50s
  ##


  ## Need to batch this process too.
  pred_stats_list <- pred_batches[,
    site_stats(
      ## Using view to fail on copy
      torch_tensor(predicted[x_row %in% .SD$site & pred %in% imp_preds, y], device = local_device, pin_memory=TRUE)$view(list(nrow(.SD), n_preds, n_gf)),
      size_dtype,
      mem_max,
      NA
    ),
    by = batch_ind]


  site_sigma <- torch_cat(pred_stats_list$site_sigma, 1)
  site_sigma_det <- torch_cat(pred_stats_list$site_sigma_det, 1)
  site_mean <- torch_cat(pred_stats_list$site_mean, 1)

  ## End batching of input matrices
  return(list(
    site_sigma = list(site_sigma),
    site_sigma_det = list(site_sigma_det),
    site_mean = list(site_mean)
  ))
}




## Load in and process new env
##
## ram_max is CPU RAM, never GPU
new_sites_process <- function(
															x_rows,
															env_dom,
															env_biooracle_names,
															gf_object,
															gfbootstrap_predicted,
															imp_preds,
															mem_max,
															local_device,
															extrap) {

		predicted <- predict(object = gf_object,
                         newdata = env_dom[x_rows, ..env_biooracle_names],
                         ## Just take points, and calculate full coefficient matrix from points
                         type = c("points"),
                         extrap = extrap,
                         avoid_copy = TRUE)
		## Skip casting, go straight to tensor.
		setDT(predicted)
		predicted[, x := NULL]
		## Setting key orders rows appropriately
		## The returned object from predict.gfbootstrap
		## changes along (from slowest to fastest)
		## gf, pred, xrow.
		## But tensor needs
		## xrow, pred, gf
		setkeyv(predicted, c("x_row", "pred", "gf"))
    ## Needed for batching
		## predicted$points[, x_row := NULL]
		## predicted$points[, pred := NULL]
		predicted[, gf := NULL]
		## 32bit may be faster, and uses half the memory
		## per tensor element
		torch_set_default_dtype(torch_float32())
		size_dtype <- 4

		n_gf <- length(gf_object$gf_list)
		n_preds <- length(imp_preds)
		n_x_row <- length(x_rows)

		mem_per_site <- size_dtype * (
				## site_mean
				n_preds +
				## pred_wide_tensor
				n_gf * n_preds +
				## site_sigma
				n_preds^2
		)

		## fixed overhead by CUDA
		overhead <- 222e6


		pred_batches <- data.table::as.data.table(prepare_batch(
																		mem_max,
																		n_x_row,
																		overhead,
																		mem_per_site,
																		max_batch_size = NA))
		## These were wrong. Smaller batches processed less rows.
		## 5000 x 1000 7s 10s 12s 15s
		## 10000 x 1000  17.64s 15s
		## 20000 x 1000 39s 51s 51s

		## Now with 80000 rows to process.
		## 100 x 100 1.12m (not 1:12) 50s
		## 1000 x 1000 58s
		## 2000 x 1000 51s
		##   2000 x 2000 25s 59ssensitive to inner batches. Stars aligned for first run
		##   4000 x 2000 55s
		##   4000 x 4000 57s
		## 5000 x 1000 55s
		## 10000 x 1000 50s
		##


		## Need to batch this process too.
		pred_stats_list <- pred_batches[ ,
																		site_stats(
																				## Using view to fail on copy
																				torch_tensor(predicted[x_row %in% .SD$site & pred %in% imp_preds, y], device = local_device, pin_memory=TRUE)$view(list(nrow(.SD), n_preds, n_gf)),
																				size_dtype,
																				mem_max,
																				NA),
																		by = batch_ind]


		site_sigma <- torch_cat(pred_stats_list$site_sigma, 1)
		site_sigma_det <- torch_cat(pred_stats_list$site_sigma_det, 1)
		site_mean <- torch_cat(pred_stats_list$site_mean, 1)

		rm(pred_stats_list)
		rm(pred_batches)
		rm(predicted)

				## Batch GPU version: One giant matrix, operate on subsets
		## See ./predict_gfbootstrap.R for code comments


		## End batching of input matrices

		nonsingular_det_sites <- as.logical(site_sigma_det$isfinite()$to(device = "cpu"))

		## current simulation says 2904 bytes per row
		## 3443526 rows per batch should use ~10GB
		## Watching memory usage showed ~25GB usage
		## Adding * 3 to give better accuracy

		cluster_site_mean <- torch_tensor(gfbootstrap_predicted$env_pred_stats[[1]]$site_mean, device = local_device, pin_memory=TRUE)
		cluster_site_sigma <- torch_tensor(gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma, device = local_device, pin_memory=TRUE)
		cluster_site_sigma_det <- torch_tensor(gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma_det, device = local_device, pin_memory=TRUE)

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

####### Debugging from here.
		## Does not run at 3x
		## Does not run at 3.5x
		## 4.5x manual GC runs in 14.34s 13.96s
		## 5x manual GC runs in 14.93s
		##   Time is linear with rows, doubled rows, 29s then 58s
		## 4.5x no GC not stable
		## 5x no GC not stable
		## 5.5x no GC not stable
		## 6x no GC runs in 15.21s. Not stable
		## 7x no GC not stable
		## Stable at 20x no GC 8.56s. Add GC: 39.81s
		## Stable at 10x no GC 12.8s
		## stable at 9x no GC 13.37
		## stable at 8x no GC 13.92
		## Still seeing speedups at 40x 7.63s, 8.6 13s
		## Slower at 80x 12.8s
		## 60x 10.61 12.26s
		## 50x 12.7s
		## 45 12.37s
		## 41 10.8s
		## 30 9.06s
		## 35 12.15s
		## What is special about 72000 rows?
		## Not much

		## Limiting factor is not batches OR loop
		## Too little memory crashes.
		## Lots of small loops: each loop runs quickly, expected looping overhead.
		## A few big loops: little looping overhead, lots of time spent in loop

		## likely explanation:
		## grinding the batched matrices is the limiting factor.
		## Not GC, not looping.
		## If grinding is roughly linear with size, then
		## above a lower batch size, grinding will be the limit.

		## At 10 rows per batch, overheads matter more.
		## Max GPU utilisation occurs around
		## above 1000, but by 10000

		mem_per_pair <- size_dtype * (
				8 * n_preds ^ 2)

		site_pairs <- data.table::CJ(cluster = seq.int(nrow(gfbootstrap_predicted$env_id[[1]])),
                                 new = seq.int(n_x_row)[nonsingular_det_sites])


		pair_batches <- data.table::as.data.table(prepare_batch(
																		mem_max,
																		nrow(site_pairs),
																		overhead,
																		mem_per_pair,
																		max_batch_size = NA))

		site_pairs[ , batch_ind := pair_batches$batch_ind]

		## reworking into a torch_cat style workflow,
		## so GPU->CPU only happens once at the end.
		## For very large datasets, better
		## to run much bigger batches and transfer
		## after each batch to avoid
		## using all the GPU memory storing previous batch results.
		bhatt_list <- site_pairs[ ,
														 list(bhatt_dist = list(
																			bhattacharyya_dist_tensor_compiled(
																					.SD[ , .(cluster, new)],
                                          local_device,
																					cluster_site_mean,
																					cluster_site_sigma,
																					cluster_site_sigma_det,
																					site_mean,
																					site_sigma,
																					site_sigma_det)$to(device = "cpu"))),
														 by = batch_ind]
		site_pairs[, batch_ind := NULL]
		site_pairs[, new := x_rows[new]]
  site_pairs[, bhatt_vec := as.numeric(torch_cat(bhatt_list$bhatt_dist)$nan_to_num_(0))]
		data.table::setkeyv(site_pairs, c("cluster", "new"))
		return(site_pairs)
}


## Generate Site statistics for batch
##
## takes an array pred_wide with dimensions [site, pred, gf]
##
## Each site has a set of predictors
##
## site_mean [site, preds] (mean predictor values at site)
## site_sigma [site, preds, preds] (covariance matrix of predictors at site)
## site_sigma_det [site] (determinant of site_sigma)
##
## Returns a list with each of these concatenated
## so that the lowest dimension gives site
site_stats <- function(
											 pred_wide,
											 size_dtype,
											 mem_max,
											 max_sigma_batch_size
											 ) {

		n_x_row <- dim(pred_wide)[1]
		n_preds <- dim(pred_wide)[2]
		n_gf <- dim(pred_wide)[3]


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

		site_sigma_batches <- data.table::as.data.table(prepare_batch(
																					mem_max,
																					n_x_row,
																					overhead,
																					mem_per_site,
																					max_batch_size = max_sigma_batch_size))

		site_sigma_list <- site_sigma_batches[ ,
																					list(site_sigma =
																									 list(t_batch_cov(
																											 pred_wide[.SD$site,,], .SD$site, n_gf, n_preds))),
																					by = batch_ind]
		site_sigma = torch_cat(site_sigma_list$site_sigma, 1)
		site_sigma_det = torch_logdet(site_sigma)
		site_mean = torch_mean(pred_wide, 3)

		out <- list(
				site_sigma = list(site_sigma),
				site_sigma_det = list(site_sigma_det),
				site_mean =	list(site_mean)
		)		
}
