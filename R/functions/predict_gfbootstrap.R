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
      env_pred_raw = list(NA),
      imp_preds = list(NA),
      sim_mat = list(NA)
    ))
  }
  env_dom <- env_domain[domain ==  gfbootstrap_combined$env_domain, data][[1]]

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

  predicted_stats <- pred_points[pred %in% imp_preds,
                               {
                                 wide_boot <- data.table::dcast(
                                                            .SD[, .(pred, y, gf)],
                                                            gf ~ pred,
                                                            value.var = "y")
                                 wide_boot[, gf := NULL]
																 wide_boot_gmm <- wide_boot
																 ## wide_boot_gmm <- as.gpu.matrix(as.data.frame(wide_boot), type = "tensorflow", dtype="float32", device = "cuda")
																 names(wide_boot_gmm) <- NULL
																 rownames(wide_boot_gmm) <- NULL
																 site_mean <- colMeans(wide_boot_gmm)
																 site_sigma <- cov(wide_boot_gmm)
																 out <- data.table::data.table(
																												site_mean = list(as.vector(site_mean)),
                                                   site_sigma = list(site_sigma),
																												site_sigma_det = determinant(site_sigma, logarithm=TRUE)$modulus
																										)
                                 },
                               by = c("x_row")]

		## Batch GPU version: One giant matrix, operate on subsets
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

		## pred_wide_gpu <- as.gpu.matrix(as.data.frame(pred_wide), type = "tensorflow", dtype = "float32", device = "cuda")

		site_mean <- do.call(rbind, purrr::map(seq.int(n_x_row), \(x, n_gf, pred_wide_gpu){
				row_index <- seq.int(from = (x-1)*n_gf + 1, length.out = n_gf)
				out <- colMeans(pred_wide_gpu[row_index, ])
		}, pred_wide_gpu = pred_wide, n_gf = n_gf))
																				#site_mean <- as.gpu.matrix(site_mean, type = "torch", dtype = "float32", device = "cuda")

		site_sigma <- do.call(rbind, purrr::map(seq.int(n_x_row), \(x, n_gf, pred_wide_gpu){
				row_index <- seq.int(from = (x-1)*n_gf + 1, length.out = n_gf)
				out <- cov(pred_wide_gpu[row_index, ])
		}, pred_wide_gpu = pred_wide, n_gf = n_gf))
		#site_sigma <- as.gpu.matrix(site_sigma, type = "torch", dtype = "float32", device = "cuda")
		row_pairs <- data.table::CJ(i = seq.int(n_x_row), j = seq.int(n_x_row))

		site_sigma_det <- do.call(c, purrr::map(seq.int(n_x_row), \(x, n_preds, site_sigma){
				## sigma_det_rows <- ncol(site_sigma_det)
				##x_sigma_det_ind <- seq.int(from = (x-1) * sigma_det_rows + 1, length.out = sigma_det_rows)
				row_index <- seq.int(from = (x-1)*n_preds + 1, length.out = n_preds)
				submat <- site_sigma[row_index, ]
				out <- determinant(submat, logarithm=TRUE)$modulus

		}, site_sigma = site_sigma, n_preds = n_preds))

		row_pairs <- data.table::CJ(i = seq.int(n_x_row), j = seq.int(n_x_row))

		dist_long <- purrr::map2_dbl(row_pairs$i, row_pairs$j,
              ~ {
                if(.x < .y) {
                    b_dist <- my_bhattacharyya_dist_gpu(
												              .x,
																			.y,
																			site_mean,
																			site_sigma,
																			site_sigma_det
										)
                  return(b_dist)
                } else {
                  return(NA)
                }
              }, predicted_stats)

  ## Bhattacharyya coefficient = exp(-Bhattacharyya distance)
  sim_mat <- matrix(exp(-dist_long), nrow(predicted_stats), nrow(predicted_stats))
  sim_mat[upper.tri(sim_mat)] <- t(sim_mat)[upper.tri(sim_mat)]
  diag(sim_mat) <- 1

		## Unset gpu.matrix in predicted_stats
		predicted_stats[ , site_sigma := lapply(site_sigma, as.matrix)]

    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(predicted_stats),
      env_pred_raw = list(predicted),
      imp_preds = list(imp_preds),
      sim_mat = list(list(sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
    ))

}

  my_bhattacharyya_dist <- function(x_mean,
                               x_sigma,
                               x_det,
                               y_mean,
                               y_sigma,
                               y_det
                               ){
    ## If the input determinants are 0,
    ## then bhattacharyya dist will be infinite
    if (exp(x_det) == 0 || exp(y_det) == 0) {
      return(Inf)
    }
    joint_mean <- x_mean-y_mean
    ## if(!is.na(thres)){
    ##     m1 <- t(joint_mean) %*% y_sigma_inv %*% joint_mean
    ##     m2 <- t(joint_mean) %*% x_sigma_inv %*% joint_mean
    ##     #print(c(m1,m2))
    ##     if(min(m1,m2) > thres){
    ##     return(Inf)
    ##     }
			## }
			## x_sigma <- as.gpu.matrix(x_sigma, type = "tensorflow", dtype="float32", device = "cuda")
			## y_sigma <- as.gpu.matrix(y_sigma, type = "tensorflow", dtype="float32", device = "cuda")
    joint_cov <- (x_sigma + y_sigma)/2
    joint_det <- determinant(joint_cov, logarithm = TRUE)$modulus
    joint_cov_inv <- tryCatch(
			ginv(joint_cov),
      ## chol2inv(chol(joint_cov)),
    error = function(e){
      return(MASS::ginv(joint_cov))
      }
    )


    #joint_mean <- x_mean-y_mean

    bhattacharyya_dist <- 0.125 * ((t(joint_mean) %*% joint_cov_inv) %*% joint_mean) +
				0.5 * log(exp(joint_det) / sqrt(exp(x_det) * exp(y_det)))
    return(as.numeric(bhattacharyya_dist))
    }


my_bhattacharyya_dist_gpu <- function(x,
																			y,
																			site_mean,
																			site_sigma,
																			site_sigma_det
																			){
																				#mem_used<-torch::cuda_memory_stats()$allocated_bytes$all$current
																				#if (mem_used > 240000000) torch::cuda_empty_cache()
		## If the input determinants are 0,
		## then bhattacharyya dist will be infinite
		if (exp(site_sigma_det[x]) == 0 || exp(site_sigma_det[y]) == 0) {
				return(Inf)
		}
		joint_mean <- site_mean[x, ] - site_mean[y, ]
		## if(!is.na(thres)){
		##     m1 <- t(joint_mean) %*% y_sigma_inv %*% joint_mean
		##     m2 <- t(joint_mean) %*% x_sigma_inv %*% joint_mean
		##     #print(c(m1,m2))
		##     if(min(m1,m2) > thres){
		##     return(Inf)
		##     }
		## }
		## x_sigma <- as.gpu.matrix(x_sigma, type = "tensorflow", dtype="float32", device = "cuda")
		## y_sigma <- as.gpu.matrix(y_sigma, type = "tensorflow", dtype="float32", device = "cuda")
		sigma_rows <- ncol(site_sigma)
		x_sigma_ind <- seq.int(from = (x-1) * sigma_rows + 1, length.out = sigma_rows)
		y_sigma_ind <- seq.int(from = (y-1) * sigma_rows + 1, length.out = sigma_rows)
		joint_cov <- (site_sigma[x_sigma_ind, ] + site_sigma[y_sigma_ind])/2
		joint_det <- determinant(joint_cov, logarithm = TRUE)$modulus
		joint_cov_inv <- tryCatch(
				ginv(joint_cov),
				## chol2inv(chol(joint_cov)),
				error = function(e){
						return(MASS::ginv(joint_cov))
				}
		)


																				#joint_mean <- x_mean-y_mean

		bhattacharyya_dist <- 0.125 * ((t(joint_mean) %*% joint_cov_inv) %*% joint_mean) +
				0.5 * log(
									exp(joint_det) /
									sqrt(exp(site_sigma_det[x]) * exp(site_sigma_det[y]))
							)

		return(as.numeric(bhattacharyya_dist))
}
