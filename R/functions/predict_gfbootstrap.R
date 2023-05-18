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
			env_id = list(NA),
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
		## r x c
		## pred_wide is (x_row*gf) x (n_preds)
		## I want to batch into (x_row) x  (npreds) x (gf)
		## to align with cov
		## matrix inner veector currently starts with a pred, goes through all gfs in an x_row, starts a new x_row.
		## so vector is (from slowest to fastest changing) (pred), (x_row), (gf)
		## array goes from fastest changing to slowest. Reverse the order for array
		pred_wide_array <-array(as.matrix(pred_wide), c(n_gf, n_x_row, n_preds))
		## torch assumes the leftmost dim is the slices, so leftmost needs to be x_row.
		## also, rows are variables (preds). Is it still row x col? Yes.
		pred_wide_batch <- aperm(pred_wide_array, c(2,3,1))

		## t -> (npreds) x (x_row *gf)
		## c -> (
		##;; pred_wide_batch <- aperm(array(t(pred_wide), c(n_preds,n_preds,n_x_row)), c(3,1,2))
		pred_wide_tensor <- torch_tensor(pred_wide_batch)
		site_mean <- torch_mean(pred_wide_tensor, 3)
		site_sigma <- torch_tensor(array(0, c(n_x_row, n_preds, n_preds)))

		for (i in seq.int(n_x_row)) {
				site_sigma[i,,] <- pred_wide_tensor[i,,]$cov()
		}

		site_sigma_det <- torch_slogdet(site_sigma)[[2]]

		singular_det_sites <- as.logical(site_sigma_det$isfinite())
		row_pairs <- data.table::CJ(i = seq.int(n_x_row)[singular_det_sites], j = seq.int(n_x_row)[singular_det_sites])
		row_pairs_filtered <- row_pairs[ i < j, ]

		joint_mean <- site_mean[row_pairs_filtered$i, ] - site_mean[row_pairs_filtered$j, ]

		joint_cov <- (site_sigma[row_pairs_filtered$i, , ] + site_sigma[row_pairs_filtered$j, ,])/2
		joint_det <- torch_slogdet(joint_cov)[[2]]
		joint_cov_inv <- torch_inverse(joint_cov)
		## joint_mean_t <- joint_mean$unsqueeze(2)
		## joint_mean$unsqueeze_(3)

		## torch_baddbmm
		## beta * param1 + alpha * (param2 x param3)
		## here param3 is cov * mean.colvec
		## and param2 is mean.rowvec
		bhattacharyya_dist <- torch_baddbmm(
		(joint_det - 0.5 * (site_sigma_det[row_pairs_filtered$i] + site_sigma_det[row_pairs_filtered$j]))$unsqueeze_(-1)$unsqueeze_(-1),
		joint_mean$unsqueeze(2), ## Don't modify inline, tends to mess up evaluate here,
		torch_bmm(joint_cov_inv, joint_mean$unsqueeze(3)),
		beta = 0.5,
		alpha = 0.125)$squeeze_()$neg_()$exp_()

		sim_mat <- torch_sparse_coo_tensor(t(as.matrix(row_pairs_filtered)), bhattacharyya_dist, c(n_x_row, n_x_row))$to_dense() 

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
		predicted_stats <- list(site_mean = as.matrix(site_mean),
														site_sigma = array(site_sigma),
														site_sigma_det = as.numeric(site_sigma_det))
    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(predicted_stats),
      env_pred_raw = list(predicted),
			env_id = list(env_dom[,..env_id_col]),
      imp_preds = list(imp_preds),
      sim_mat = list(list(sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
    ))

}
