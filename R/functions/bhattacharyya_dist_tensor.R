## Tensor batch calculation of the
## bhattacharyya coefficient
## @param row_pairs is an n x 2 matrix or equivalent
## where each row is the indicies of the sites being
## compared
## @param site_mean, site_sigma and site_sigma_det are torch
## tensor arrays, where the first dim is the index of the
## site. site_mean is a vector per site, dim(site, pred).
## site_sigma is a matrix per site, dim(site, pred, pred).
## site_sigma_det is a numeric per site, dim(site).
bhattacharyya_dist_tensor <- function(row_pairs,
																			site_mean_x,
																			site_sigma_x,
																			site_sigma_det_x,
																			site_mean_y,
																			site_sigma_y,
																			site_sigma_det_y
																			) {
		rows_x <- row_pairs[[1]]
		rows_y <- row_pairs[[2]]
		joint_cov <- site_sigma_x[rows_x, , ]$add_(site_sigma_y[rows_y, , ])$mul_(0.5)
		joint_det <- torch_slogdet(joint_cov)[[2]]
		## Peak memory usage follows formula:
		## nrow(row_pairs) * size_dtype * (
		##  5 ^ n_preds ^ 2 +
		## 3 * n_preds +
		## 12)
		## size_dtype will be 4 for float32 with torch
		## or 8 for float64.
		## Ignores overhead of inputs and any variables
		## not passed into function.
		joint_cov_inv <- joint_cov$cholesky()
		rm(joint_cov)
		joint_cov_inv <- joint_cov_inv$cholesky_inverse()

		## Allow R to reclaim memory if needed
		joint_mean <- site_mean_x[rows_x, ] - site_mean_y[rows_y, ]

		bhattacharyya_dist <- torch_baddbmm(
		  (joint_det - 0.5 * (site_sigma_det_x[rows_x] + site_sigma_det_y[rows_y]))$unsqueeze_(-1)$unsqueeze_(-1),
			## Don't modify inline, tends to mess up
			## evaluate here, and unsqueeze just creates
			## a view, not a copy
			joint_mean$unsqueeze(2),
			torch_bmm(joint_cov_inv, joint_mean$unsqueeze(3)),
			beta = 0.5,
			alpha = 0.125)$squeeze_()$neg_()$exp_()

		## On CPU, which may use swap, manually GC
		## to stay within max memory usage.
		## GPU has no swap, so automatic GC will keep
		## memory within limits. Also, GPU will crash if asked to
		## use too much memory.
		if (bhattacharyya_dist$device == torch_device("cpu")) {
				gc()
		}
		return(bhattacharyya_dist)
}
