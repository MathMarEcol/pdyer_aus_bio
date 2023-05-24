## Tensor batch calculation of the
## bhattacharyya coefficient
## @param row_pairs is an n x 2 matrix or equivalent
## where each row is the indicies of the sites being
## compared
## @param site_mean, site_sigma and site_sigma_det
## are arrays, where the first dim is the index of the
## site. site_mean is a vector per site, dim(site, pred).
## site_sigma is a matrix per site, dim(site, pred, pred).
## site_sigma_det is a numeric per site, dim(site).
## Code probably runs fastest if they are torch tensors.
bhattacharyya_dist_tensor <- function(row_pairs,
																			site_mean_x,
																			site_sigma_x,
																			site_sigma_det_x,
																			site_mean_y,
																			site_sigma_y,
																			site_sigma_det_y
																			) {
		## Size = (sites^2 - sites)/2 * preds ^2 * float32 = 64GB
		rows_x <- row_pairs[[1]]
		rows_y <- row_pairs[[2]]
		joint_cov <- (site_sigma_x[rows_x, , ] + site_sigma_y[rows_y, ,])/2
		## Size = (sites^2 - sites)/2 * float32 = 70Mb
		joint_det <- torch_slogdet(joint_cov)[[2]]
		## Size same as joint_cov, so also 64GB
		## Peak here is 2 * joint_cov, order of magnitude larger than anything else
		joint_cov_inv <- torch_inverse(joint_cov)

		## Allow R to reclaim memory if needed
		rm(joint_cov)
		## Size = (sites^2 - sites)/2 *preds * float32 = 2.1GB
		joint_mean <- site_mean_x[rows_x, ] - site_mean_y[rows_y, ]

		## joint_mean_t <- joint_mean$unsqueeze(2)
		## joint_mean$unsqueeze_(3)

		## torch_baddbmm
		## beta * param1 + alpha * (param2 x param3)
		## here param3 is cov * mean.colvec
		## and param2 is mean.rowvec
		## Peak size to calculate
		## joint_cov_inv + joint_mean + <joint_cov_inv*joint_mean>(same as joint_mean) + 2*joint_mean$unsqueeze (same as joint_mean) + joint_det * 5 (all possible intermediates with joint_det and site_sigma_det)
		## joint_det * 6 (all possible intermediates with joint_det and site_sigma_det, also output size) + joint_mean * 4 (input, unsqueeze twice, and bmm with joint_cov_inv has same number of elements) + joint_cov_inv 
		## 0.07GB * 6 + 2.1GB * 4 + 64GB = 74GB
		## let sites_hs = (sites^2 - sites)/2
		## sites_hs * float32 * 6 + sites_hs * preds * float32 * 4 + sites_hs * preds^2 * float32
		## sites_hs * float32 (6 + 4 * preds + preds^2)
		## when preds is 30, this is sites_hs * 4104
		## Sites_hs is a line of pairs, so it can be broken up and calculated in stages without problem
		## Conservative peak max occurs when inverting joint_cov, two very large
		## tensors exist at once, with no in-place variant available.
		## Assume max memory is
		## sites_hs * float32 (6 + 4 * preds + 2 * preds^2)
		## 7704 * sites_hs for 30 preds.
		bhattacharyya_dist <- torch_baddbmm(
		  (joint_det - 0.5 * (site_sigma_det_x[rows_x] + site_sigma_det_y[rows_y]))$unsqueeze_(-1)$unsqueeze_(-1),
			joint_mean$unsqueeze(2), ## Don't modify inline, tends to mess up evaluate here,
			torch_bmm(joint_cov_inv, joint_mean$unsqueeze(3)),
			beta = 0.5,
			alpha = 0.125)$squeeze_()$neg_()$exp_()

		return(bhattacharyya_dist$to(device = "cpu"))
}
