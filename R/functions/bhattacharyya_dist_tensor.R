# Compiled torchscript
compiled <- torch::jit_compile("
#def bhatt_single (site_mean_x,
#											site_sigma_x,
#											site_sigma_det_x,
#											site_mean_y,
#											site_sigma_y,
#											site_sigma_det_y) :
#  joint_cov =site_sigma_x.add_(
#     site_sigma_y).mul_(0.5)
#  joint_det = torch.logdet(joint_cov)
#  ## worth benchmarking chol here
#  # joint_cov_inv = torch.cholesky_inverse(
#  #   torch.linalg.cholesky(joint_cov)
#  #)
#  joint_cov_inv = torch.inverse(joint_cov)
#  joint_mean = torch.sub(
#    site_mean_x,
#    site_mean_y
#  )
#  return torch.sub(joint_det,
#      torch.add(
#        site_sigma_det_x,
#        site_sigma_det_y
#      ),
#      alpha = 0.5
#    ).mul_(0.5).add(
#      torch.mm(joint_mean.unsqueeze(0),
#      torch.mm(joint_cov_inv, joint_mean.unsqueeze(1))),
#      alpha = 0.125).neg_().exp_()
#def bhatt_para (site_mean_x,
#											site_sigma_x,
#											site_sigma_det_x,
#											site_mean_y,
#											site_sigma_y,
#											site_sigma_det_y,
#                      rows_x,
#                      rows_y) :
#  futures : List[torch.jit.Future[torch.Tensor]] = []
#  for x,y in zip(rows_x, rows_y):
#    futures.append(torch.jit.fork(
#      bhatt_single,
#      site_mean_x[x],
#      site_sigma_x[x],
#      site_sigma_det_x[x],
#      site_mean_y[y],
#      site_sigma_y[y],
#      site_sigma_det_y[y]
#    ))
#  results = []
#  for future in futures:
#    results.append(torch.jit.wait(future))
#  return torch.stack(results)
def bhatt_coeff_full (site_mean_x,
											site_sigma_x,
											site_sigma_det_x,
											site_mean_y,
											site_sigma_y,
											site_sigma_det_y,
                      rows_x,
                      rows_y) :
  joint_cov = torch.index_select(site_sigma_x, 0, rows_x).add_(
     torch.index_select(site_sigma_y, 0, rows_y)).mul_(0.5)
  joint_det = torch.logdet(joint_cov)
  ## worth benchmarking chol here
  # joint_cov_inv = torch.cholesky_inverse(
  #   torch.linalg.cholesky(joint_cov)
  #)
  joint_cov_inv = torch.inverse(joint_cov)
  joint_mean = torch.sub(
    torch.index_select(site_mean_x, 0, rows_x),
    torch.index_select(site_mean_y, 0, rows_y)
  )
  return torch.baddbmm(
    torch.sub(joint_det,
      torch.add(
        torch.index_select(site_sigma_det_x, 0, rows_x),
        torch.index_select(site_sigma_det_y, 0, rows_y)
      ),
      alpha = 0.5
    ).unsqueeze_(-1).unsqueeze_(-1),
    joint_mean.unsqueeze(1),
    torch.bmm(joint_cov_inv, joint_mean.unsqueeze(2)),
    beta = 0.5,
    alpha = 0.125).squeeze_().neg_().exp_()
def bhatt_coeff (joint_det,
                joint_mean,
                joint_cov_inv,
                site_sigma_det_x,
                site_sigma_det_y,
                rows_x,
                rows_y) :
  det_diff = torch.add(
      torch.index_select(site_sigma_det_x, 0, rows_x),
      torch.index_select(site_sigma_det_y, 0, rows_y)
    )
  bhatt_det = torch.sub(joint_det, det_diff, alpha = 0.5).unsqueeze_(-1).unsqueeze_(-1)
  joint_mean_row = torch.unsqueeze(joint_mean, 1)
  joint_mean_col = torch.unsqueeze(joint_mean, 2)
  pre_mahalanobis = torch.bmm(joint_cov_inv, joint_mean_col)
  bhatt_c = torch.baddbmm(
    bhatt_det,
    joint_mean_row,
    pre_mahalanobis,
    beta = 0.5,
    alpha = 0.125).squeeze_().neg_().exp_()
  return bhatt_c
def test (joint_det,
                joint_mean,
                joint_cov_inv,
                site_sigma_det_x,
                site_sigma_det_y,
                rows_x,
                rows_y) :
  det_diff = torch.add(
      torch.index_select(site_sigma_det_x, 0, rows_x),
      torch.index_select(site_sigma_det_y, 0, rows_y)
    )
  bhatt_det = torch.sub(joint_det, det_diff, alpha = 0.5).unsqueeze_(-1).unsqueeze_(-1)
  joint_mean_row = torch.unsqueeze(joint_mean, 1)
  joint_mean_col = torch.unsqueeze(joint_mean, 2)
  pre_mahalanobis = torch.bmm(joint_cov_inv, joint_mean_col)
  bhatt_coeff = torch.baddbmm(
    bhatt_det,
    joint_mean_row,
    pre_mahalanobis,
    beta = 0.5,
    alpha = 0.125).squeeze_().neg_().exp_()
  return bhatt_coeff
")

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
bhattacharyya_dist_tensor_compiled <- function(row_pairs,
                                               local_device,
																			site_mean_x,
																			site_sigma_x,
																			site_sigma_det_x,
																			site_mean_y,
																			site_sigma_y,
																			site_sigma_det_y
																			) {
    ## Compiled torchscript is offset indexed (0-indexed)
    ## R code is count indexed (1-indexed)
		rows_x <- torch::torch_tensor(row_pairs[[1]]-1,
                                  device = local_device,
                                  dtype = torch::torch_int())
		rows_y <- torch::torch_tensor(row_pairs[[2]]-1,
                                  device = local_device,
                                  dtype = torch::torch_int())
    bhatt_coeff <- compiled$bhatt_coeff_full(
      site_mean_x,
      site_sigma_x,
      site_sigma_det_x,
      site_mean_y,
      site_sigma_y,
      site_sigma_det_y,
      rows_x,
      rows_y
    )

		## On CPU, which may use swap, manually GC
		## to stay within max memory usage.
		## GPU has no swap, so automatic GC will keep
		## memory within limits. Also, GPU will crash if asked to
		## use too much memory.
		if (bhatt_coeff$device == torch_device("cpu")) {
				gc()
		}
		return(bhatt_coeff)
}



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
  joint_det <- joint_cov$logdet()
		## Peak memory usage follows formula:
		## nrow(row_pairs) * size_dtype * (
		##  5 ^ n_preds ^ 2 +
		## 3 * n_preds +
		## 12)
		## size_dtype will be 4 for float32 with torch
		## or 8 for float64.
		## Ignores overhead of inputs and any variables
		## not passed into function.
		## for matrices up to 30x30, standard inverse is faster
		## joint_cov_inv <- joint_cov$cholesky()$cholesky_inverse()
		joint_cov_inv <- joint_cov$inverse()
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
