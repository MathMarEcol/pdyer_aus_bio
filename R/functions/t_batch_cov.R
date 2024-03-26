# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
#' batch covariance matrix for tensors
#' see 
#' https://stackoverflow.com/questions/71357619/how-do-i-compute-batched-sample-covariance-in-pytorch
t_batch_cov <- function(pred_wide_tensor_batch,
												row_batch,
												n_gf,
												n_preds) {
				
		tmp_mean <- pred_wide_tensor_batch$mean(3)$unsqueeze(3)
		tmp_diff <- (pred_wide_tensor_batch - tmp_mean)$reshape(list(length(row_batch) * n_gf, n_preds))
		tmp_prods <- torch_bmm(tmp_diff$unsqueeze(3), tmp_diff$unsqueeze(2))$reshape(list(length(row_batch), n_gf, n_preds, n_preds))
		batched_cov <- tmp_prods$sum(2) / (n_gf -1)
}
