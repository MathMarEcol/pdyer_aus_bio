extrapolate_to_env <- function(
                               gfbootstrap_combined,
                               gfbootstrap_predicted,
                               gfbootstrap_cluster,
                               env_domain_plot,
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
        return(data.table(gfbootstrap_cluster[, .(env_domain, trophic, survey, depth_cat, clust_method)],
													env_pred_stats = list(NA),
													env_id = list(NA),
													imp_preds = list(NA),
													pred_sim_mat = list(NA) ##double wrap the sim mat so data.table doesn't try to print it
                          ))
    }
    
    ## Make sure env domain is ready

    env_dom <- env_domain_plot[domain ==  gfbootstrap_combined$env_domain, data][[1]]

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

		imp_preds <- gfbootstrap_combined$imp_preds[[1]]
		
		## Batch GPU version: One giant matrix, operate on subsets
		## See ./predict_gfbootstrap.R for code comments
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
		rm(predicted)
		rm(pred_points)
		pred_wide_array <-array(as.matrix(pred_wide), c(n_gf, n_x_row, n_preds))
		rm(pred_wide)
		pred_wide_batch <- aperm(pred_wide_array, c(2,3,1))
		rm(pred_wide_array)
		pred_wide_tensor <- torch_tensor(pred_wide_batch, dtype = torch_float32())
		rm(pred_wide_batch)
		gc()
		site_mean <- torch_mean(pred_wide_tensor, 3)
		site_sigma <- torch_tensor(array(0, c(n_x_row, n_preds, n_preds)), dtype = torch_float32())

		for (i in seq.int(n_x_row)) {
				site_sigma[i,,] <- pred_wide_tensor[i,,]$cov()
		}
		rm(pred_wide_tensor)
		gc()
		site_sigma_det <- torch_slogdet(site_sigma)[[2]]
		
		nonsingular_det_sites <- as.logical(site_sigma_det$isfinite())
		
    site_pairs <- data.table::CJ(cluster = seq.int(nrow(gfbootstrap_predicted$env_id[[1]])),
                                 new = seq.int(n_x_row)[nonsingular_det_sites])

		mem_per_pair <- 4 * (6 + 4 * n_preds + 2 * n_preds^2)
		if (is.na(mem_max <- as.numeric(Sys.getenv("TENSOR_MEM_MAX", "")))) {
				n_row_batch <- floor(mem_max / mem_per_pair)
		} else {
				n_row_batch <- floor(mem_max / mem_per_pair)
		}
		n_batches <- ceiling(nrow(site_pairs) / n_row_batch)
		
		site_pairs[ , batch_ind := rep(seq.int(n_batches), each = n_row_batch, length.out = nrow(site_pairs))]

		cluster_site_mean <- torch_tensor(
				gfbootstrap_combined$env_pred_stats$site_mean[[1]],
				dtype = torch_float32())
		cluster_site_sigma <- torch_tensor(
				gfbootstrap_combined$env_pred_stats$site_sigma[[1]],
				dtype = torch_float32())
		cluster_site_sigma_det <- torch_tensor(
				gfbootstrap_combined$env_pred_stats$site_sigma_det[[1]],
				dtype = torch_float32())
		
		site_pairs[ ,
											 bhatt_dist :=	as.numeric(bhattacharyya_dist_tensor(
													 .SD[ , .(cluster, new)],
													 cluster_site_mean,
													 cluster_site_sigma,
													 cluster_site_sigma_det,
													 site_mean,
													 site_sigma,
													 site_sigma_det)),
											 by = batch_ind]
		site_pairs[ , batch_ind := NULL]
		
		sim_mat <- matrix(site_pairs$bhatt_dist,
											nrow(gfbootstrap_predicted$env_id[[1]]),
											length(nonsingular_det_sites))

		## Unset gpu.matrix in predicted_stats
		predicted_stats <- list(site_mean = as.matrix(site_mean),
														site_sigma = array(as.numeric(site_sigma), dim(site_sigma)),
														site_sigma_det = as.numeric(site_sigma_det))
		
    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
                                  env_pred_stats = list(predicted_stats),
                                  env_id = list(env_dom[,..env_id_col]),
                                  imp_preds = list(imp_preds),
                                  pred_sim_mat = list(list(sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
                                  ))
    
    
    


}
