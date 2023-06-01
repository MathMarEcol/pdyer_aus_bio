assign_new_sites_to_cluster_caster <- function(
                                               cluster_env_extrapolate,
                                               gfbootstrap_cluster,
                                               env_domain,
                                               env_id_col,
                                               spatial_vars
                                               ) {

    ## predict(cast_ob_with_sim_mat, pred_sim_mat)

		## Batch over list in cluster_env_extrapolate
		row_clust_membership <- cluster_env_extrapolate$extrap_sims[[1]][ ,
														caster_extrap_membership(
																.SD$site_pairs[[1]],
																gfbootstrap_cluster,
																cluster_env_extrapolate$env_id[[1]][[env_id_col]],
																env_id_col),
														by = batch_ind]
    ## Have membership for each x_row
    ## Need to create the clust_ind table for the polygons function

		clust_ind <- rbindlist(row_clust_membership$max)
		
    clust_ind[env_domain[domain == gfbootstrap_cluster$env_domain[[1]], data][[1]], on = c(env_id_col),
              c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

    return(data.table(gfbootstrap_cluster[, .(env_domain, trophic, survey, depth_cat, clust_method)],
                      clust_ind = list(clust_ind),
                      pred_membership = list(row_clust_membership),
                      ))
}


caster_extrap_membership <- function(long_sim_mat,
																		 gfbootstrap_cluster,
																		 env_ids,
																		 env_id_col){

		n_cluster <- max(long_sim_mat$cluster)
		n_new <- nrow(long_sim_mat)/n_cluster
		pred_sim_mat <- matrix(long_sim_mat$bhatt_vec,
													 ncol = max(long_sim_mat$cluster))
    ## Temporary, use local predict_clust for debugging
    predicted_cluster_membership <- predict_clust(
				gfbootstrap_cluster$best_clust_ob[[1]],
				pred_sim_mat,
				aff_thres = gfbootstrap_cluster$clust[[1]]$aff_thres[
																											gfbootstrap_cluster$best_clust],
				type = "max")
    predicted_cluster_probability <- predict_clust(
				gfbootstrap_cluster$best_clust_ob[[1]],
				pred_sim_mat,
				type = "probability")
		
		max_dt <- data.table(cl = predicted_cluster_membership, cl_factor = as.factor(predicted_cluster_membership))
		max_dt[, c(env_id_col) := env_ids]
 		
		return(list(max = list(max_dt), prob_cl = list(predict_cluster_probability)))
}
