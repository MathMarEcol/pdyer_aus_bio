assign_new_sites_to_cluster_caster <- function(
                                               cluster_env_extrapolate,
                                               gfbootstrap_cluster,
                                               env_domain,
                                               env_id_col,
                                               spatial_vars
                                               ) {

    ## predict(cast_ob_with_sim_mat, pred_sim_mat)

    predicted_cluster_membership <- castcluster::predict_clust(gfbootstrap_cluster$best_clust_ob[[1]],
                                                               cluster_env_extrapolate$pred_sim_mat[[1]], type = "max")
    predict_cluster_probability <- castcluster::predict_clust(gfbootstrap_cluster$best_clust_ob[[1]],
                                                             cluster_env_extrapolate$pred_sim_mat[[1]], type = "probability") 

    predicted_cluster_membership$clust[predicted_cluster_membership$aff < gfbootstrap_cluster$clust[[1]]$aff_thres[[gfbootstrap_cluster$best_clust]]] <- NA

    ## Have membership for each x_row
    ## Need to create the clust_ind table for the polygons function
    clust_ind <- cbind(predicted_cluster_membership, cluster_env_extrapolate$env_pred_stats[[1]][, ..env_id_col])
    clust_ind[env_domain[domain == gfbootstrap_cluster$env_domain[[1]], data][[1]], on = c(env_id_col),
              c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

    clust_ind[, cl_factor := as.factor(cl)]

    return(data.table(gfbootstrap_cluster[, .(env_domain, trophic, survey, depth_cat, clust_method)],
                      clust_ind = list(clust_ind),
                      pred_membership = list(predicted_cluster_membership),
                      pred_prob = list(predict_cluster_probability)
                      )
}
