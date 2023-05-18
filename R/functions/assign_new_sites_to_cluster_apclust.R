assign_new_sites_to_cluster_apclust <- function(
                                                cluster_env_extrapolate,
                                                gfbootstrap_cluster,
                                                env_domain,
                                                env_id_col,
                                                spatial_vars
                                                ) {
    ## Apcluster does not seem to have a function for "predict".
    ## However, an apcluster object is a set of exemplars
    ## Just need to find the nearest exemplar.
    exemplars <- gfbootstrap_cluster$best_clust_ob@exemplars
    
    predicted_cluster_raw <- cluster_env_extrapolate$pred_mat[[1]][ , exemplars]
    predicted_cluster_prob <- predicted_cluster_raw / rowSums(predicted_cluster_raw)
    predicted_cluster_prob <- data.table::data.table(
                        x_row =
                            rep(seq_along(nrow(predicted_cluster_raw)), each = length(exemplars), ),
                        clust = rep(seq_len(exemplars), times = nrow(predicted_cluster_raw)),
                        aff = as.vector(predicted_cluster_prob))
    
    predicted_cluster_membership <- data.table::rbindlist(
                                                     lapply(nrow(predicted_cluster_raw),
           function(r, predicted_cluster_raw){
               xr <- predicted_cluster_raw[r, ]
               cl <- which.max(xr)
               return(data.table::data.table(x_row = r, clust = cl, aff = predicted_cluster_raw[r, cl]))
           }, predicted_cluster_raw = predicted_cluster_raw))

    
    ## Have membership for each x_row
    ## Need to create the clust_ind table for the polygons function
    clust_ind <-  cbind(predicted_cluster_membership, cluster_env_extrapolate$env_id[[1]][, ..env_id_col])
    clust_ind[env_domain[domain == gfbootstrap_cluster$env_domain[[1]], data][[1]], on = c(env_id_col),
              c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

    clust_ind[, cl_factor := as.factor(cl)]
    
    return(data.table(gfbootstrap_cluster[, .(env_domain, trophic, survey, depth_cat, clust_method)],
                      clust_ind = list(clust_ind),
                      pred_membership = list(predicted_cluster_membership),
                      pred_prob = list(predicted_cluster_prob)
                      ))
}


