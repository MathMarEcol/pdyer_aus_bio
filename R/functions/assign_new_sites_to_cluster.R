assign_new_sites_to_cluster <- function(
                                        cluster_env_extrapolate,
                                        gfbootstrap_cluster,
                                        env_domain,
                                        env_id_col,
                                        spatial_vars
                                        ) {

    if (any(is.na(cluster_env_extrapolate$pred_sim_mat[[1]]))) {
        ## Upstream target decided survey was not usable.
        ## Propagating
        ##
        return(data.table(cluster_env_extrapolate[, .(env_domain, trophic, survey, depth_cat, clust_method)],
                          clust_ind = list(NA),
                          pred_membership = list(NA),
                          pred_prob = list(NA)
                          ))
    }

    ## Expecting a sim_mat, clustering over that sim_mat, and a pred_sim_mat
    ## Here, we find the cluster that is most similar to each pred_sim_mat

    out <- switch(clust_methods,
                  "caster" = {
                      assign_new_sites_to_cluster_caster(
                          cluster_env_extrapolate,
                          gfbootstrap_cluster,
                          env_domain,
                          env_id_col,
                          spatial_vars
                      )
                  },
                  "apclust" = {
                      assign_new_sites_to_cluster_apclust(
                          cluster_env_extrapolate,
                          gfbootstrap_cluster,
                          env_domain,
                          env_id_col,
                          spatial_vars
                      )
                  },
                  {
                      stop("Unknown clustering method")
                  })
    
    


}
