cluster_gfbootstrap_apclustdefault <- function(
                                        clust_methods,
                                        gfbootstrap_predicted,
                                env_domain,
                                env_id_col,
                                spatial_vars
                                ) {

    ## q = 0.5 means select the median similarity.
    ## Recommended default that tends towards more clusters.
    apc <- apcluster(s = gfbootstrap_predicted$sim_mat[[1]][[1]], q = 0.5)
    ##May not return a well formed cluster
    ## if apclust does not converge.
    if(length(apc@clusters) < 2) {
                                        #no valid clustering found
        return(data.table(gfbootstrap_predicted[, .(env_domain, trophic, survey, depth_cat)],
                          clust_method = clust_methods,
                          clust = list(NA),
                          best_clust = NA,
                          best_clust_ob = list(NA),
                          clust_ind = list(NA)
                    ))
    } else {
        mem_mat <- castcluster::membership_mat(apc@clusters)
        h <- castcluster::hubert_gamma(gfbootstrap_predicted$sim_mat[[1]][[1]],
                                       mem_mat, norm_z = TRUE)
        out <- data.table::data.table(
          pref = apc@p, gamma = h,
          k = length(apc@clusters),
          apc_ob = I(list(apc))
        )
    }

  best_clust_ob <- apc@clusters
  clust_ind <- castcluster::cast_obj_to_df(best_clust_ob)
    names(clust_ind)[names(clust_ind) == "elem"] <- "x_row"
    names(clust_ind)[names(clust_ind) == "clust"] <- "cl"
    data.table::setDT(clust_ind)
      data.table::setkey(clust_ind, "x_row")
    clust_ind <-  cbind(clust_ind, gfbootstrap_predicted$env_id[[1]][, ..env_id_col])
  clust_ind[env_domain[domain == gfbootstrap_predicted$env_domain[[1]], data][[1]], on = c(env_id_col),
            c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

    clust_ind[, cl_factor := as.factor(cl)]

  return(data.table(gfbootstrap_predicted[, .(env_domain, trophic, survey, depth_cat)],
                    clust_method = clust_methods,
                    clust = list(out),
                    best_clust = 1,
                    best_clust_ob = I(list(best_clust_ob)),
                    clust_ind = list(clust_ind)
                    ))
}
