cluster_gfbootstrap <- function(
                                clust_methods,
                                gfbootstrap_predicted,
                                env_domain,
                                env_id_col,
                                spatial_vars,
                                m,
                                min_range,
                                min_tol,
                                keep_all_clusts
                                ){

  if(any(is.na(gfbootstrap_predicted$sim_mat[[1]]))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
      return(data.table(gfbootstrap_predicted[, .(env_domain, trophic, survey, depth_cat)],
                        clust_method = clust_methods,
      clust = list(NA),
      best_clust =  NA,
      best_clust_ob = list(NA),
      clust_ind = list(NA)
    ))
  }

    out <- switch(clust_methods,
           "caster" = {
               cluster_gfbootstrap_caster(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   m,
                   min_range,
                   min_tol,
                   keep_all_clusts
               )
           },
           "apclust" = {
               cluster_gfbootstrap_apclust(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   m,
                   min_range,
                   min_tol,
                   keep_all_clusts
                   )
           },
           {
               stop("Unknown clustering method")
           })

    }
