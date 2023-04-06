cluster_gfbootstrap <- function(
                                clust_methods,
                                gfbootstrap_predicted,
                                env_domain,
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
      return(data.table(gfbootstrap_predicted[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid,  env_pred_stats, env_pred_raw, imp_preds, sim_mat)],
                        clust_method = clust_methods,
      clust = list(NA),
      best_clust =  NA,
      clust_ind = list(NA)
    ))
  }

    out <- switch(clust_methods,
           "caster" = {
               cluster_gfbootstrap_caster(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
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
