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

  if(all(is.na(gfbootstrap_predicted$sim_mat))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
      return(data.table(gfbootstrap_predicted[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                        clust_method = clust_methods,
      clust = list(NA),
      best_clust =  NA,
      best_clust_ob = list(NA),
      clust_ind = list(NA)
    ))
  }

    out <- switch(clust_methods,
           "casteroptimal" = {
               cluster_gfbootstrap_casteroptimal(
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
           "casterdefault" = {
               cluster_gfbootstrap_casterdefault(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars
               )
           },
           "apclustdefaultmedian" = {
               cluster_gfbootstrap_apclustdefault(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   q = 0.5
               )
           },
           "apclustdefaultmin" = {
               cluster_gfbootstrap_apclustdefault(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   q = 0
               )
           },
           "apclustoptimal" = {
               cluster_gfbootstrap_apclustoptimal(
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
