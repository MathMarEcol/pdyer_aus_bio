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
               sim_mat <- gfbootstrap_predicted$sim_mat[[1]][[1]]
               cluster_gfbootstrap_casterfixed(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   aff_thres = mean(sim_mat[upper.tri(sim_mat)])
               )
           },
           "casterohfive" = {
               cluster_gfbootstrap_casterfixed(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   aff_thres = 0.05
               )
           },
           "casterohfivebonferroni" = {
               sim_mat_nrow <- nrow(gfbootstrap_predicted$sim_mat[[1]][[1]])
               cluster_gfbootstrap_casterfixed(
                 clust_methods,
                 gfbootstrap_predicted,
                 env_domain,
                 env_id_col,
                 spatial_vars,
                 aff_thres = 0.05 / ((sim_mat_nrow * (sim_mat_nrow - 1)) / 2)
               )
           },
           "casternonzero" = {
               sim_mat <- gfbootstrap_predicted$sim_mat[[1]][[1]]
               sim_mat_nz <- sim_mat > 0
               cluster_gfbootstrap_casterfixed(
                   clust_methods,
                   gfbootstrap_predicted,
                   env_domain,
                   env_id_col,
                   spatial_vars,
                   aff_thres = min(sim_mat[sim_mat_nz])
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
