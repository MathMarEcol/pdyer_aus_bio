# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
assign_new_sites_to_cluster <- function(
                                        cluster_env_extrapolate,
                                        gfbootstrap_cluster,
                                        env_biooracle_names,
                                        env_domain,
                                        env_id_col,
                                        spatial_vars
                                        ) {



    stopifnot(
      all(
        cluster_env_extrapolate[, .(env_domain, res_gf, res_clust, trophic, survey, depth_cat)]==
          gfbootstrap_cluster[, .(env_domain,  res_gf, res_clust, trophic, survey, depth_cat)]
      ) && all(
        cluster_env_extrapolate[, .(env_year, env_pathway)] ==
          env_biooracle_names[, .(env_year, env_pathway)]
      )
    )



    if (all(is.na(cluster_env_extrapolate$extrap_sims))) {
        ## Upstream target decided survey was not usable.
        ## Propagating
        ##
        return(data.table(cluster_env_extrapolate[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                          clust_method = gfbootstrap_cluster$clust_method,
                          clust_ind = list(NA),
                          pred_membership = list(NA),
                          pred_prob = list(NA)
                          ))
    }

    ## Expecting a sim_mat, clustering over that sim_mat, and a pred_sim_mat
    ## Here, we find the cluster that is most similar to each pred_sim_mat
		clust_methods <- gfbootstrap_cluster$clust_method
    out <- switch(clust_methods,
                  "casterdefault" =,
                  "casteroptimal" =,
                  "casternonzero" = ,
                  "casterquantile25" = ,
                  "casterquantile50" = ,
                  "casterquantile75" = ,
                  "casterquantile90" = ,
                  "casterquantile95" = ,
                  "casterohfive" = ,
                  "casterohfivebonferroni" = {
                      assign_new_sites_to_cluster_caster(
                          cluster_env_extrapolate,
                          gfbootstrap_cluster,
                          env_domain,
                          env_id_col,
                          spatial_vars
                      )
                  },
                  "apclustdefaultmedian" =,
                  "apclustdefaultmin" =,
                  "apclustoptimal" = {
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
