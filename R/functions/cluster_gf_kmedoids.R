# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
cluster_gf_kmedoids <- function(gf_predicted,
                                cluster_fixed_k,
                                clara_samples,
                                clara_sampsize,
                                clara_trace,
                                clara_rngR,
                                clara_pamLike,
                                clara_correct.d) {

  if (all(is.na(gf_predicted$imp_preds))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gf_predicted[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                      clust_method = "kmedoids",
                      clust_ind = list(NA),
                      gf_fixed_clust = list(NA)
                      ))
  }

  ## Clustering according to multiple  methods
  ## Fixed K
  ## NbClust

  ## Should have data to cluster in gf_predicted$comp_turnover

  gf_fixed_clusts <- cluster::clara(
    gf_predicted$comp_turnover[[1]][,gf_predicted$imp_preds[[1]], with = FALSE],
    cluster_fixed_k,
    metric = "manhattan",
    samples = clara_samples,
    sampsize = clara_sampsize,
    trace = clara_trace,
    rngR = clara_rngR,
    pamLike = clara_pamLike,
    correct.d = clara_correct.d
    )


  clust_ind <- data.table(
    gf_predicted$comp_turnover[[1]][, c(spatial_vars, env_id_col), with = FALSE],
    cl = gf_fixed_clusts$clustering,
    x_row = seq.int(1, nrow(gf_predicted$comp_turnover[[1]]))
    )
  clust_ind[, cl_factor := as.factor(cl)]

    data.table::setkey(clust_ind, "x_row")

  return(data.table(gf_predicted[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                    clust_method = "kmedoids",
                    clust_ind = list(clust_ind),
                    gf_fixed_clust = list(gf_fixed_clusts)
                    ))


}
