cluster_gf_kmedoids <- function(gf_predicted,
                                env_domain,
                                env_biooracle_names,
                                cluster_fixed_k,
                                k_range,
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
    return(data.table(gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                      clust_method = "kmedoids",
                      clust_ind = list(NA),
                      gf_fixed_clust = list(NA),
                      gf_nbclust = list(NA)
                      ))
  }

  ## Clustering according to multiple  methods
  ## Fixed K
  ## NbClust

  ## Should have data to cluster in gf_predicted$comp_turnover

  gf_fixed_clusts <- cluster::clara(
    gf_predicted$comp_turnover[[1]][,env_biooracle_names, with = FALSE],
    cluster_fixed_k,
    metric = "manhattan",
    samples = clara_samples,
    sampsize = clara_sampsize,
    trace = clara_trace,
    rngR = clara_rngR,
    pamLike = clara_pamLike,
    correct.d = clara_correct.d
    )

  ## Slow
  gf_nbclust <- NbClust::NbClust(
    gf_predicted$comp_turnover[[1]][,env_biooracle_names, with = FALSE],
    distance = "manhattan",
    min.nc = min(k_range),
    max.nc = max(k_range),
    method = "kmeans",
    index = "alllong"
  )


  clust_ind_fixed <- data.table(
    gf_predicted$comp_turnover[[1]][, c(spatial_vars, env_id_col), with = FALSE],
    cl = gf_fixed_clusts$clustering,
    x_row = seq.int(1, nrow(gf_predicted$comp_turnover[[1]]))
    )
  clust_ind_fixed[, cl_factor := as.factor(cl)]

    data.table::setkey(clust_ind_fixed, "x_row")

  return(data.table(gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                    clust_method = "kmedoids",
                    clust_ind_fixed = list(
                      data.table(
                        clust_ind = list(clust_ind_fixed),
                        gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                        clust_method = "kmedoids"
                        )
                    ),
                    gf_fixed_clust = list(gf_fixed_clusts),
                    gf_nbclust = list(gf_nbclust)
                    ))


}
