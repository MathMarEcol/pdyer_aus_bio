cluster_gf_kmedoids <- function(gf_predicted,
                                env_domain,
                                cluster_fixed_k,
                                k_range,
                                clara_samples,
                                clara_sampsize,
                                clara_trace,
                                clara_rngR,
                                clara_pamLike,
                                clara_correct.d) {

  if (all(is.na(gf_predicted$gf))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                      gf_fixed_clust = list(NA),
                      gf_nbclust = list(NA)
                      ))
  }

  ## Clustering according to multiple  methods
  ## Fixed K
  ## NbClust

  ## Should have data to cluster in gf_predicted$comp_turnover

  gf_fixed_clusts <- cluster::clara(
    gf_predicted$comp_turnover[[1]],
    cluster_fixed_k,
    metric = "manhattan",
    samples = clara_samples,
    sampsize = clara_sampsize,
    trace = clara_trace,
    rngR = clara_rngR,
    pamLike = clara_pamLike,
    correct.d = clara_correct.d
    )

  gf_nbclust <- NbClust::NbClust(
    gf_predicted$comp_turnover[[1]],
    distance = "manhattan",
    min.nc = min(k_range),
    max.nc = max(k_range),
    method = "kmeans",
    index = "alllong"
  )


  return(data.table(gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                    gf_fixed_clust = list(gf_fixed_clusts),
                    gf_nbclust = list(gf_nbclust)
                    ))

}
