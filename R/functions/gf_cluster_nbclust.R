fit_nbclust <- function(
                                   gf_predicted,
                                   env_biooracle_names,
                                   nbclust_dist,
                                   nbclust_method,
                                   nbclust_index,
                                   k_range
                                   ) {

  if (all(is.na(gf_predicted$imp_preds))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                      clust_method = "kmedoids",
                      gf_nbclust = list(NA)
                      ))
  }


  gf_nbclust <- tryCatch(
    {
      NbClust::NbClust(
        gf_predicted$comp_turnover[[1]][, env_biooracle_names, with = FALSE],
        distance = nbclust_dist,
        min.nc = min(k_range),
        max.nc = max(k_range),
        method = nbclust_method,
        index = nbclust_index
      )
    },
    error = function(e) {
      return(e)
    }
  )

  if (inherits(gf_nbclust, "error")) {
    best_nc <- 0
  } else {
    best_nc <- gf_nbclust$Best.nc["Number_clusters"]
  }

  return(data.table(gf_predicted[, .(env_domain, trophic, survey, depth_cat)],
                    clust_method = "kmedoids",
                    gf_nbclust = list(gf_nbclust),
                    best_nc = best_nc,
                    nbclust_index = nbclust_index
                    ))
}



merge_nbclust <- function(
                          gf_cluster_nbclust_tmp
                          ) {

  ## gf_cluster_nbclust <- gf_cluster_nbclust_tmp[
   ## ,
     ## ...convert best_nc into a vector. Labels not needed, but could be added as vector names.
##
  ## by = c("env_domain", "trophic", "survey", "depth_cat", "clust_method")]
}
