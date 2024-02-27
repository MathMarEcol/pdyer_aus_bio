nbclust_generate_branches <- function(gf_predicted,
                                      k_range,
                                      nbclust_index_metadata,
                                      nbclust_dist,
                                      nbclust_method,
                                      nbclust_max_runtime,
                                      nbclust_include_graphical) {
  ## Filter indexes by runtime and graphical
  runtime_filter <- nbclust_index_metadata$runtime <= nbclust_max_runtime
  graphical_filter <- !nbclust_index_metadata$graphical | nbclust_include_graphical

  nbclust_index_metadata[, rowid := seq.int(nrow(nbclust_index_metadata))]

  ## Serial branches are a single row per index per gf x dist x method
  ## min_k and max_k are derived from k_range
  serial_rows <- (nbclust_index_metadata$serial | nbclust_index_metadata$runtime <= 2) &
    runtime_filter &
    graphical_filter

  nbclust_serial <- data.table::CJ(
    gf_ind = seq.int(nrow(gf_predicted)),
    dist = nbclust_dist,
    method = nbclust_method,
    metric_ind = nbclust_index_metadata$rowid[serial_rows]
  )

  nbclust_serial[, c("k_min", "k_max") :=
    .(
      rep(min(k_range), nrow(nbclust_serial)),
      rep(max(k_range), nrow(nbclust_serial))
    )]

  nbclust_serial[, parallel := FALSE]

  ## Parallel branches are row per k per index per gf
  parallel_rows <- !(nbclust_index_metadata$serial | nbclust_index_metadata$runtime <= 2) &
    runtime_filter &
    graphical_filter

  nbclust_parallel <- data.table::CJ(
    gf_ind = seq.int(nrow(gf_predicted)),
    dist = nbclust_dist,
    method = nbclust_method,
    metric_ind = nbclust_index_metadata$rowid[parallel_rows],
    k_min = seq.int(min(k_range), max(k_range))
  )

  nbclust_parallel[, k_max := .(k_min)]
  nbclust_parallel[, parallel := TRUE]
  return(rbind(nbclust_serial, nbclust_parallel))
}

nbclust_fit_branches <- function(gf_predicted,
                                 nbclust_index_metadata,
                                 nbclust_branch_table,
                                 env_biooracle_names) {
  ## Best.nc across all k
  ## index for k
  gf_predicted <- gf_predicted[nbclust_branch_table$gf_ind, ]
  nbclust_index_metadata <- nbclust_index_metadata[ nbclust_branch_table$metric_ind, ]

  if (all(is.na(gf_predicted$imp_preds))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(nbclust_branch_table,
      bestnc = NA,
      metric = NA,
      crit = NA
    ))
  }

  gf_nbclust <- tryCatch(
  {
    NbClust::NbClust(
      gf_predicted$comp_turnover[[1]][, c(gf_predicted$imp_preds[[1]]), with = FALSE],
      distance = nbclust_branch_table$dist,
      min.nc = nbclust_branch_table$k_min,
      max.nc = nbclust_branch_table$k_max,
      method = nbclust_branch_table$method,
      index = nbclust_index_metadata$index
    )
  },
  error = function(e) {
    return(e)
  }
  )

  best_nc <- NA
  metric <- NA
  crit <- NA

  if (!inherits(gf_nbclust, "error")) {
    if (nbclust_branch_table$parallel) {
      metric <- gf_nbclust$All.index
      if (nbclust_index_metadata$crit) {
        crit <- gf_nbclust$All.CriticalValues
      }
    } else {
        best_nc <- gf_nbclust$Best.nc["Number_clusters"]
    }
  }

  return(data.table(nbclust_branch_table,
                    bestnc = best_nc,
                    metric = metric,
                    crit = crit,
                    nbclust_index = nbclust_index_metadata$index
                    ))
}

nbclust_merge_branches <- function(nbclust_branch_fitted,
                                   gf_predicted,
                                   nbclust_index_metadata
                                   ) {

  ## First, collapse parallel indicies
    gf_cluster_nbclust_tmp <- nbclust_branch_fitted[

    ## All rows of parallel indices
    parallel == TRUE,
    {

      ordered <- .SD[order(k_min)]
      if (nbclust_index_metadata[index == .BY[["nbclust_index"]], "crit"][[1]] == FALSE) {
        ## Pass function metric
        bestnc <- nbclust_index_metadata[index == .BY[["nbclust_index"]], "optima_func"][[1]][[1]](ordered$metric)
      } else {
        ## Pass function metric and crit
        bestnc <- nbclust_index_metadata[index == .BY[["nbclust_index"]], "optima_func"][[1]][[1]](ordered$metric,
          ordered$crit)
      }
      best_nc <- ordered$k_min[bestnc]
      if (length(best_nc) != 0) {
        list(bestnc = best_nc)
      } else {
        list(bestnc = -1)
      }
    },
    by = c("gf_ind", "dist", "method", "nbclust_index")
  ]
  ## Add serial metrics back
  gf_cluster_nbclust_all <- rbind(
    gf_cluster_nbclust_tmp,
    nbclust_branch_fitted[parallel == FALSE, colnames(gf_cluster_nbclust_tmp), with=FALSE]
  )

  ## Next, collapse back to gf objects

  gf_cluster_nbclust <- gf_cluster_nbclust_all[,
                         list(best_nc = list(setNames(.SD$bestnc, .SD$nbclust_index))),
                         by = c("gf_ind", "dist", "method")]


  gf_predicted[, gf_ind := seq.int(nrow(gf_predicted))]

  out <- gf_cluster_nbclust[gf_predicted, on = "gf_ind"]

  out[, c("comp_turnover", "imp_preds", "env_id", "gf_ind") := NULL]

  return(out)

}

