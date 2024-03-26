# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
cluster_gfbootstrap_apclustoptimal <- function(
                                        clust_methods,
                                        gfbootstrap_predicted,
                                env_domain,
                                env_id_col,
                                spatial_vars,
                                m,
                                min_range,
                                min_tol,
                                keep_all_clusts
                                ) {

     ap_clust <- data.table::setDT(apclust_optimise(log(gfbootstrap_predicted$sim_mat[[1]][[1]]), m = m, min_range = min_range, min_tol = min_tol, return_full = keep_all_clusts))


  best_clust <- which.max(ap_clust$gamma)
  best_clust_ob <- ap_clust$apc_ob[[best_clust]]@clusters
  clust_ind <- castcluster::cast_obj_to_df(best_clust_ob)
    names(clust_ind)[names(clust_ind) == "elem"] <- "x_row"
    names(clust_ind)[names(clust_ind) == "clust"] <- "cl"
    data.table::setDT(clust_ind)
      data.table::setkey(clust_ind, "x_row")
    clust_ind <-  cbind(clust_ind, gfbootstrap_predicted$env_id[[1]][, ..env_id_col])
    clust_ind[env_domain[domain == gfbootstrap_predicted$env_domain[[1]] &
                         res == gfbootstrap_predicted$res_clust &
                         env_year == gfbootstrap_predicted$env_year &
                        env_pathway == gfbootstrap_predicted$env_pathway,
                         data][[1]], on = c(env_id_col),
            c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

    clust_ind[, cl_factor := as.factor(cl)]

  return(data.table(gfbootstrap_predicted[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                    clust_method = clust_methods,
                    clust = list(ap_clust),
                    best_clust = best_clust,
                    best_clust_ob = I(list(best_clust_ob)),
                    clust_ind = list(clust_ind)
                    ))
}
apclust_opt_recurse <- function(sim_mat,
                         pref_range,
                         m,
                         min_range,
                         min_tol,
                         rec_data,
                         rec_depth){
    ## p is clustering parameter
    ## sim_mat is similarity matrix
    ## mem_mat is clustering in format needed for hubert_gamma
    ## gamma is our target metric
      ## Check for errors in recursion logic, or bad inputs
  ## assertthat::assert_that(diff(pref_range)[1] > min_range)
    ##first iteration, set up empty data.frame
    if(is.null(rec_data)){
        rec_data <- data.frame(pref = double(),
                               gamma = double(),
                               k = integer(),
                               apc_ob = I(list()),
                               rec_depth = integer())
        }
  ## Calculate Hubert's \Gamma statistic for each partition
    pref_parts <- seq(min(pref_range), max(pref_range), length.out = m)
  gamma_score <- do.call(rbind,
                         future.apply::future_lapply(pref_parts, function(pref, sim_mat, rec_depth, rec_data) {
                             if(pref %in% rec_data$pref) {
                                        #already calculated, reuse
                                 reuse_data <- rec_data[match(pref, rec_data$pref), ]
                                 out <- data.frame(pref = pref, gamma = reuse_data$gamma,
                                             k = reuse_data$k,
                                             apc_ob = I(reuse_data$apc_ob),
                                             rec_depth = rec_depth)
                                 return(out)
                                         
                             } else {
                                 apc <- apcluster(s = sim_mat, q = pref)
                                 ##May not return a well formed cluster
                                 ## if apclust does not converge.
                                 if(length(apc@clusters) < 2) {
                                        #no valid clustering found
                                     h <- NA
                                     out <- data.frame(pref = pref, gamma = h,
                                             k = length(apc@clusters),
                                             apc_ob = I(list(apc)),
                                             rec_depth = rec_depth)

                                 } else {
                                     mem_mat <- castcluster::membership_mat(apc@clusters)
                                     h <- castcluster::hubert_gamma(exp(sim_mat), mem_mat, norm_z = TRUE)
                                     out <- data.frame(pref = pref, gamma = h,
                                             k = length(apc@clusters),
                                             apc_ob = I(list(apc)),
                                             rec_depth = rec_depth)
                                 }
                                 return(out)
                             }
                         },  sim_mat = sim_mat, rec_depth = rec_depth, rec_data = rec_data, future.seed = TRUE)
  )

  ## Find the best gamma score
  max_score_ind <- which.max(gamma_score$gamma)

  new_range <- c(0,0)
  ## Remove the highest and lowest scores, unless
  ## the max is on one end
  new_range[1] <- pref_parts[2]
  new_range[2] <- pref_parts[m - 1]

  if(gamma_score$pref[max_score_ind] == min(pref_parts)) {
      new_range[1] <- min(pref_parts) - diff(pref_parts)[1] / 2
      new_range[2] <- pref_parts[m - 2]
    } else if(gamma_score$pref[max_score_ind] == max(pref_parts)){
        new_range[2] <- max(pref_parts) + diff(pref_parts)[1] / 2
        new_range[1] <- pref_parts[3]
  }
  if(new_range[1] == new_range[2]) {
      new_range[1] <- min(new_range) - diff(pref_parts)[1] / 2
      new_range[2] <- max(new_range) + diff(pref_parts)[1] / 2
  }
  new_range[new_range < 0] <- 0
  new_range[new_range > 1] <- 1
    ## Check whether to keep narrowing, or return
    if(all(is.nan(gamma_score$gamma))){
        stop("cluster_gfbootstrap_apclust.R: All gamma scores were errors")
    }
    invalid_diff <- sum(!is.nan(gamma_score$gamma)) <= 1  
        
  if((diff(range(gamma_score$gamma, na.rm = TRUE))[1] < min_tol && !invalid_diff) || diff(new_range)[1] < min_range) {
    return(rbind(rec_data, gamma_score))
  } else {
    return(apclust_opt_recurse(sim_mat = sim_mat,
                        pref_range = new_range,
                        m = m,
                        min_range = min_range,
                        min_tol = min_tol,
                        rec_data = rbind(rec_data, gamma_score),
                        rec_depth = rec_depth + 1
                        )
           )
  }


 }
apclust_optimise <- function(
                             sim_mat,
                             return_full = TRUE,
                             m = 4,
                             min_tol = 0.0001,
                             min_range = 0.001
                             ) {
  ##Check input is meaningful
  assertthat::assert_that(assertthat::are_equal(dim(sim_mat)[1], dim(sim_mat)[2])) ##sim_mat must be square
  assertthat::assert_that(m >= 4)
  assertthat::assert_that(min_tol > 0)

    pref_range <- c(0, 1)
  ##begin recursion
  ret <- apclust_opt_recurse(sim_mat = sim_mat,
                         pref_range = pref_range,
                         m = m,
                         min_range = min_range,
                         min_tol = min_tol,
                         rec_data = NULL,
                         rec_depth = 1)

  if(return_full) {
    return(ret)
  } else {
    return(ret[which.max(ret$gamma),])
  }
}


