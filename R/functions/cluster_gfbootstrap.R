cluster_gfbootstrap <- function(
                                gfbootstrap_predicted,
                                env_domain,
                                spatial_vars
                                ) {


  if(any(is.na(gfbootstrap_predicted$sim_mat[[1]]))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gfbootstrap_predicted[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid)],
      caster_clust = list(NA),
      clust_ind = list(NA)
    ))

  }


    ## return(data.table::data.table(gfbootstrap_survey[, .(env_domain, trophic, survey, depth_cat)],
    ##   env_pred_stats = list(predicted_stats),
    ##   env_pred_raw = list(predicted),
    ##   sim_mat = list(sim_mat)

    ## ))

 caster_clust <-  data.table::setDT(castcluster::cast_optimal(gfbootstrap_predicted$sim_mat[[1]][[1]]))


  best_clust <- which.max(caster_clust$gamma)

  clust_ind <- data.table::rbindlist(lapply(seq_along(caster_clust$cast_ob[[best_clust]]), function(x) {data.table::data.table(x_row = caster_clust$cast_ob[[best_clust]][[x]], cl = x)}))
  data.table::setkey(clust_ind, "x_row")

  clust_ind[env_domain[domain == gfbootstrap_predicted$env_domain[[1]], data][[1]], on = c(x_row = env_id_col),
            c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

  clust_ind[, cl_factor := as.factor(cl)]

  return(data.table(gfbootstrap_predicted[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid)],
                    caster_clust = list(caster_clust),
                    best_clust = best_clust,
                    clust_ind = list(clust_ind)
                    ))

}
