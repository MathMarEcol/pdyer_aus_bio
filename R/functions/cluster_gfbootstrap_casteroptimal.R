cluster_gfbootstrap_casteroptimal <- function(
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
    oplan <- plan()
    if ({nworkers <- as.numeric(Sys.getenv("FUTURE_WORKERS", 1))} > 1 && inherits(plan(), "sequential")) {
        plan(future.callr::callr, workers = min(nworkers, m))
    }

    aff_range = range(gfbootstrap_predicted$sim_mat[[1]][[1]][
                                                upper.tri(gfbootstrap_predicted$sim_mat[[1]][[1]])])
    min_range = diff(aff_range)/100

 caster_clust <-  data.table::setDT(castcluster::cast_optimal(gfbootstrap_predicted$sim_mat[[1]][[1]], m = m, min_tol = min_tol, return_full = keep_all_clusts, aff_range = aff_range, min_range = min_range))


  best_clust <- which.max(caster_clust$gamma)
  best_clust_ob <- caster_clust$cast_ob[[best_clust]]
  clust_ind <- data.table::rbindlist(lapply(seq_along(best_clust_ob), function(x) {data.table::data.table(x_row = caster_clust$cast_ob[[best_clust]][[x]], cl = x)}))
  data.table::setkey(clust_ind, "x_row")
    clust_ind <-  cbind(clust_ind, gfbootstrap_predicted$env_id[[1]][, ..env_id_col])
    clust_ind[env_domain[domain == gfbootstrap_predicted$env_domain[[1]] &
                       res == gfbootstrap_predicted$res_clust &
                         env_year == gfbootstrap_predicted$env_year &
                        env_pathway == gfbootstrap_predicted$env_pathway, data][[1]], on = c(env_id_col),
              c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

  clust_ind[, cl_factor := as.factor(cl)]



    plan(oplan)
    return(data.table(gfbootstrap_predicted[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                      clust_method = clust_methods,
                      clust = list(caster_clust),
                      best_clust = best_clust,
                      best_clust_ob = list(best_clust_ob),
                      clust_ind = list(clust_ind)
                      ))


}
