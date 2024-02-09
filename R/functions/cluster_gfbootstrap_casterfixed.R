cluster_gfbootstrap_casterfixed <- function(
                                       clust_methods,
                                       gfbootstrap_predicted,
                                       env_domain,
                                       env_id_col,
                                       spatial_vars,
                                       aff_thres) {

    ## Select mean similarity as aff_thres.
    ## Will not give peak Hubert Gamma, but should be near max,
    ## given that Hubert Gamma is increased for similarities
    ## within a cluster that are above the mean similarity.
    sim_mat <- gfbootstrap_predicted$sim_mat[[1]][[1]]


    clust_first_pass <- castcluster::cast_alg(sim_mat, aff_thres)
    clust_stabilise <- castcluster::cast_stabilize(clust_first_pass,
                                                   aff_thres,
                                                   sim_mat)

    if(length(clust_stabilise) == 1) {
        h <- NA
    } else {
        mem_mat <- castcluster::membership_mat(clust_stabilise)
        h <- castcluster::hubert_gamma(sim_mat, mem_mat, norm_z = TRUE)
    }
    caster_clust <- data.table::data.table(aff_thres = aff_thres, gamma = h,
                      k = length(clust_stabilise),
                      cast_ob = I(list(clust_stabilise)))

  best_clust <- 1
  best_clust_ob <- caster_clust$cast_ob[[best_clust]]
  clust_ind <- data.table::rbindlist(lapply(seq_along(best_clust_ob), function(x) {data.table::data.table(x_row = caster_clust$cast_ob[[best_clust]][[x]], cl = x)}))
  data.table::setkey(clust_ind, "x_row")
    clust_ind <-  cbind(clust_ind, gfbootstrap_predicted$env_id[[1]][, ..env_id_col])
    clust_ind[env_domain[domain == gfbootstrap_predicted$env_domain[[1]] &
                       res == gfbootstrap_predicted$res_clust  &
                         env_year == gfbootstrap_predicted$env_year &
                        env_pathway == gfbootstrap_predicted$env_pathway, data][[1]], on = c(env_id_col),
              c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

  clust_ind[, cl_factor := as.factor(cl)]

    return(data.table(gfbootstrap_predicted[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat)],
                      clust_method = clust_methods,
                    clust = list(caster_clust),
                    best_clust = best_clust,
                    best_clust_ob = list(best_clust_ob),
                    clust_ind = list(clust_ind)
                    ))


}
