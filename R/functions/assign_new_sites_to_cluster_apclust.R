assign_new_sites_to_cluster_apclust <- function(
                                                cluster_env_extrapolate,
                                                gfbootstrap_cluster,
                                                env_domain,
                                                env_id_col,
                                                spatial_vars
                                                ) {
    ## Apcluster does not seem to have a function for "predict".
    ## However, an apcluster object is a set of exemplars
    ## Just need to find the nearest exemplar.
    exemplars <- gfbootstrap_cluster$clust[[1]]$apc_ob[[1]]@exemplars

		## Batch over list from cluster_env_extrapolate
		row_clust_membership <- cluster_env_extrapolate$extrap_sims[[1]][ ,
														by = batch_ind,
														apclust_extrap_membership(
																.SD$site_pairs[[1]],
																exemplars,
																cluster_env_extrapolate$env_id[[1]][[env_id_col]],
																env_id_col)
                            ]

    ## Have membership for each x_row.

    clust_ind <- data.table::rbindlist(row_clust_membership$max)


    clust_ind[
      env_domain[domain == gfbootstrap_cluster$env_domain[[1]] &
        res == gfbootstrap_cluster$res_gf &
        env_year == cluster_env_extrapolate$env_year &
        env_pathway == cluster_env_extrapolate$env_pathway, data][[1]],
      on = c(env_id_col),
      c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))
    ]

    clust_ind[ , c("env_id"):= NULL]

    return(data.table(
        gfbootstrap_cluster[,
                            .(env_domain, res_gf, res_clust, trophic, survey, depth_cat, clust_method)],
        env_year = cluster_env_extrapolate$env_year,
        env_pathway = cluster_env_extrapolate$env_pathway,
        clust_ind = list(clust_ind),
        pred_membership = list(row_clust_membership)
    )
    )

}


apclust_extrap_membership <- function (long_sim_mat,
																			 exemplars,
																			 env_ids,
																			 env_id_col) {

    ## Drop any "cluster" sites that are not
    ## exemplars, then
    ## For each unique "new" site,
    ## Find the "new" "cluster" "bhatt_vec" row
    ## that maximises bhattacharyya coefficient.
		max_clust <- long_sim_mat[cluster %in% exemplars,
								 by = new,
								 .SD[which.max(bhatt_vec),]
                 ]

    ## Normalise similarity vector to exemplars to
    ## get probability of membership
    prob_cl <- long_sim_mat[cluster %in% exemplars,
                            by = new,
                            list(bhatt_vec = .SD$bhatt_vec / sum(.SD$bhatt_vec),
                                 cluster)
                            ]


    ## Convert to factor then take levels to clusters 1-5.
    ## Maybe avoid factor, and go for explicit merge
		exemplar_to_clust <- data.table::data.table(exemplars = exemplars,
                                                cl =
                                                    seq.int(along.with =
                                                                exemplars))
		max_clust <- max_clust[exemplar_to_clust,
                           on = c(cluster = "exemplars"),
                           nomatch = NULL]
		setkeyv(max_clust, c("new"))

    max_clust[, cl_factor := as.factor(cl)]
		max_clust[, c(env_id_col) := env_ids[new]]

    max_clust[, c("new", "cluster", "bhatt_vec") := NULL]

    prob_cl <- prob_cl[exemplar_to_clust,
                           on = c(cluster = "exemplars"),
                           nomatch = NULL]
		setkeyv(prob_cl, c("new"))

    prob_cl[, cl_factor := as.factor(cl)]
		prob_cl[, c(env_id_col) := env_ids[new]]
    prob_cl_wide <- data.table::dcast(prob_cl, new ~ cl, value.var = "bhatt_vec")
    prob_cl_wide[, new := NULL]
    prob_cl_wide_matrix <- as.matrix(prob_cl_wide)
  return(list(max = list(max_clust), prob_cl = list(prob_cl_wide_matrix)))

}
