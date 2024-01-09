assign_new_sites_to_cluster_caster <- function(cluster_env_extrapolate,
                                               gfbootstrap_cluster,
                                               env_domain,
                                               env_id_col,
                                               spatial_vars) {
{

    ## predict(cast_ob_with_sim_mat, pred_sim_mat)

		## Batch over list in cluster_env_extrapolate
		row_clust_membership <- cluster_env_extrapolate$extrap_sims[[1]][ ,
														caster_extrap_membership(
																.SD$site_pairs[[1]],
																gfbootstrap_cluster,
																cluster_env_extrapolate$env_id[[1]][[env_id_col]],
																env_id_col),
														by = batch_ind]
    ## Have membership for each x_row
    ## Need to create the clust_ind table for the polygons function

		clust_ind <- rbindlist(row_clust_membership$max)

    ## Some sites are dropped due to depth
    ## Some sites are dropped due to det == 0
    ## All sites are in env_domain
    ## Sites after dropping due to depth are in cluster_env_extrapolate$env_id
    ## Want all sites from cluster_env_extrapolate$env_id, with
    ## det == 0 sites set to cluster NA.
    env_ids <- cluster_env_extrapolate$env_id[[1]][[env_id_col]]
    missing_env_rows <- !(env_ids %in% clust_ind[[env_id_col]])
    missing_env <- data.table::data.table(
                                   cl = rep(NA, sum(missing_env_rows)),
                                   cl_factor = rep(NA, sum(missing_env_rows))
                               )
    missing_env[[env_id_col]] <- env_ids[missing_env_rows]
    clust_ind <- rbind(clust_ind, missing_env)



    pred_clust_prob_full <- matrix(0,
      nrow = length(env_ids),
      ncol = length(gfbootstrap_cluster$best_clust_ob[[1]])
      )

    for (r in seq.int(nrow(cluster_env_extrapolate$extrap_sims[[1]]))) {
      pred_clust_prob_full[cluster_env_extrapolate$extrap_sims[[1]][r, site_pairs][[1]][cluster == 1, new], ] <- row_clust_membership[r, prob_cl][[1]]
    }
    pred_clust_prob_full[is.nan(pred_clust_prob_full)] <- 0

    clust_ind[
      env_domain[
        domain == gfbootstrap_cluster$env_domain[[1]] &
          res == gfbootstrap_cluster$res_gf &
          env_year == cluster_env_extrapolate$env_year &
          env_pathway == cluster_env_extrapolate$env_pathway,
        data
      ][[1]],
      on = c(env_id_col),
      c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))
    ]


    return(data.table(gfbootstrap_cluster[, .(env_domain, res_gf, res_clust, trophic, survey, depth_cat, clust_method)],
                      env_year = cluster_env_extrapolate$env_year,
                      env_pathway = cluster_env_extrapolate$env_pathway,
                      clust_ind = list(clust_ind),
                      pred_membership = list(row_clust_membership)
                      ))
}


caster_extrap_membership <- function(long_sim_mat,
																		 gfbootstrap_cluster,
																		 env_ids,
																		 env_id_col) {

    n_cluster_sites <- max(long_sim_mat$cluster)
		pred_sim_mat <- matrix(long_sim_mat$bhatt_vec,
													 ncol = n_cluster_sites)

    ## Temporary, use local predict_clust for debugging
    predicted_cluster_membership <- castcluster::predict_clust(
				gfbootstrap_cluster$best_clust_ob[[1]],
				pred_sim_mat,
				aff_thres = gfbootstrap_cluster$clust[[1]]$aff_thres[
																											gfbootstrap_cluster$best_clust],
				type = "max")
    predicted_cluster_probability <- castcluster::predict_clust(
      gfbootstrap_cluster$best_clust_ob[[1]],
      pred_sim_mat,
      type = "probability"
    )

    ## The length of gfbootstrap_cluster$best_clust_ob is
    ## not the same as max(long_sim_mat$cluster)
    ## the caster object is shorter by 1.
    ## Found problem: column "cluster" is not cluster id, but
    ## site id from clustering.


    ## env_ids contains env_domain row ids.
    ## env_ids maps from valid sites (row id) to env_domain rows
    ## A valid site is one that has an appropriate depth.
    ## All valid sites are included in the clustering.
    ## The original sim mat uses all sites, and det==0 sites just get 0 everywhere.
    ## long_sim_mat$new maps to valid sites.
    ## pred_sim_mat drops mapping
    ## Can recover it from long_sim_mat[cluster == 1, new]
    ## However, some sites have det==0, and are not included in long_sim_mat.
    ## long_sim_mat$new is using env_domain id's as well
		max_dt <- data.table::data.table(cl = predicted_cluster_membership, cl_factor = as.factor(predicted_cluster_membership))
    max_dt[, c(env_id_col) := env_ids[long_sim_mat[cluster == 1, new]]]


		return(list(max = list(max_dt), prob_cl = list(predicted_cluster_probability)))
}
