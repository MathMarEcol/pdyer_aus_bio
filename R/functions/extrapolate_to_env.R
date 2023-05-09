extrapolate_to_env <- function(
                               gfbootstrap_combined,
                               gfbootstrap_predicted,
                               gfbootstrap_cluster,
                               env_domain_plot,
                               env_biooracle_names,
                               extrap,
                               pred_importance_top,
                               env_id_col,
                               depth_range
                               ) {


    if (any(is.na(gfbootstrap_combined$gfbootstrap[[1]]))) {
        ## Upstream target decided survey was not usable.
        ## Propagating
        ##
        return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
         TODO: put NA's for intended return value'                 ????
                          ))
    }
    
    ## Make sure env domain is ready

    env_dom <- env_domain_plot[domain ==  gfbootstrap_combined$env_domain, data][[1]]

    if (gfbootstrap_combined$depth_cat !=  "all" ) {
        env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gfbootstrap_combined$depth_cat]]), ]
    }

    predicted <- predict(object = gfbootstrap_combined$gfbootstrap[[1]],
                         newdata = env_dom[,..env_biooracle_names],
                         ## Just take points, and calculate full coefficient matrix from points
                         type = c("points"),
                         extrap = extrap)
    pred_points <- predicted$points
    data.table::setDT(pred_points)
    imp <- importance(gfbootstrap_combined$gfbootstrap[[1]], sort = TRUE)
    if (pred_importance_top >= 1) {
        imp_preds <- names(imp)[seq.int(1,min(length(imp), pred_importance_top))]
    } else {
        imp_explained <- cumsum(imp)/sum(imp)
        ## Take all predictors below threshold, then one more
        n_preds <- sum(imp_explained < pred_importance_top) + 1
        imp_preds <- names(imp)[seq.int(1,n_preds)]
    }
    
    predicted_stats <- pred_points[pred %in% imp_preds,
                                        #predicted_stats <- pred_points[,
    {
        wide_boot <- data.table::dcast(
                                     .SD[, .(pred, y, gf)],
                                     gf ~ pred,
                                     value.var = "y")
        wide_boot[, gf := NULL]
        site_mean <- colMeans(wide_boot)
        site_sigma <- cov(wide_boot)
        out <- data.table::data.table(site_mean = list(site_mean),
                                      site_sigma = list(site_sigma),
                                      site_sigma_det = determinant(site_sigma, logarithm=FALSE)$modulus)
    },
    by = c("x_row")]
    
    ## Assumes that predict() preserves the order of newdata
    data.table::setkey(predicted_stats, "x_row")
    predicted_stats[ , c(env_id_col) := env_dom[[env_id_col]]]

    ## Here we create a "similarity matrix" between just-predicted sites
    ## and predicted sites from earlier

    gfbootstrap_predicted$env_pred_stats contains the target sites
    gfbootstrap_predicted$sim_mat contains the internal similarities of old sites
    gfbbotstrap_cluster$ contains the clustering object.

    site_pairs <- data.table::CJ(old = gfbootstrap_predicted$env_pred_stats[[1]]$x_row,
                                 new = predicted_stats$x_row)
    dist_long <- purrr::map2_dbl(site_pairs$new, site_pairs$old,
                                 \(new_site, old_site){
                                     b_dist <- my_bhattacharyya_dist(
                                         predicted_stats$site_mean[[new_site]],
                                         predicted_stats$site_sigma[[new_site]],
                                         predicted_stats$site_sigma_det[[new_site]],
                                         gfbootstrap_predicted$env_pred_stats[[1]]$site_mean[[old_site]],
                                         gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma[[old_site]],
                                         gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma_det[[old_site]]
                                     )
    })
    ## R is "column-major" so matrix does all rows per col
    ## here col is old, so new should change faster

    pred_sim_mat <- matrix(exp(-dist_long), nrow(predicted_stats), nrow(gfbootstrap_predicted$env_pred_stats[[1]]))

    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
                                  env_pred_stats = list(predicted_stats),
                                  env_pred_raw = list(predicted),
                                  imp_preds = list(imp_preds),
                                  pred_sim_mat = list(list(pred_sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
                                  ))
    
    
    


}
