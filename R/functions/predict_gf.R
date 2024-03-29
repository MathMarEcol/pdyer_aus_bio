# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
predict_gf <- function(
                       gf_combined,
                       res_clust_target,
                       env_domain,
                       env_biooracle_names,
                       extrap,
                       pred_importance_top,
                       env_id_col,
                       depth_range
                       ) {

  if (all(is.na(gf_combined$gf))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
      return(data.table::data.table(gf_combined[, .(env_domain, env_year, env_pathway, res_gf, trophic, survey, depth_cat)],
                                  res_clust = res_clust_target,
                                  env_id = list(NA),
                                  imp_preds = list(NA),
                                  comp_turnover = list(NA)
                                  ))
  }
  gf_combined$gf <- list(qs::qread(gf_combined$gf[[1]]))

  env_dom <- env_domain[domain == gf_combined$env_domain &
                        res == res_clust_target &
                        env_year == gf_combined$env_year &
                        env_pathway == gf_combined$env_pathway, "data"][[1]][[1]]

  if (gf_combined$depth_cat !=  "all" ) {
			env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gf_combined$depth_cat]]), ]
  }

    imp <- gradientForest::importance(gf_combined$gf[[1]], sort = TRUE)

    if (sum(imp) == 0) {
      ## GF  object is broken
      return(data.table(gf_combined[, .(env_domain, env_year, env_pathway, res_gf, trophic, survey, depth_cat)],
                        res_clust = res_clust_target,
                        env_id = list(NA),
                        imp_preds = list(NA),
                        sim_mat = list(NA)
                        ))
    }
    if (pred_importance_top >= 1) {
        imp_preds <- names(imp)[seq.int(1,min(length(imp), pred_importance_top))]
    } else {
        imp_explained <- cumsum(imp)/sum(imp)
        ## Take all predictors below threshold, then one more
        n_preds <- sum(imp_explained < pred_importance_top) + 1
        imp_preds <- names(imp)[seq.int(1,n_preds)]
    }

    n_gf <- length(gf_combined$gf[[1]]$gf_list)
		n_preds_raw <- length(env_biooracle_names[env_year == gf_combined$env_year & env_pathway == gf_combined$env_pathway, env_biooracle_names][[1]])
    n_preds <- length(imp_preds)

    comp_turnover <- predict(gf_combined$gf[[1]],
      newdata = env_dom[, ..imp_preds],
      extrap = extrap
      )

    comp_turnover <- data.table::setDT(cbind(comp_turnover, env_dom[,c(spatial_vars, env_id_col), with=FALSE]))

  ## Unusual format intended to reduce memory load
  return(data.table::setDT(list(
    env_domain = gf_combined$env_domain,
    env_year = gf_combined$env_year,
    env_pathway = gf_combined$env_pathway,
    res_gf = gf_combined$res_gf,
    trophic = gf_combined$trophic,
    survey = gf_combined$survey,
    depth_cat = gf_combined$depth_cat,
    res_clust = res_clust_target,
    env_id = list(env_dom[, ..env_id_col]),
    imp_preds = list(imp_preds),
    comp_turnover = list(comp_turnover)
  )))

}
