predict_env <- function(
                        res_clust_target,
                        res_gf_target,

                        env_domain,
                        env_biooracle_names,
                        extrap,
                        pred_importance_top,
                        env_id_col,
                        depth_range
                       ) {

  ## PCA is easy to calculate, so generate all datasets in one worker

  ## env_domain
  ## only cluster at res_clust_target
  ## align with env_biooracle_names
  ## takine means and all
  ## only do epi
  ## res_gf



  pred_env <- env_domain
  pred_env <- pred_env[res %in% res_clust_target, ]
  pred_env[, `:=`(res_clust = res)]
  pred_env[, res := NULL]
  ## pred_env$trophic = "envonly"  #or envmeanonly
  ## pred_env$survey = "envonly"
  pred_env$depth_cat = "epi"

  all_combined <- data.table::CJ(
    r = seq.int(nrow(pred_env)),
    res_gf = res_gf_target,
    trophic = c("envonly", "envmeanonly")
  )

  pred_env$r = seq.int(nrow(pred_env))
  pred_env <- pred_env[all_combined, on = "r"]
  pred_env[, r := NULL]
  pred_env[, survey := trophic]
  pred_env[, env_domain := domain]
  pred_env[, domain := NULL]

  ## For each row, calculate PCA

  get_pca_env <- function(x, metadata) {
    ## print(metadata) ## list
    x <- x[[1]][[1]]
    ## Closure for env_id_col
    env_id <- x[,..env_id_col]

    ## closure for env_biooracle_names
    env_names <- switch(metadata$survey,
                        "envmeanonly" = {
                          ## Only means and bathymetry
                          env_names <- env_biooracle_names[
                            env_year == metadata$env_year &
                              env_pathway == metadata$env_pathway,
                            "env_biooracle_names"
                          ][[1]][[1]]
                          env_names <- env_names[grepl("mean_ss", env_names)]
                          c(env_names, "MS_bathy_5m")
                        },
                        {
                          ## default, all env
                          env_biooracle_names[
                            env_year == metadata$env_year &
                              env_pathway == metadata$env_pathway,
                            "env_biooracle_names"
                          ][[1]][[1]]
                        }
      )

    comp_turnover <- prcomp(x[, ..env_names], center = TRUE, scale = TRUE)
    imp_preds <- if (pred_importance_top >= 1) {
      names(
        sort(
          sqrt(
            comp_turnover$rotation[, 1]^2 +
              comp_turnover$rotation[, 2]^2
          ),
          decreasing = TRUE
        )
      )[seq.int(1, min(nrow(comp_turnover$rotation), pred_importance_top))]
    } else {
      imp_explained <- cumsum(comp_turnover$sdev) / sum(comp_turnover$sdev)
      ## Take all predictors below threshold, then one more
      n_preds <- sum(imp_explained < pred_importance_top) + 1
      names(
        sort(
          sqrt(
            comp_turnover$rotation[, 1]^2 +
              comp_turnover$rotation[, 2]^2
          ),
          decreasing = TRUE
        )
      )[seq.int(1, n_preds)]
    }

    ## Add coordinates and env_id back to transformed data
    x_trans <- data.table::data.table(comp_turnover$x[, seq.int(1, length(imp_preds))])

    ## Without copy, the returned names have the col names
    ## added after this line
    pc_names <- data.table::copy(names(x_trans))
    x_trans[, c(env_id_col) := x[[env_id_col]]]
    x_trans[x,
            c(spatial_vars) := mget(paste0("i.", c(spatial_vars))),
            on = c(env_id_col)
    ]

    list(
      env_id = list(env_id),
      imp_preds = list(pc_names),
      comp_turnover = list(x_trans)
    )
  }

  metadata_names <- names(pred_env)
  metadata_names <- metadata_names[!metadata_names %in% "data"]

  pred_env[,
    get_pca_env(.SD, .BY),
    by = metadata_names
  ]

}
