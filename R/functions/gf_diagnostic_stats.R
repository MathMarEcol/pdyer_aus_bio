gfbootstrap_diagnostic_stats <- function(gfbootstrap_combined,
                                         gfbootstrap_predicted,
                                         env_domain_cluster,
                                         gfbootstrap_cluster,
                                         env_biooracle_names,
                                         pred_importance_top

                                         ) {
  if (all(is.na(gfbootstrap_predicted$imp_preds))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
      ##
      fail <- data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
      species_total = NA,
      species_mean = NA,
      species_mean_sd = NA,
      n_sample_sites_total = NA,
      n_sample_sites_mean = NA,
      n_sample_sites_sd = NA,
      n_extrap_sites = NA,
      spec_r2_total = NA,
      spec_r2_median = NA,
      spec_r2_mean = NA,
      spec_r2_sd = NA,
      survey_r2_mean = NA,
      survey_r2_sd = NA,
      nsurveys = NA,
      var_spheq = list(NA),
      var_spheq_mean = NA,
      var_spheq_sd = NA,
      sd_spheq = list(NA),
      sd_spheq_mean = NA,
      sd_spheq_sd = NA,
      extrap_score = list(NA),
      extrap_score_mean = NA,
      extrap_score_sd = NA,
      cumimp_magnitude = list(NA),
      cumimp_magnitude_mean = NA,
      cumimp_magnitude_sd = NA,
      mean_similarity = NA,
      ngf = NA,
      npreds = NA,
      type = NA,
      clust_count = list(NA),
      clust_count_mean = NA,
      clust_count_sd = NA,
      clust_count_best = NA,
      clust_score = list(NA),
      clust_score_mean = NA,
      clust_score_sd = NA,
      clust_score_best = NA
      )
      data.table::setcolorder(fail, sort(names(fail)))
    return(fail)
  }

  gf_list <- gfbootstrap_combined$gfbootstrap[[1]]$gf_list



  stats <- list()

  ## Get cluster counts
  clusterings <- gfbootstrap_cluster[gfbootstrap_predicted,
                                     on = .NATURAL]
  ## There should always be some matching rows,
  ## even if the clustering failed for some reason.
  stopifnot(nrow(clusterings) > 0)

  stats$clust_count <- list(vapply(
    clusterings$clust,
    \(x){
      x$k
    }, integer(1)
  ))
  names(stats$clust_count[[1]]) <- clusterings$clust_method


  stats$clust_count_mean <- mean(stats$clust_count[[1]])

  stats$clust_count_sd <- sd(stats$clust_count[[1]])


  stats$clust_score <- list(vapply(
    clusterings$clust,
    \(x){
      x$gamma
    }, numeric(1)
  ))
  names(stats$clust_score[[1]]) <- clusterings$clust_method

  stats$clust_score_mean <- mean(stats$clust_score[[1]])

  stats$clust_score_sd <- sd(stats$clust_score[[1]])

  stats$clust_score_best <- max(stats$clust_score[[1]])

  stats$clust_count_best <- stats$clust_count[[1]][which.max(stats$clust_score[[1]])]

  ## Variance at each env site in "sph"erical "eq"uivalent
  ## Essentially a comparable measure that represents volume of the
  ## variance
  stats$var_spheq <- list(
    ## Get variance matrix at each env site
    apply(
      gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma,
      1, \(sigma){
        ## Get nthroot(diag(sigma)
        prod(diag(sigma))^(1 / nrow(sigma))
      }
    )
  )
  stats$var_spheq_mean = mean(stats$var_spheq[[1]])
  stats$var_spheq_sd = sd(stats$var_spheq[[1]])
  stats$sd_spheq = list(sqrt(stats$var_spheq[[1]]))
  stats$sd_spheq_mean = mean(sqrt(stats$var_spheq[[1]]))
  stats$sd_spheq_sd = sd(sqrt(stats$var_spheq[[1]]))

  ## Position of a site in compositional turnover space
  ## is manhattan.
  stats$cumimp_magnitude <- list(apply(
    gfbootstrap_predicted$env_pred_stats[[1]]$site_mean,
    1, \(cumimp){
      sum(cumimp)
    }
  ))
  stats$cumimp_magnitude_mean = mean(stats$cumimp_magnitude[[1]])
  stats$cumimp_magnitude_sd = sd(stats$cumimp_magnitude[[1]])


  sim_mat <- gfbootstrap_predicted$sim_mat[[1]][[1]]
  stats$mean_similarity <- mean(sim_mat[upper.tri(sim_mat)])



  ## Get degree of extrapolation
  ## A score of 1 indicates that all predictors at a site were extrapolated
  ## A score of 0 indicates no predictors were extrapolated.
  ## Extrapolated means the site is outside the range of samples seen during fitting.
  env_dom <- env_domain_cluster[domain ==  gfbootstrap_combined$env_domain, data][[1]]
  if (gfbootstrap_combined$depth_cat !=  "all" ) {
			env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gfbootstrap_combined$depth_cat]]), ]
  }
    imp <- gradientForest::importance(gfbootstrap_combined$gfbootstrap[[1]], sort = TRUE)

  if (sum(imp) == 0) {
      ## GF bootstrap object is broken
      return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
                        env_pred_stats = list(NA),
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
  predicted <- predict(
    object = gfbootstrap_combined$gfbootstrap[[1]],
    newdata = env_dom[, ..env_biooracle_names],
    ## Just take points, and calculate full coefficient matrix from points
    type = c("points"),
    extrap = NA,
    avoid_copy  = TRUE
  )
  data.table::setDT(predicted)

  ## Need to average over each gf object created by bootstrapping
  stats$ngf <- length(gfbootstrap_combined$gfbootstrap[[1]]$gf_list)
  ## Also average over number of predictors
  stats$npreds <- length(imp_preds)
  ## Important to know the number of sites
  stats$n_extrap_sites <- nrow(env_dom)

  stats$extrap_score <- list(predicted[,
    by = x_row,
    list(extrap_score = sum(is.na(y)) / (stats$ngf * stats$npreds))
  ][, extrap_score])

  stats$extrap_score_mean <- mean(stats$extrap_score[[1]])
  stats$extrap_score_sd <- sd(stats$extrap_score[[1]])

  if (inherits(gf_list[[1]], "combinedGradientForest")) {
      stats$type <- "combinedGradientForest"

      ## Number of species
      ## total of all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$species_total = mean(vapply(
          gf_list, \(gf){
              sum(gf$nspec)
          },
          numeric(1)
      ))


      ## mean of all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$species_mean = mean(vapply(
          gf_list, \(gf){
              mean(gf$nspec)
          },
          numeric(1)
      ))

      ## sd of species counts in
      ## combinedGF
      ## Average across bootstraps
      stats$species_mean_sd = mean(vapply(
          gf_list, \(gf){
              sd(gf$nspec)
          },
          numeric(1)
      ))

      ## Number of sites
      ## total of all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$n_sample_sites_total = mean(vapply(
          gf_list, \(gf){
              sum(vapply(
                  gf$X, \(x){
                      nrow(x)
                  },
                  numeric(1)
              ))
          },
          numeric(1)
      ))
      ## mean number of sites across all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$n_sample_sites_mean = mean(vapply(
          gf_list, \(gf){
              mean(vapply(
                  gf$X, \(x){
                      nrow(x)
                  },
                  numeric(1)
              ))
          },
          numeric(1)
      ))
            ## sd number of sites of all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$n_sample_sites_sd = mean(vapply(
          gf_list, \(gf){
              sd(vapply(
                  gf$X, \(x){
                      nrow(x)
                  },
                  numeric(1)
              ))
          },
          numeric(1)
      ))

      ## Species R^2
      ## total of all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$spec_r2_total = mean(vapply(
        gf_list, \(gf){
          sum(gf$rsq)
        },
        numeric(1)
      ))
      ## Species R^2 median across all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$spec_r2_median = mean(vapply(
          gf_list, \(gf){
              median(gf$rsq)
          },
          numeric(1)
      ))
      ## Species R^2 mean across all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$spec_r2_mean = mean(vapply(
          gf_list, \(gf){
              mean(gf$rsq)
          },
          numeric(1)
      ))
      ## Species R^2 sd across all surveys
      ## in combinedGF
      ## Average across bootstraps
      stats$spec_r2_sd = mean(vapply(
          gf_list, \(gf){
              sd(gf$rsq) ## TODO
          },
          numeric(1)
      ))
      ## Survey R^2
      ## Survey R^2 of combinedGF
      ## Average across bootstraps
      stats$survey_r2_mean = mean(vapply(
          gf_list, \(gf){
              sum(importance(gf))
          },
          numeric(1)
      ))
      ## Survey R^2
      ## in combinedGF
      ## sd across bootstraps
      stats$survey_r2_sd = sd(vapply(
          gf_list, \(gf){
              sum(importance(gf))
          },
          numeric(1)
      ))

      ## Number of surveys
      stats$nsurveys = mean(vapply(
          gf_list, \(gf){
              length(gf$X)
          },
          numeric(1)
      ))

  } else if (inherits(gf_list[[1]], "gradientForest")) {

    stats$type <- "gradientForest"
    ## Number of species
    ## Single survey, here number of species
    ## is just mean of bootstrapping selection
    ## Average across bootstraps
    stats$species_total <- mean(vapply(
      gf_list, \(gf){
        gf$species.pos.rsq
      },
      numeric(1)
    ))


    ## mean of all surveys
    ## for single survey,
    ## identical to species_total (n=1)
    ## Average across bootstraps
    stats$species_mean = mean(vapply(
        gf_list, \(gf){
            gf$species.pos.rsq
        },
        numeric(1)
    ))
    ## SD in species
    ## Here SD of bootstrapping
    stats$species_mean_sd = sd(vapply(
        gf_list, \(gf){
            gf$species.pos.rsq
        },
        numeric(1)
    ))

      ## Number of sites
      ## for single survey,
      ## identical to n_sample_sites_mean (n=1)
      ## Average across bootstraps
      stats$n_sample_sites_total = mean(vapply(
          gf_list, \(gf){
              nrow(gf$X)
          },
          numeric(1)
      ))
      ## mean number of sites across all surveys
      ## for single survey,
      ## identical to n_sample_sites_total (n=1)
      ## Average across bootstraps
      stats$n_sample_sites_mean = mean(vapply(
          gf_list, \(gf){
              nrow(gf$X)
          },
          numeric(1)
      ))
      ## sd number of sites of all surveys
      ## sd across bootstraps, should be 0
      stats$n_sample_sites_sd = sd(vapply(
          gf_list, \(gf){
              nrow(gf$X)
          },
          numeric(1)
      ))

      ## Species R^2
      ## total of all species in single survey
      ## Average across bootstraps
      stats$spec_r2_total = mean(vapply(
          gf_list, \(gf){
              sum(gf$result)
          },
          numeric(1)
      ))
      ## Species R^2
      ## median across all species in survey
      ## Average across bootstraps
      stats$spec_r2_median = mean(vapply(
          gf_list, \(gf){
              median(gf$result)
          },
          numeric(1)
      ))
      ## Species R^2
      ## mean across all species in survey
      ## Average across bootstraps
      stats$spec_r2_mean = mean(vapply(
          gf_list, \(gf){
              mean(gf$result)
          },
          numeric(1)
      ))
      ## Species R^2
      ## sd across all species in survey
      ## Average across bootstraps
      stats$spec_r2_sd = mean(vapply(
          gf_list, \(gf){
              sd(gf$result)
          },
          numeric(1)
      ))
      ## Survey R^2
      ## Survey R^2 of survey
      ## Average across bootstraps
      stats$survey_r2_mean = mean(vapply(
          gf_list, \(gf){
              sum(importance(gf))
          },
          numeric(1)
      ))
      ## Survey R^2
      ## sd across bootstraps
      stats$survey_r2_sd = sd(vapply(
          gf_list, \(gf){
              sum(importance(gf))
          },
          numeric(1)
      ))

      ## Number of surveys
      stats$nsurveys = 1

  } else {
      stop("Unknown GF type in gf_diagnostic_stats.R")
  }


  stats_dt <- data.table::setDT(stats)

  ret <- data.table::data.table(stats_dt, gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)])
  data.table::setcolorder(ret, sort(names(ret)))
  return(ret)

}






gfbootstrap_diagnostic_plots <- function(gfbootstrap_combined,
                                         gfbootstrap_diagnostics,
                                         env_domain_plot,
                                         env_domain_cluster,
                                         env_poly,
                                         marine_map,
                                         env_id_col,
                                         env_biooracle_names,
                                         pred_importance_top,
                                         plot_description,
                                         output_folder) {
  survey_specs <- gfbootstrap_combined[
    ,
    c(
      "env_domain",
      "trophic",
      "survey",
      "depth_cat"
    )
  ]
  survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
  survey_specs <- as.character(survey_specs)
   pl_survey_name <- paste0(c(survey_specs, plot_description),
                                                 collapse = "_")

  pl_file_base <- file.path(output_folder, paste0(survey_specs, collapse = "_"))

  if (inherits(gfbootstrap_combined$gfbootstrap[[1]], "combinedBootstrapGF")) {
    gg_plots <- gfbootstrap::gg_combined_bootstrapGF(gfbootstrap_combined$gfbootstrap[[1]])
    file_names <- vapply(names(gg_plots), function(n, gg_plots, pl_file_base) {
      pl_file <- paste0(c(paste0(c(pl_file_base, n, plot_description), collapse = "_"), ".png"), collapse = "")
      ggsave_wrapper(filename = pl_file, plot = gg_plots[[n]])
      return(pl_file)
    }, character(1), gg_plots = gg_plots, pl_file_base = pl_file_base)
  } else if (inherits(gfbootstrap_combined$gfbootstrap[[1]], "bootstrapGradientForest")) {
    gg_plots <- gfbootstrap::gg_bootstrapGF(gfbootstrap_combined$gfbootstrap[[1]])
    file_names <- vapply(names(gg_plots), function(n, gg_plots, pl_file_base) {
      pl_file <- paste0(c(paste0(c(pl_file_base, n, plot_description), collapse = "_"), ".png"), collapse = "")
      ggsave_wrapper(filename = pl_file, plot = gg_plots[[n]])
      return(pl_file)
    }, character(1), gg_plots = gg_plots, pl_file_base = pl_file_base)
  } else {
    file_names <- paste0(c(paste0(c(pl_file_base, "nofit", plot_description), collapse = "_"), ".png"), collapse = "")
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = file_names, plot = no_plot)
    return(file_names)
  }

  ##

  ## Plot extrapolation scores
  ## At clustering resolution and plotting resolutionx

  file_names <- c(
    file_names,
    gf_diag_plot_helper(gfbootstrap_combined,
                                gfbootstrap_diagnostics,
                                env_domain_cluster,
                                env_poly,
                        marine_map,
                        env_id_col,
                        env_biooracle_names,
                        pred_importance_top,
                        pl_file_base,
                        plot_description,
                        pl_type = "clustering"
                        )
  )

  file_names <- c(
      file_names,
    gf_diag_plot_helper(gfbootstrap_combined,
                        gfbootstrap_diagnostics,
                        env_domain_plot,
                        env_poly,
                        marine_map,
                        env_id_col,
                        env_biooracle_names,
                        pred_importance_top,
                        pl_file_base,
                        plot_description,
                        pl_type = "plotting"
                        )
  )

  return(file_names)

}


gf_diag_plot_helper <- function(gfbootstrap_combined,
                                gfbootstrap_diagnostics,
                                env_domain,
                                env_poly,
                                marine_map,
                                env_id_col,
                                env_biooracle_names,
                                pred_importance_top,
                                pl_file_base,
                                plot_description,
                                pl_type
                                ) {
    s2_used <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)

    imp <- gradientForest::importance(gfbootstrap_combined$gfbootstrap[[1]], sort = TRUE)
  if (pred_importance_top >= 1) {
    imp_preds <- names(imp)[seq.int(1, min(length(imp), pred_importance_top))]
  } else {
    imp_explained <- cumsum(imp) / sum(imp)
    ## Take all predictors below threshold, then one more
    n_preds <- sum(imp_explained < pred_importance_top) + 1
    imp_preds <- names(imp)[seq.int(1, n_preds)]
  }


  env_dom <- env_domain[domain == gfbootstrap_combined$env_domain, data][[1]]

  if (gfbootstrap_combined$depth_cat != "all") {
    env_dom <- env_dom[-MS_bathy_5m >= min(depth_range[[gfbootstrap_combined$depth_cat]]), ]
  }


  ## Predict if we are not in the clustering env domain
  if (nrow(env_dom) == length(gfbootstrap_diagnostics$extrap_score[[1]])) {
    cluster_res_data <- data.table::data.table(
      extrap_score = gfbootstrap_diagnostics[, extrap_score][[1]],
      env_dom[, ..env_id_col]
    )
  } else {
    ## Batch to avoid memory explosion
      mem_max <- as.numeric(Sys.getenv("TENSOR_CPU_MEM_MAX", ""))/10
      n_gf <- length(gfbootstrap_combined$gfbootstrap[[1]]$gf_list)
      n_preds_raw <- length(env_biooracle_names)
      size_doubles <- 8
      size_int <- 4
      pred_obj_size <- n_gf * n_preds_raw * (
          size_doubles * 3 +
          size_int * 2
      )
      mem_per_site <-  pred_obj_size *
    ## generate pred
        (2 +
         ## setkey and pred stats
         1)
      env_dom_batch <- data.table::as.data.table(prepare_batch(mem_max,
        nrow(env_dom),
        2e9,
        mem_per_site,
        max_batch_size = NA
      ))

      extrap_score <- env_dom_batch[,
                                    by = batch_ind,
                                    {
                                      predicted <- predict(
                                        object = gfbootstrap_combined$gfbootstrap[[1]],
                                        newdata = env_dom[.SD$site, env_biooracle_names, with = FALSE],
                                        ## Just take points, and calculate full coefficient matrix from points
                                        type = c("points"),
                                        extrap = NA,
                                        avoid_copy = TRUE
                                      )
                                      data.table::setDT(predicted)
                                      ## Need to average over each gf object created by bootstrapping
                                      ngf <- length(gfbootstrap_combined$gfbootstrap[[1]]$gf_list)
                                      ## Also average over number of predictors
                                      npreds <- length(imp_preds)

                                      list(extrap_score = predicted[,
                                        by = x_row,
                                        list(extrap_score = sum(is.na(y)) / (ngf * npreds))
                                      ][, extrap_score])
                                    }
                                    ]

      cluster_res_data <- data.table::data.table(
                                          extrap_score = extrap_score[, extrap_score],
                                          env_dom[, ..env_id_col]
                                      )

  }


  cluster_res_data[env_domain[domain == gfbootstrap_diagnostics$env_domain[[1]], data][[1]],
    on = c(env_id_col),
    c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))
  ]

  plot_cols <- c(spatial_vars, "extrap_score")

  clust_raster <- terra::rast(
    x = as.matrix(cluster_res_data[, ..plot_cols]),
    type = "xyz",
    crs = "+proj=longlat +datum=WGS84"
  )

  env_poly_local <- env_poly[name == gfbootstrap_diagnostics$env_domain[[1]], data][[1]]

  env_bbox <- sf::st_bbox(env_poly_local,
    crs = "+proj=longlat +datum=WGS84"
  )

  pl_extrap <- tmap::tm_shape(clust_raster, bbox = env_bbox) +
    tmap::tm_raster("extrap_score", palette = "seq", style = "cont", breaks = c(0, 1))

  if (gfbootstrap_diagnostics$env_domain != "aus_eez") {
    pl_extrap <- pl_extrap + tmap::tm_shape(marine_map, bbox = env_bbox) +
      tmap::tm_borders(lwd = 1)
  }
  pl_extrap <- pl_extrap + tmap::tm_shape(env_poly_local, bbox = env_bbox) +
    tmap::tm_borders(lwd = 1)

  pl_file <- paste0(paste0(c(pl_file_base, paste0("extrap", pl_type), plot_description), collapse = "_"), ".png")
  tmap_save_wrapper(tm = pl_extrap, filename = pl_file, scale = 0.8, dpi = 1200)
    sf::sf_use_s2(s2_used)
  return(pl_file)
}
