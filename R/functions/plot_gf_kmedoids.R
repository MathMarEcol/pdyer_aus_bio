# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
plot_gf_kmedoids <- function(gf_cluster_kmedoids,
                             gf_kmedoid_polygons,
                             gf_predicted,
                             cluster_fixed_k,
                             all_bio_env,
                             all_bio_long,
                             env_poly,
                             spatial_vars,
                             marine_map,
                             plot_description,
                             output_folder) {

  survey_specs <- gf_cluster_kmedoids[
    ,
    c(
        "env_domain", "env_year", "env_pathway",
        "res_gf",
        "res_clust",
        "trophic",
        "survey",
        "depth_cat",
        "clust_method"
    )
  ]
  survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
  survey_specs <- as.character(survey_specs)

  pl_survey_name <- paste0(c(survey_specs, plot_description),
                                                 collapse = "_")
  pl_file_base <- file.path(output_folder, pl_survey_name)
  pl_file <- c(
    no_samp = paste0(pl_file_base, "_clustering_no_samples.pdf"),
    samp_clipped = paste0(pl_file_base, "_clustering_samples_env_domain.pdf"),
    samp = paste0(pl_file_base, "_clustering_samples_sample_domain.pdf")
  )

  if (all(is.na(gf_cluster_kmedoids$clust_ind))) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file["no_samp"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp_clipped"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp"], plot = no_plot)
    return(pl_file)
  }

    imp_preds <- gf_predicted$imp_preds[[1]]
  pred_string <- paste(
      sapply(
        split(imp_preds, rep(seq.int(1,length(imp_preds)), each = 5, length.out = length(imp_preds))),
        function(x){paste(x, collapse = ", ")}
      ),
      collapse = "\n"
  )
    if (sf::sf_use_s2() &&
      any(c(
        !is.na(gf_kmedoid_polygons$polygons) &&
          !all(sf::st_is_valid(gf_kmedoid_polygons$polygons[[1]])),
        !is.na(gf_kmedoid_polygons$polygons_no_clust) &&
          !all(sf::st_is_valid(gf_kmedoid_polygons$polygons_no_clust[[1]]))
      ))) {
      sf::sf_use_s2(FALSE)
      s2_disabled <- TRUE
    } else {
      s2_disabled <- FALSE
    }

  ## plot_clust_poly is defined in plot_gfbootstrap.R
    pl_no_samp <- plot_clust_poly(cluster_polygons = gf_kmedoid_polygons$polygons[[1]],
                                gf_kmedoid_polygons$polygons_no_clust[[1]],
                                spatial_vars = spatial_vars,
                  marine_map = env_poly[name == "aus_eez", data][[1]],
                  plot_map = gf_cluster_kmedoids$env_domain != "aus_eez",
                  env_poly = env_poly[name == gf_cluster_kmedoids$env_domain, data][[1]],
                  labels = plot_clust_labels)+
    tmap::tm_layout(main.title = glue::glue_data(gf_cluster_kmedoids,
                                                 "{cluster_fixed_k} clusters"),
                    main.title.size = 0.5)

  ## ggsave_wrapper(filename = pl_file["no_samp"], plot = pl_no_samp)
  tmap_save_wrapper(tm = pl_no_samp, filename = pl_file["no_samp"])

  if (gf_predicted$survey %in% c("envonly", "envmeanonly")) {
    return(pl_file["no_samp"])
  }

  # TODO will need to aggregate samples for combined surveys. Waiting until I have a ready run to make it easier

  grouping_vars <- c("trophic", "survey", "depth_cat", "env_domain", "env_year", "env_pathway")
  custom_grouping_vars <- c("trophic", "survey", "depth_cat")
  ## Check for custom combination
  custom_group_matches <- vapply(custom_combinations, function(x) {
    all(x$descriptions[, ..custom_grouping_vars] == gf_cluster_kmedoids[, ..custom_grouping_vars])
  }, logical(1))
  if (sum(custom_group_matches > 1)) {
    stop("plot_gfbootstrap.R: Multiple custom groups have matched")
  }


  if (any(custom_group_matches)) {
    ## This line is only reached if custom_gorup_matches has exactly one true entry.
    custom_group_matches <- which(custom_group_matches)
    ## Using a custom combination
    use_vars <- custom_combinations[custom_group_matches][[1]]$descriptions[, ..custom_grouping_vars]
    use_vars <- grouping_vars[use_vars != "all"]
    match_table <- data.table::data.table(
      custom_combinations[custom_group_matches][[1]]$matches,
      gf_cluster_kmedoids[, .(env_domain, env_year, env_pathway, res_gf)]
    )

  } else {
    ## Using a "default" combination
    use_vars <- gf_cluster_kmedoids[, ..grouping_vars]
    use_vars <- grouping_vars[use_vars != "all"]
    match_table <- gf_cluster_kmedoids[, ..use_vars]
  }

  bio_env_merge <- all_bio_env[match_table, on = use_vars]
  fit_grids <- unique(data.table::rbindlist(
    lapply(bio_env_merge$wide_taxa_env,
           function(x, spatial_vars) {
             if (all(class(x) == c("data.table", "data.frame"))) {
               return(x[, ..spatial_vars])
             } else {
               if (length(x) == 1 & is.na(x)) {
                 return(NULL)
               } else {
                 stop("Unexpected object while getting grid locations")
               }
             }
           },
           spatial_vars = spatial_vars
           )
  ))

  fit_grids <- fit_grids[complete.cases(fit_grids)]
  ## fit_grids <- sf::st_as_sf(fit_grids,
  ##                        coords = spatial_vars,
  ##                            crs = "+proj=longlat +datum=WGS84")
  use_vars <- use_vars[!(use_vars %in% c("env_domain", "env_year", "env_pathway", "res_gf"))]
  if(length(use_vars) == 0) {
    bio_merge <- all_bio_long
  } else {
    bio_merge <- all_bio_long[match_table, on = use_vars]
  }
  fit_samples <- unique(data.table::rbindlist(
                              lapply(bio_merge$samps,
                                    function(x, spatial_vars){
                                      if(all(class(x) == c("data.table",  "data.frame"))) {
                                        return(x[,..spatial_vars])
                                      } else {
                                        if(length(x) == 1 & is.na(x)) {
                                          return(NULL)
                                        } else {
                                          stop("Unexpected object while getting sample locations")
                                        }
                                        return(NULL)
                                      }
                                    }, spatial_vars = spatial_vars
                                    )
                            ))
  fit_samples <- fit_samples[complete.cases(fit_samples)]

  pl_samp_clipped <- plot_clust_poly(gf_kmedoid_polygons$polygons[[1]],
                                     gf_kmedoid_polygons$polygons_no_clust[[1]],
                                     spatial_vars = spatial_vars,
                  marine_map = env_poly[name == "aus_eez", data][[1]],
                  plot_map = gf_cluster_kmedoids$env_domain != "aus_eez",
                  env_poly = env_poly[name == gf_cluster_kmedoids$env_domain, data][[1]],
                                    labels = plot_clust_labels,
                  samples = fit_samples,
                  grids = fit_grids)+
    tmap::tm_layout(main.title = glue::glue_data(gf_cluster_kmedoids,
                                                 "{cluster_fixed_k} clusters"),
                    main.title.size = 0.5)

  ##ggsave_wrapper(filename = pl_file["samp_clipped"], plot = pl_samp_clipped)
  tmap_save_wrapper(tm = pl_samp_clipped, filename = pl_file["samp_clipped"])



    pl_samp <- plot_clust_poly(gf_kmedoid_polygons$polygons[[1]],
                               gf_kmedoid_polygons$polygons_no_clust[[1]],
                  spatial_vars = spatial_vars,
                  marine_map = env_poly[name == "aus_eez", data][[1]],
                  plot_map = gf_cluster_kmedoids$env_domain != "aus_eez",
                  env_poly = env_poly[name == gf_cluster_kmedoids$env_domain, data][[1]],
                  labels = plot_clust_labels,
                  samples = fit_samples,
                  grids = fit_grids,
                  clip_samples = FALSE) +
    tmap::tm_layout(main.title = glue::glue_data(gf_cluster_kmedoids,
                                                 "{cluster_fixed_k} clusters"),
                    main.title.size = 0.5)

  ## ggsave_wrapper(filename = pl_file["samp"], plot = pl_samp)
  tmap_save_wrapper(tm = pl_samp, filename = pl_file["samp"])

  ## Need plot_clust_poly


  if(s2_disabled) sf::sf_use_s2(TRUE)
  return(pl_file)

}
