plot_gfbootstrap <- function(
                             gfbootstrap_cluster,
                             gfbootstrap_polygons,
                             gfbootstrap_predicted,
                             all_bio_env,
                             all_bio_long,
                             env_poly,
                            spatial_vars,
                            marine_map,
                            plot_clust_labels,
                            plot_description,
                            plot_sim_mat,
                            output_folder
                             ) {
  ## As a targets pipeline function that just plots,
  ## it should be type "file" and return a char vec of filenames
  ##
  ## Plot
  ## Clusters around Aus
  ## Clusters around Aus with samples
  ## Sim Mat
  ## Sim Mat histogram
  survey_specs <- gfbootstrap_polygons[
    ,
    c(
      "env_domain", "env_year", "env_pathway", "res_gf", "res_clust",
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
    no_samp = paste0(pl_file_base, "_clustering_no_samples.png"),
    samp_clipped = paste0(pl_file_base, "_clustering_samples_env_domain.png"),
    samp = paste0(pl_file_base, "_clustering_samples_sample_domain.png")
  )
  if (plot_sim_mat) {
    pl_file <- c(pl_file,
                 sim_mat = paste0(pl_file_base, "_clustering_sim_mat.png"),
                 sim_mat_ungrouped = paste0(pl_file_base, "_clustering_sim_mat_ungrouped.png"),
                 sim_mat_hist = paste0(pl_file_base, "_clustering_sim_mat_hist.png")
    )
  }
  if (is.na(gfbootstrap_cluster$best_clust)) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file["no_samp"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp_clipped"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp"], plot = no_plot)
    if (plot_sim_mat) {
        ggsave_wrapper(filename = pl_file["sim_mat"], plot = no_plot)
        ggsave_wrapper(filename = pl_file["sim_mat_ungrouped"], plot = no_plot)
        ggsave_wrapper(filename = pl_file["sim_mat_hist"], plot = no_plot)
    }
    return(pl_file)
  }

  k <- gfbootstrap_cluster$clust[[1]]$k[gfbootstrap_cluster$best_clust]
  imp_preds <- gfbootstrap_predicted$imp_preds[[1]]
  pred_string <- paste(
      sapply(
        split(imp_preds, rep(seq.int(1,length(imp_preds)), each = 5, length.out = length(imp_preds))),
        function(x){paste(x, collapse = ", ")}
      ),
      collapse = "\n"
  )


    if (sf::sf_use_s2() &&
      any(c(
        !is.na(gfbootstrap_polygons$polygons) &&
          !all(sf::st_is_valid(gfbootstrap_polygons$polygons[[1]])),
        !is.na(gfbootstrap_polygons$polygons_no_clust) &&
          !all(sf::st_is_valid(gfbootstrap_polygons$polygons_no_clust[[1]]))
      ))) {
      sf::sf_use_s2(FALSE)
      s2_disabled <- TRUE
    } else {
      s2_disabled <- FALSE
    }

  pl_no_samp <- plot_clust_poly(cluster_polygons = gfbootstrap_polygons$polygons[[1]],
                                gfbootstrap_polygons$polygons_no_clust[[1]],
                                spatial_vars = spatial_vars,
                  marine_map = env_poly[name == "aus_eez", data][[1]],
                  plot_map = gfbootstrap_cluster$env_domain != "aus_eez",
                  env_poly = env_poly[name == gfbootstrap_cluster$env_domain, data][[1]],
                  labels = plot_clust_labels)+
    tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_cluster,
                                                 "Clustering for depth [{depth_cat}] in survey [{survey}]\n",
                                                 "studying trophic level [{trophic}], domain is {env_domain} at res {res_clust}.\n",
                                                 "Clustered with {clust_method} which found {k} clusters. Predictors used:\n",
                                                 "{pred_string}"),
                    main.title.size = 0.5)

  ## ggsave_wrapper(filename = pl_file["no_samp"], plot = pl_no_samp)
  tmap_save_wrapper(tm = pl_no_samp, filename = pl_file["no_samp"], scale = 0.1, dpi = 1200)

  # TODO will need to aggregate samples for combined surveys. Waiting until I have a ready run to make it easier

  grouping_vars <- c("trophic", "survey", "depth_cat", "env_domain", "env_year", "env_pathway", "res_gf")
  custom_grouping_vars <- c("trophic", "survey", "depth_cat")
  ## Check for custom combination
  custom_group_matches <- vapply(custom_combinations, function(x) {
    all(x$descriptions[, ..custom_grouping_vars] == gfbootstrap_cluster[, ..custom_grouping_vars])
  }, logical(1))
  if (sum(custom_group_matches > 1)) {
    stop("plot_gfbootstrap.R: Multiple custom groups have matched")
  }

  if (any(custom_group_matches)) {
    ## Using a custom combination
    use_vars <- custom_combinations[custom_group_matches][[1]]$descriptions[, ..custom_grouping_vars]
    use_vars <- grouping_vars[use_vars != "all"]
    match_table <- data.table::data.table(
      custom_combinations[custom_group_matches][[1]]$matches,
      gfbootstrap_cluster[, .(env_domain, env_year, env_pathway, res_gf)]
    )

  } else {
    ## Using a "default" combination
    use_vars <- gfbootstrap_cluster[, ..grouping_vars]
    use_vars <- grouping_vars[use_vars != "all"]
    match_table <- gfbootstrap_cluster[, ..use_vars]
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

  pl_samp_clipped <- plot_clust_poly(gfbootstrap_polygons$polygons[[1]],
                                     gfbootstrap_polygons$polygons_no_clust[[1]],
                                     spatial_vars = spatial_vars,
                  marine_map = env_poly[name == "aus_eez", data][[1]],
                  plot_map = gfbootstrap_cluster$env_domain != "aus_eez",
                  env_poly = env_poly[name == gfbootstrap_cluster$env_domain, data][[1]],
                  labels = plot_clust_labels,
                  samples = fit_samples,
                  grids = fit_grids)+
    tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_cluster,
                                                 "Clustering showing samples in domain for depth [{depth_cat}]\n",
                                                 "in survey [{survey}] studying trophic level [{trophic}],\n",
                                                 "domain is {env_domain} at res {res_clust}. Clustered with {clust_method} which found {k} clusters. Predictors used:\n",
                                                 "{pred_string}"),
                    main.title.size = 0.5)

  ##ggsave_wrapper(filename = pl_file["samp_clipped"], plot = pl_samp_clipped)
  tmap_save_wrapper(tm = pl_samp_clipped, filename = pl_file["samp_clipped"], scale = 0.1, dpi = 1200)



    pl_samp <- plot_clust_poly(gfbootstrap_polygons$polygons[[1]],
                               gfbootstrap_polygons$polygons_no_clust[[1]],
                  spatial_vars = spatial_vars,
                  marine_map = env_poly[name == "aus_eez", data][[1]],
                  plot_map = gfbootstrap_cluster$env_domain != "aus_eez",
                  env_poly = env_poly[name == gfbootstrap_cluster$env_domain, data][[1]],
                  labels = plot_clust_labels,
                  samples = fit_samples,
                  grids = fit_grids,
                  clip_samples = FALSE) +
    tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_cluster,
                                                 "Clustering showing all samples, including unused, for depth [{depth_cat}]\n",
                                                 "in survey [{survey}] studying trophic level [{trophic}],\n",
                                                 "domain is {env_domain} at res {res_clust}. Clustered with {clust_method} which found {k} clusters. Predictors used:\n",
                                                 "{pred_string}"),
                    main.title.size = 0.5)

  ## ggsave_wrapper(filename = pl_file["samp"], plot = pl_samp)
  tmap_save_wrapper(tm = pl_samp, filename = pl_file["samp"], scale = 0.1, dpi = 1200)

    if(plot_sim_mat) {
        sim_mat <- gfbootstrap_predicted$sim_mat[[1]][[1]]
        is_caster <- grepl("^caster", gfbootstrap_cluster$clust_method) &&  (gfbootstrap_cluster$best_clust > 1)
        aff_thres_local <- if(is_caster){
                               gfbootstrap_cluster$clust[[1]]$aff_thres[gfbootstrap_cluster$best_clust]
                           } else {
                               NULL
                           }
        pl_sim_mat <- castcluster::gg_sim_mat(sim_mat,
                                              cast_ob = gfbootstrap_cluster$best_clust_ob[[1]],
                                              aff_thres = aff_thres_local,
                                              highlight = TRUE,
                                              sort_within_clust = is_caster,
                                              sort_among_clust = is_caster) +
            ggplot2::ggtitle(glue::glue_data(gfbootstrap_cluster, "Similarity matrix for depth [{depth_cat}] in survey [{survey}] studying trophic level [{trophic}], domain is {env_domain} at res {res_clust}. Clustered with {clust_method} which found {k} clusters."))

        ggsave_wrapper(filename = pl_file["sim_mat"], plot = pl_sim_mat)

        pl_sim_mat_ungrouped <- castcluster::gg_sim_mat(sim_mat) +
            ggplot2::ggtitle(glue::glue_data(gfbootstrap_cluster, "Similarity matrix for depth [{depth_cat}] in survey [{survey}] studying trophic level [{trophic}], domain is {env_domain} at res {res_clust}. Clustered with {clust_method} which found {k} clusters."))

        ggsave_wrapper(filename = pl_file["sim_mat_ungrouped"], plot = pl_sim_mat_ungrouped)


        pl_sim_mat_hist <- ggplot2::ggplot(data.frame(x = as.vector(strip_diag(sim_mat))),
                                           ggplot2::aes(x = x)) +
            geom_histogram(na.rm = TRUE) +
            ggplot2::ggtitle(glue::glue_data(gfbootstrap_cluster, "Histogram of similarities for depth [{depth_cat}] in survey [{survey}] studying trophic level [{trophic}], domain is {env_domain} at res {res_clust}. Clustered with {clust_method} which found {k} clusters"))

        ggsave_wrapper(filename = pl_file["sim_mat_hist"], plot = pl_sim_mat_hist)
    }

    if(s2_disabled) sf::sf_use_s2(TRUE)
  return(pl_file)
}

strip_diag <- function(x) {
  diag(x) <- NA
  return(x)
}

plot_clust_poly <- function(cluster_polygons,
                            no_cluster_polygons,
                            spatial_vars,
                            marine_map,
                            plot_map = FALSE,
                            env_poly,
                            labels = TRUE,
                            samples = NULL,
                            grids = NULL,
                            clip_samples = TRUE){

  if(!is.null(samples)){
    samples <- as.data.table(samples)
  }
  if(!is.null(grids)){
    grids <- as.data.table(grids)
  }
  if (clip_samples || is.null(samples)){
    env_bbox <-  sf::st_bbox(env_poly,
                             crs = "+proj=longlat +datum=WGS84")
  } else {
      sv1 <- spatial_vars[1]
      sv2 <- spatial_vars[2]
    env_bbox <- sf::st_bbox(c(xmin = min(samples[, ..sv1]),
                                 xmax = max(samples[, ..sv1]),
                                 ymin = min(samples[, ..sv2]),
                                 ymax = max(samples[, ..sv2])),
                              crs = "+proj=longlat +datum=WGS84"
                            )
  }


  ## tmap does not like large numbers of cluster categories
  ## Since I am manually setting colours, I don't need to
  ## use a factor.
  ## cluster_polygons$clustering <- as.factor(  cluster_polygons$clustering)
  nclust <- max(cluster_polygons$clustering)
  ## remove any values between 35% and 60% of the rainbow spectrum
  rainbow_cut <- if (nclust > 1) {
    green_cut <- seq(0, 1, 1 / (nclust - 1))
    green_cut <- green_cut < 0.3 | green_cut > 0.45
    rainbow(nclust)[green_cut]
  } else {
    rainbow(1)
  }

pl_tm <-   tm_shape(cluster_polygons, bbox = env_bbox) +
    tm_polygons(col = "clustering",
                style="cont",
                palette = rainbow_cut) +
  tm_layout(legend.show = FALSE)

    if(!all(is.na(no_cluster_polygons))) {
        pl_tm <- pl_tm + tm_shape(no_cluster_polygons, bbox = env_bbox) +
            tm_polygons(col = "grey")
    }
  ## pl <- ggplot2::ggplot(cluster_polygons, mapping = ggplot2::aes(fill = as.factor(clustering))) +
  ##   ggplot2::geom_sf() +
  ## ggplot2::scale_fill_manual(values = rainbow(max(clustering)), guide = FALSE) +
  ## ggplot2::labs(fill = "cluster") +
  ## ggplot2::geom_sf(data = marine_map, inherit.aes = FALSE, color = "black", fill= NA) +
  ##  ggplot2::coord_sf(xlim = c(env_bbox$xmin, env_bbox$xmax), ylim = c(env_bbox$ymin, env_bbox$ymax)) +
  ## ggthemes::theme_tufte()

  if(labels) {
    ## pl <- pl +
    ##   ggplot2::geom_sf_label(ggplot2::aes(label= clustering), fill = "white")
    pl_tm <- pl_tm + tm_text(text = "clustering")

  }
  if(!is.null(samples)){
    if(class(samples)[1] == "list" && length(samples) == 1){
      samples <- samples[[1]]
     }
    if(clip_samples){
      sv1 <- spatial_vars[1]
      sv2 <- spatial_vars[2]
      keep <- samples[, ..sv1]  >= env_bbox$xmin &
        samples[, ..sv1]  <= env_bbox$xmax &
        samples[, ..sv2]  >= env_bbox$ymin &
        samples[, ..sv2]  <= env_bbox$ymax
      samples <- samples[as.vector(keep),]
    }
    samples <- sf::st_as_sf(samples,
                            coords = spatial_vars,
                            crs = "+proj=longlat +datum=WGS84")
  ##   pl <- pl +
  ## ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), shape = ".", colour = "dimgray", data = samples[,spatial_vars], inherit.aes = FALSE)
  pl_tm <- pl_tm +
    tm_shape(samples) +
    tm_dots(col = "lightgray", shape = 01, size = 0.2)
  }
  if(!is.null(grids)){
    if(class(grids)[1] == "list" && length(grids) == 1){
      grids <- grids[[1]]
     }
  ##   pl <- pl +
  ## ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), data = grids[,spatial_vars], shape = "o", colour = "dimgray", inherit.aes = FALSE)
    grids <- sf::st_as_sf(grids,
                              coords = spatial_vars,
                              crs = "+proj=longlat +datum=WGS84")

  pl_tm <- pl_tm +
    tm_shape(grids) +
    tm_symbols(col = "dimgray", border.lwd = NA, alpha = 0.4, size = 0.4, shape = 23)
  }
  if(plot_map){
    pl_tm <- pl_tm + tm_shape(marine_map, bbox = env_bbox) +
      tm_borders(lwd = 2)
  }
  ## marine_map throws warning "bounding box has potentially an invalid value
  ## range for longlat data"
    pl_tm <- pl_tm + tm_shape(env_poly, bbox = env_bbox) +
    tm_borders(lwd = 2)

  ## return(pl)
  return(pl_tm)

}
