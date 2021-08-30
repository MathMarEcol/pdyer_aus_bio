plot_gfbootstrap <- function(
                             gfbootstrap_caster,
                             gfbootstrap_predicted,
                             all_bio_env,
                             all_bio_long,
                             env_poly,
                            spatial_vars,
                            regrid_resolution,
                            marine_map,
                            plot_clust_labels,
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
  pl_file_base <- file.path(output_folder, gfbootstrap_caster$surv_full_name[1])
  pl_file <- c(
    no_samp = paste0(pl_file_base, "_clustering_no_samples.png"),
    samp_clipped = paste0(pl_file_base, "_clustering_samples_env_domain.png"),
    samp = paste0(pl_file_base, "_clustering_samples_sample_domain.png"),
    sim_mat = paste0(pl_file_base, "_clustering_sim_mat.png"),
    sim_mat_hist = paste0(pl_file_base, "_clustering_sim_mat_hist.png")
  )
  if (is.na(gfbootstrap_caster$best_clust)) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(gfbootstrap_caster$surv_full_name[1], " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file["no_samp"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp_clipped"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["sim_mat"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["sim_mat_hist"], plot = no_plot)
    return(pl_file)
  }

  k <- gfbootstrap_caster$caster_clust[[1]]$k[gfbootstrap_caster$best_clust]

  pl_no_samp <- plot_clust_poly(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars],
                  gfbootstrap_caster$clust_ind[[1]]$cl,
                  spatial_vars,
                  marine_map,
                  env_poly[name == gfbootstrap_caster$env_domain, data][[1]],
                  regrid_res = regrid_resolution,
                  labels = plot_clust_labels)+
    tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_caster,
                                                 "Clustering for depth [{depth_cat}] in survey [{survey}]\n",
                                                 "studying trophic level [{trophic}], domain is {env_domain}\n",
                                                 "with {k} clusters"),
                    main.title.size = 0.5)

  ## ggsave_wrapper(filename = pl_file["no_samp"], plot = pl_no_samp)
  tmap_save_wrapper(tm = pl_no_samp, filename = pl_file["no_samp"], scale = 0.1, dpi = 1200)

  # TODO will need to aggregate samples for combined surveys. Waiting until I have a ready run to make it easier

  grouping_vars <- c("trophic", "survey", "depth_cat", "env_domain")
  use_vars <- gfbootstrap_caster[, ..grouping_vars]
  use_vars <- grouping_vars[use_vars != "all"]
  bio_env_merge <- all_bio_env[gfbootstrap_caster, on = use_vars]
  fit_grids <- unique(data.table::rbindlist(
                              lapply(bio_env_merge$wide_taxa_env,
                                    function(x, spatial_vars){
                                      x[,..spatial_vars]
                                    }, spatial_vars=spatial_vars
                                    )
                            ))
  fit_grids <- fit_grids[complete.cases(fit_grids)]
  ## fit_grids <- sf::st_as_sf(fit_grids,
  ##                        coords = spatial_vars,
  ##                            crs = "+proj=longlat +datum=WGS84")
  use_vars <- use_vars[use_vars != "env_domain"]
  bio_merge <- all_bio_long[gfbootstrap_caster, on = use_vars]
  fit_samples <- unique(data.table::rbindlist(
                              lapply(bio_merge$samps,
                                    function(x, spatial_vars){
                                      x[,..spatial_vars]
                                    }, spatial_vars=spatial_vars
                                    )
                            ))
  fit_samples <- fit_samples[complete.cases(fit_samples)]
  ## fit_samples <- sf::st_as_sf(fit_samples,
  ##                        coords = spatial_vars,
  ##                            crs = "+proj=longlat +datum=WGS84")

  pl_samp_clipped <- plot_clust_poly(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars],
                  gfbootstrap_caster$clust_ind[[1]]$cl,
                  spatial_vars,
                  marine_map,
                  env_poly[name == gfbootstrap_caster$env_domain, data][[1]],
                  regrid_res = regrid_resolution,
                                    labels = plot_clust_labels,
                  samples = fit_samples,
                  grids = fit_grids)+
    tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_caster,
                                                 "Clustering showing samples in domain for depth [{depth_cat}]\n",
                                                 "n survey [{survey}] studying trophic level [{trophic}],\n",
                                                 "domain is {env_domain} with {k} clusters"),
                    main.title.size = 0.5)

  ##ggsave_wrapper(filename = pl_file["samp_clipped"], plot = pl_samp_clipped)
  tmap_save_wrapper(tm = pl_samp_clipped, filename = pl_file["samp_clipped"], scale = 0.1, dpi = 1200)



  pl_samp <- plot_clust_poly(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars],
                  gfbootstrap_caster$clust_ind[[1]]$cl,
                  spatial_vars,
                  marine_map,
                  env_poly[name == gfbootstrap_caster$env_domain, data][[1]],
                  regrid_res = regrid_resolution,
                                    labels = plot_clust_labels,
                  samples = fit_samples,
                  grids = fit_grids,
                  clip_samples = FALSE) +
    tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_caster,
                                                 "Clustering showing all samples, including unused, for depth [{depth_cat}]\n",
                                                 "in survey [{survey}] studying trophic level [{trophic}],\n",
                                                 "domain is {env_domain} with {k} clusters "),
                    main.title.size = 0.5)

  ## ggsave_wrapper(filename = pl_file["samp"], plot = pl_samp)
  tmap_save_wrapper(tm = pl_samp, filename = pl_file["samp"], scale = 0.1, dpi = 1200)

  pl_sim_mat <- castcluster::gg_sim_mat(gfbootstrap_predicted$sim_mat[[1]][[1]],
                                       cast_ob = gfbootstrap_caster$caster_clust[[1]]$cast_ob[[gfbootstrap_caster$best_clust]],
                                       aff_thres = gfbootstrap_caster$caster_clust[[1]]$aff_thres[gfbootstrap_caster$best_clust],
                                       highlight = TRUE) +
    ggplot2::ggtitle(glue::glue_data(gfbootstrap_caster, "Similarity matrix for depth [{depth_cat}] in survey [{survey}] studying trophic level [{trophic}], domain is {env_domain} with {k} clusters"))

  ggsave_wrapper(filename = pl_file["sim_mat"], plot = pl_sim_mat)


  pl_sim_mat_hist <- ggplot2::ggplot(data.frame(x = as.vector(strip_diag(gfbootstrap_predicted$sim_mat[[1]][[1]]))),
                                     ggplot2::aes(x = x)) +
    geom_histogram(na.rm = TRUE) +
    ggplot2::ggtitle(glue::glue_data(gfbootstrap_caster, "Histogram of similarities for depth [{depth_cat}] in survey [{survey}] studying trophic level [{trophic}], domain is {env_domain} with {k} clusters"))

  ggsave_wrapper(filename = pl_file["sim_mat_hist"], plot = pl_sim_mat_hist)
  return(pl_file)
}

strip_diag <- function(x) {
  diag(x) <- NA
  return(x)
}

plot_clust_poly <- function(sites,
                            clustering,
                            spatial_vars,
                            marine_map,
                            env_poly,
                            regrid_res,
                            labels = TRUE,
                            samples = NULL,
                            grids = NULL,
                            clip_samples = TRUE){

  sites<- as.data.table(sites)
  samples<- as.data.table(samples)
  grids <- as.data.table(grids)
  if (clip_samples | is.null(samples)){
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


##   target_grid <- terra::rast(x = env_bbox,
##                                      resolution = regrid_res,
## #                             extent =
##                                      crs = "+proj=longlat +datum=WGS84")

  clust_raster <- terra::rast(
            x = as.matrix(data.frame(sites[, ..spatial_vars], clustering)),
            type = "xyz",
            crs = "+proj=longlat +datum=WGS84")

  names(clust_raster) <- c("clustering")
  clust_multipoly <- terra::as.polygons(clust_raster)
  clust_poly_sf <- sf::st_as_sf(clust_multipoly)
  ## tmap does not like large numbers of cluster categories
  ## Since I am manually setting colours, I don't need to
  ## use a factor.
  ## clust_poly_sf$clustering <- as.factor(  clust_poly_sf$clustering)


pl_tm <-   tm_shape(clust_poly_sf, bbox = env_bbox) +
    tm_polygons(col = "clustering",
                ## style="cat",
                palette = rainbow(max(clustering))) +
  tm_layout(legend.show = FALSE)


  ## pl <- ggplot2::ggplot(clust_poly_sf, mapping = ggplot2::aes(fill = as.factor(clustering))) +
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
    if(class(samples)[1] == "list" & length(samples) == 1){
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
    if(class(grids)[1] == "list" & length(grids) == 1){
      grids <- grids[[1]]
     }
  ##   pl <- pl +
  ## ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), data = grids[,spatial_vars], shape = "o", colour = "dimgray", inherit.aes = FALSE)
    grids <- sf::st_as_sf(grids,
                              coords = spatial_vars,
                              crs = "+proj=longlat +datum=WGS84")

  pl_tm <- pl_tm +
    tm_shape(grids) +
    tm_squares(col = "dimgray")
  }

  ## marine_map throws warning "bounding box has potentially an invalid value
  ## range for longlat data"
    pl_tm <- pl_tm + tm_shape(marine_map, bbox = env_bbox) +
    tm_borders(lwd = 2)

  ## return(pl)
  return(pl_tm)

}
