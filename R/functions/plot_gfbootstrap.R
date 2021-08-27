plot_gfbootstrap <- function(
                             gfbootstrap_caster,
                             gfbootstrap_predicted,
                             all_bio_env,
                             all_bio_long,
                             env_poly,
                            spatial_vars,
                            regrid_resolution,
                            output_folder
                             ) {
  ## As a targets pipeline function that just plots,
  ## it should be type "file" and return a char vec of filenames
  ##
  ## Plot
  ## Clusters around Aus
  ## Clusters around Aus with samples
  ## Sim Mat
  pl_file_base <- file.path(output_folder, gfbootstrap_cluster$surv_full_name[1])
  pl_file <- c(
    no_samp = paste0(pl_file_base, "_clustering_no_samples.png"),
    samp_clipped = paste0(pl_file_base, "_clustering_samples_env_domain.png"),
    samp = paste0(pl_file_base, "_clustering_samples_sample_domain.png"),
    sim_mat = paste0(pl_file_base, "_clustering_sim_mat.png")
  )
  if (is.na(gfbootstrap_caster$best_clust)) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(gfbootstrap_cluster$surv_full_name[1], " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file["no_samp"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp_clipped"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["samp"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["sim_mat"], plot = no_plot)
    return(pl_file)
  }


  pl_no_samp <- plot_clust_poly(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars],
                  gfbootstrap_caster$clust_ind[[1]]$cl,
                  spatial_vars,
                  marine_map,
                  env_poly,
                  regrid_res = regrid_resolution)+
    ggplot2::ggtitle(glue::glue("{depth} depth of {survey} survey studying {trophic}, domain is {domain} with {k} clusters"))

  ggsave_wrapper(filename = pl_file["no_samp"], plot = pl_no_samp)


  # TODO will need to aggregate samples for combined surveys. Waiting until I have a ready run to make it easier

  pl_samp_clipped <- plot_clust_poly(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars],
                  gfbootstrap_caster$clust_ind[[1]]$cl,
                  spatial_vars,
                  marine_map,
                  env_poly,
                  regrid_res = regrid_resolution,
                  samples = all_bio_long$samps[[1]][,..spatial_vars],
                  grids = all_bio_env$wide_taxa_env[[1]][,..spatial_vars]) +
    ggplot2::ggtitle(glue::glue("{depth} depth of {survey} survey studying {trophic}, domain is {domain} with {k} clusters and showing samples in domain"))

  ggsave_wrapper(filename = pl_file["samp_clipped"], plot = pl_samp_clipped)



  pl_samp <- plot_clust_poly(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars],
                  gfbootstrap_caster$clust_ind[[1]]$cl,
                  spatial_vars,
                  marine_map,
                  env_poly,
                  regrid_res = regrid_resolution,
                  samples = all_bio_long$samps[[1]][,..spatial_vars],
                  grids = all_bio_env$wide_taxa_env[[1]][,..spatial_vars],
                  clip_samples = FALSE) +
    ggplot2::ggtitle(glue::glue("{depth} depth of {survey} survey studying {trophic}, domain is {domain} with {k} clusters and showing all samples, including unused"))

  ggsave_wrapper(filename = pl_file["samp"], plot = pl_samp)

  pl_sim_mat <-gfbootstrap::gg_sim_mat(gfbootstrap_predicted$sim_mat[[1]],
                                       cast_ob = gfbootstrap_caster$caster_clust[[1]]$cast_ob[[gfbootstrap_caster$best_clust]],
                                       highlight = TRUE) +
    ggplot2::ggtitle(glue::glue("Similarity matrix for {depth} depth of {survey} survey studying {trophic}, domain is {domain} with {k} clusters"))

  ggsave_wrapper(filename = pl_file["sim_mat"], plot = pl_sim_mat)


  return(pl_file)
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

  if (clip_samples | is.null(samples)){
    env_bbox <-  sf::st_bbox(env_poly,
                             crs = "+proj=longlat +datum=WGS84")
  } else {
    env_bbox <- sf::st_bbox(c(xmin = min(samples[, spatial_vars[1]]),
                                 xmax = max(samples[, spatial_vars[1]]),
                                 ymin = min(samples[, spatial_vars[2]]),
                                 ymax = max(samples[, spatial_vars[2]]),
                              crs = "+proj=longlat +datum=WGS84"
                                 )
                            )
  }


  target_grid <- terra::rast(x = env_poly,
                                     resolution = regrid_res,
#                             extent =
                                     crs = "+proj=longlat +datum=WGS84")

  clust_raster <- terra::rast(
            x = sites[, spatial_vars],
            y = target_grid,
            field = as.numeric(clustering)
            )

  names(clust_raster) <- c("clustering")
  clust_multipoly <- sf::st_as_sf(raster::rasterToPolygons(clust_raster, dissolve = TRUE),
                         crs = "+proj=longlat +datum=WGS84")

  sf::st_crs(clust_multipoly) <- "+proj=longlat +datum=WGS84"
  pl <- ggplot2::ggplot(clust_multipoly, mapping = ggplot2::aes(fill = as.factor(clustering))) +
    ggplot2::geom_sf() +
  ggplot2::scale_fill_manual(values = rainbow(max(clustering))) +
  ggplot2::labs(fill = "cluster") +
  ggplot2::geom_sf(data = marine_map, inherit.aes = FALSE, color = "black", fill= NA) +
   ggplot2::coord_sf(xlim = c(env_bbox$xmin, env_bbox$xmax), ylim = c(env_bbox$ymin, env_bbox$ymax)) +
  ggthemes::theme_tufte()

  if(labels) {
    pl <- pl +
      ggplot2::geom_sf_label(ggplot2::aes(label= clustering), fill = "white")
  }
  if(!is.null(samples)){
    if(class(samples)[1] == "list" & length(samples) == 1){
      samples <- samples[[1]]
     }
    if(clip_samples){
      keep <- samples[, spatial_vars[1]]  >= env_bbox$xmin &
        samples[, spatial_vars[1]]  <= env_bbox$xmax &
        samples[, spatial_vars[2]]  >= env_bbox$ymin &
        samples[, spatial_vars[2]]  <= env_bbox$ymax
      samples <- samples[keep,]
    }
    pl <- pl +
  ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), shape = ".", colour = "dimgray", data = samples[,spatial_vars], inherit.aes = FALSE)
  }
  if(!is.null(grids)){
    if(class(grids)[1] == "list" & length(grids) == 1){
      grids <- grids[[1]]
     }
    pl <- pl +
  ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), data = grids[,spatial_vars], shape = "o", colour = "dimgray", inherit.aes = FALSE)
  }

  return(pl)

}
