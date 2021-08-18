plot_gfbootstrap <- function(
                             gfbootstrap_caster,
                             env_poly,
                            spatial_vars,
                            regrid_resolution
                             ) {
  ## Plot
  ## Clusters around Aus
  ## Clusters around Aus with samples
  ## Sim Mat

  pl_file_base <- file.path("..","..", "outputs", gfbootstrap_clusterd$surv_full_name[1])

  plot_clust_poly(env_round[env_round$lon %% env_subset_val == 0 & env_round$lat %% env_subset_val == 0, spatial_vars],
                  zooplank_cast_sub_clust_ind$cl,
                  spatial_vars,
                  marine_map,
                  env_poly,
                  regrid_res = env_subset_val)

  gfbootstrap::gg_sim_mat(p_mat_diag_cov_sub, cast_ob = zooplank_cast_sub$cast_ob[[which.max(zooplank_cast_sub$gamma)]], highlight = TRUE)
  pl <- ggplot2::ggplot(mapping = ggplot2::aes(x = clust_ind$lon, y = clust_ind$lat, fill = as.factor(clust_ind$cl))) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual( values = rainbow(length(unique(clust_ind$cl))))

  ggsave_wrapper(filename = "./demo_clust_aus_eez_zoo_epi_combined.png", plot= pl,
                           )


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
