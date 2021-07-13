#' Pull in env data, process, and shape to
#' domain of study

load_env_domain <- function(
                            biooracle_folder,
                            env_vars,
                            env_modes,
                            env_poly,
                            max_depth,
                            regrid_resolution,
                            spatial_vars,
                            bio_oracle_str_template = "BO2_%s%s_ss",
                            env_limits_sd,
                            env_offset,
                            env_id_col
                            ) {

  env_pairs <- data.table::as.data.table(
                             merge.data.frame(env_vars, env_modes, all = TRUE)
                           )

  env_bio_oracle_names <- apply(env_pairs[x != "depth"], 1,
                                function(x) {
                                  sprintf(bio_oracle_str_template, x[1], x[2])
                                })
  ## Add bathymetry separately
  env_bio_oracle_names <- c(env_bio_oracle_names, "MS_bathy_5m")


  env_raster <- sdmpredictors::load_layers(env_bio_oracle_names,
                                           datadir = biooracle_folder,
                                           rasterstack = FALSE)

  target_grid <- raster::raster(x = env_poly,
                                     resolution = regrid_resolution,
                                     crs = "+proj=longlat +datum=WGS84")

  raster_crop <- raster::brick(lapply(env_raster,
                                      function(r) {
                                        raster::crop(r, raster::extent(target_grid))
                                      }))
  raster_rescale <- raster::resample(raster_crop,
                                     target_grid,
                                     method = "bilinear")
  ##mask is much quicker than st_intersection
  raster_masked <- raster::mask(raster_rescale, env_poly)
  raster::crs(raster_masked) <-"+proj=longlat +datum=WGS84"
  env_points <- data.table::data.table(raster::rasterToPoints(raster_masked))


  env_region <- na.omit(env_points)

  data.table::setnames(env_region, 1:2, spatial_vars)

  data.table::setkeyv(env_region, spatial_vars)

  env_clipped <- env_clip_extremes(env_data = env_region,
                                   std_thres = env_limits_sd)

  env_round <- rphildyerphd::align_sp(env_clipped,
                                      spatial_cols = spatial_vars,
                                      res = regrid_resolution,
                                      offset = env_offset,
                                    fun = mean)
  data.table::setDT(env_round)
  env_round[, c(env_id_col) :=  seq_len(nrow(env_round))]
  return(env_round)

}

env_clip_extremes <- function(env_data, std_thres) {

  ## Clipping extremes, post logging
  purrr::walk(names(env_data), ~{
    x_sd <- sd(env_data[[.x]])
    x_mean <- mean(env_data[[.x]])
    min_x <- x_mean - x_sd * std_thres
    max_x <- x_mean + x_sd * std_thres
    env_data[[.x]] <- pmax(min_x, pmin(env_data[[.x]], max_x))
  })
  return(env_data)
}
