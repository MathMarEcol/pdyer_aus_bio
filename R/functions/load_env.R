#' Pull in env data, process, and shape to
#' domain of study
#'
#'
# Generate a list of targets for each regrid_resolution
load_env_domain <- function(
                            biooracle_folder,
                            env_biooracle_names,
                            env_poly,
                            max_depth,
                            regrid_resolution,
                            env_extrap_year,
                            spatial_vars,
                            env_limits_sd,
                            env_offset,
                            env_id_col
                            ) {

  tmp_timeout <- getOption("timeout")
  options(timeout = 60000)

  env_raster <- suppressWarnings(sdmpredictors::load_layers(env_biooracle_names$env_biooracle_names[[1]],
                                                            datadir = biooracle_folder,
                                                            rasterstack = FALSE))
    options(timeout = tmp_timeout)

    target_grid <- raster::raster(x = env_poly$data[[1]],
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
    raster_masked <- raster::mask(raster_rescale, env_poly$data[[1]])
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
  env_domain <- data.table::data.table(domain = env_poly$name, res = regrid_resolution, env_year = env_biooracle_names$env_year, env_pathway = env_biooracle_names$env_pathway, data = list(env_round))
  return(env_domain)

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
