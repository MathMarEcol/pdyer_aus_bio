# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
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
                            res_unique_target,
                            spatial_vars,
                            env_limits_sd,
                            env_offset,
                            env_id_col,
                            biooracle_files
                            ) {

  tmp_timeout <- getOption("timeout")
  options(timeout = 6) ## Don't download here

  env_raster <- suppressWarnings(sdmpredictors::load_layers(env_biooracle_names$env_biooracle_names[[1]],
                                                            datadir = biooracle_folder,
                                                            rasterstack = FALSE))
    options(timeout = tmp_timeout)

    target_grid <- raster::raster(x = env_poly$data[[1]],
                                     resolution = res_unique_target,
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

  env_round <- align_sp(env_clipped,
                                      spatial_cols = spatial_vars,
                                      res = res_unique_target,
                                      offset = env_offset,
                                    fun = mean)
  data.table::setDT(env_round)
  env_round[, c(env_id_col) :=  seq_len(nrow(env_round))]
  env_domain <- data.table::data.table(domain = env_poly$name, res = res_unique_target, env_year = env_biooracle_names$env_year, env_pathway = env_biooracle_names$env_pathway, data = list(env_round))
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


#' Align data.frame to a grid
#'
#' Takes a data.frame and aligns it to a regular grid.
#' Rows that fall into the same grid cell are aggregated by fun.
#' If fun is NULL, then rows are not aggregated.
#'
#'
#' This function is suitable for sparse datasets that are not necessarily on grid, and are in data.frames.
#' The raster package should be used if both datasets are on a regular grid.
#' sp and sf packages should be used if you have want more complex spatial manipulations.
#'
#' @param x Data.frame of input data
#' @param spatial_cols character vector, columns in x and y that specify spatial position
#' @param res numeric. target grid resolution
#' @param grid_offset numeric. target grid offset
#' @param fun function, applied to data that falls in the same grid cell. If NULL, no agreggation is done
#' @param ... additional arguments to fun
#'
#' @return data.frame, with spatial columns rounded to grid and rows aggregated by fun if fun is not NULL. Sorted ascending by spatial_cols, starting with spatial_cols[1]
#'
#' @export
#'
#' @examples
#' set.seed(1000)
#'
#' target_res <- 1/10
#'
#' target_offset <- 0
#'
#' spatial_cols <- c("lat", "lon")
#'
#' #Parameters are checked
#' invalid_data <- cluster::clara(cluster::xclara, 5)
#' valid_data <- data.frame(lat = 1:5, lon = 1:5, val = 1:5)
#' testthat::expect_error(align_sp(invalid_data), "cannot coerce class ‘c(\"clara\", \"partition\")’ to a data.frame", fixed = TRUE)
#' testthat::expect_error(align_sp(valid_data[,2:3],  spatial_cols)  , "x does not have all of these name(s): 'lat', 'lon'",  fixed = TRUE)
#' testthat::expect_error(align_sp(valid_data,  res = "a")  , "res is not a numeric or integer vector",  fixed = TRUE)
#' testthat::expect_error(align_sp(valid_data,   res = c(1,2))  , "length(res) not equal to 1",  fixed = TRUE)
#' testthat::expect_error(align_sp(valid_data,  grid_offset = "a")  , "grid_offset is not a numeric or integer vector",  fixed = TRUE)
#' testthat::expect_error(align_sp(valid_data,  grid_offset = c(1,2))  , "length(grid_offset) not equal to 1",  fixed = TRUE)
#' testthat::expect_error(align_sp(valid_data,  fun = notafun)  , "object 'notafun' not found",  fixed = TRUE)
#'
#' #Align to grid
#' x <- data.frame(lat = c(0.1, .14, .26, 0.2, 0.35), lon = c(0.1, 0.2, 0.26, .14, 0.45), val_x = 1:5)
#'
#' testthat::expect_equal(align_sp(x,spatial_cols =  c("lat", "lon"), res = target_res, grid_offset = target_offset, fun = mean),
#'    data.frame(lat = c(0.1, .1, 0.2, .3, 0.3), lon = c(0.1, 0.2, .1, 0.3, 0.4), val_x =  c(1,2,4,3,5)))
#'
#' #Alternative spatial_cols
#' sp_test <- data.frame(latitude = c(0.1, .14, .26, 0.2, 0.35), longitude = c(0.1, 0.2, 0.26, .14, 0.45), val_x = 1:5)
#'
#' testthat::expect_equal(align_sp(sp_test, spatial_cols =  c("latitude", "longitude"), res = target_res, grid_offset = target_offset, fun = mean),
#'    data.frame(latitude = c(0.1, .1, 0.2, .3, 0.3), longitude = c(0.1, 0.2, .1, 0.3, 0.4), val_x = c(1,2,4,3,5)))
#'
#'
align_sp <- function(x,
                     spatial_cols = c("lat", "lon"),
                     res = 1,
                     grid_offset = 0,
                     fun,
                     ...){
  x <- as.data.frame(x)

  assertthat::assert_that(assertthat::has_name(x, spatial_cols))

  assertthat::assert_that(is.numeric(res))
  assertthat::assert_that(assertthat::are_equal(length(res), 1))

  assertthat::assert_that(is.numeric(grid_offset))
  assertthat::assert_that(assertthat::are_equal(length(grid_offset), 1))


  x[,spatial_cols] <- round(x[,spatial_cols]/res + grid_offset)

  if(is.null(fun)){
    x_agg <- x
  } else {
    x_agg <- aggregate(x, by = list(x[,spatial_cols[1]], x[,spatial_cols[2]]), fun, ..., simplify = TRUE)
    x_agg <- x_agg[,names(x)]
  }

  sort_order <- lapply(spatial_cols, df = x_agg, function(x, df){df[[x]]})
  do.call(order, sort_order)

  x_agg <- x_agg[do.call(order, sort_order),]
  row.names(x_agg) <-  1:nrow(x_agg)


  x_agg[,spatial_cols] <-  (x_agg[,spatial_cols] - grid_offset )*res
  return(x_agg)
}
