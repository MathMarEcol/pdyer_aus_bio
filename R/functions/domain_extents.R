

domain_extent_targets <- function(
                                 mapfile_location,
                                 map_layer,
                                 env_bounds
                                 ) {

  list(
    tar_target(
      marine_map,
      sf::st_read(mapfile_location,
                  layer = map_layer)
    ),

    ## env_poly is a polygon that defines the study area.
    ## env_extent is the bounding box of env_poly
    ## env_bounds are manually specified extents to the study area
    tar_target(
      env_poly,
      rbind(
      ## Use Aus EEZ as study area
      data.table(name = "aus_eez",
                 data = list(marine_map[marine_map$Country == "Australia", ])),

      ## Use box around Aus as study area
      data.table(name = "aus_bbox",
                 data = list(sf::st_as_sf(as(raster::extent(env_bounds), "SpatialPolygons"),
                                     crs = sf::st_crs(marine_map))
                   )
                 )
      ),
      ## Downstream targets will get a data.table row
      ## env_poly$name will return a single string
      ## but env_poly$data will return a list
      ## Most functions will want to use env_poly$data[[1]]
      iteration = "vector"
    ),

    tar_target(
      env_extent,
      data.table(name = env_poly$name, extent = list(raster::extent(env_poly$data[[1]]))),
      iteration = "vector",
      ## Without pattern, env_poly will be a data.table containing all values of
      ## env_poly
      pattern = map(env_poly)
    )
  )
}

