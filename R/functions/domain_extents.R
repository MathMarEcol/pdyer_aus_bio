

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
      ## Use Aus EEZ as study area
      ## marine_map[marine_map$Country == "Australia", ]

      ## Use box around Aus as study area
      sf::st_as_sf(as(raster::extent(env_bounds), "SpatialPolygons"),
                   crs = sf::st_crs(marine_map)
                   )
    ),

    tar_target(
      env_extent,
      raster::extent(env_poly),
    )
  )
}

