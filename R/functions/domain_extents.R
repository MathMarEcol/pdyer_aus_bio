

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
      list(
      ## Use Aus EEZ as study area
      aus_eez = marine_map[marine_map$Country == "Australia", ],

      ## Use box around Aus as study area
      aux_bbox = sf::st_as_sf(as(raster::extent(env_bounds), "SpatialPolygons"),
                   crs = sf::st_crs(marine_map)
                   )
      ),
      ## Downstream targets will get a list, or, if they map over env_poly,
      ## they will get env_poly[[i]], so downstream targets do not need to
      ## unroll the list with env_poly[[1]] themselves.
      iteration = "list"
    ),

    tar_target(
      env_extent,
      raster::extent(env_poly),
      iteration = "list",
      ## Without pattern, env_poly will be a list containing all values of
      ## env_poly
      pattern = map(env_poly)
    )
  )
}

