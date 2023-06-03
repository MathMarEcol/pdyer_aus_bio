cluster_raster_to_polygons <- function(
                                       cluster_row, #generic, works with any clustering
                                       spatial_vars
                                       ) {


  if(all(is.na(cluster_row$clust_ind))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table::data.table(cluster_row[, .(env_domain, trophic, survey, depth_cat, clust_method)],
      polygons = list(NA)
    ))

  }

  ## cluster_row is assumed to have a clust_ind table with sites and a cl entry with cluster IDs
    sites <- data.frame(cluster_row$clust_ind[[1]][, ..spatial_vars],
                                  cl = cluster_row$clust_ind[[1]]$cl)
    sites_clust <- sites[!is.na(sites$cl), ]
    sites_no_clust <- sites[is.na(sites$cl), ]
    sites_no_clust$cl <- rep.int(0, nrow(sites_no_clust))
  clust_raster <- terra::rast(
            x = as.matrix(sites_clust),
            type = "xyz",
            crs = "+proj=longlat +datum=WGS84")

  names(clust_raster) <- c("clustering")
  clust_multipoly <- terra::as.polygons(clust_raster)
  clust_poly_sf <- sf::st_as_sf(clust_multipoly)

    if(nrow(sites_no_clust) > 0) {
        no_clust_raster <- terra::rast(
                                      x = as.matrix(sites_no_clust),
                                      type = "xyz",
                                      crs = "+proj=longlat +datum=WGS84")
        names(no_clust_raster) <- c("clustering")
        no_clust_multipoly <- terra::as.polygons(no_clust_raster)
        no_clust_sf <- sf::st_as_sf(no_clust_multipoly)
    } else {
        no_clust_sf <- NA
    }

    return(data.table::data.table(cluster_row[, .(env_domain, trophic, survey, depth_cat, clust_method)],
                                  polygons = list(clust_poly_sf),
                                  polygons_no_clust = list(no_clust_sf)
    ))
}
