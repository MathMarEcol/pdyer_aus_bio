cluster_raster_to_polygons <- function(
                                       cluster_row, #generic, works with any clustering
                                       spatial_vars
                                       ) {


  if(any(is.na(cluster_row$clust_ind[[1]]))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table::data.table(cluster_row[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid, clust_method, clust, best_clust, clust_ind)],
      polygons = list(NA)
    ))

  }

  ## cluster_row is assumed to have a clust_ind table with sites and a cl entry with cluster IDs
  sites<- data.table::as.data.table(cluster_row$clust_ind[[1]][, ..spatial_vars])
  clust_raster <- terra::rast(
            x = as.matrix(data.frame(sites[, ..spatial_vars], gfbootstrap_caster$clust_ind[[1]]$cl)),
            type = "xyz",
            crs = "+proj=longlat +datum=WGS84")

  names(clust_raster) <- c("clustering")
  clust_multipoly <- terra::as.polygons(clust_raster)
  clust_poly_sf <- sf::st_as_sf(clust_multipoly)

    return(data.table::data.table(cluster_row[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid, clust_method, clust, best_clust, clust_ind)],
      polygons = list(clust_poly_sf)
    ))
}
