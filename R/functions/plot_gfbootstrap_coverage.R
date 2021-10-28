plot_gfbootstrap_coverage <- function(
                                        gfbootstrap_polygons,
                                        mpa_polygons,
                                        output_folder
                                      ) {

  pl_file_base <- file.path(output_folder, gfbootstrap_polygons$surv_full_name[1])
  pl_file <-  paste0(pl_file_base, "_cluster_mpa_coverage.png")
  ### Skip failed surveys
  if (is.na(gfbootstrap_polygons$polygons)) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(gfbootstrap_polygons$surv_full_name[1], " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file, plot = no_plot)
    return(pl_file)
  }

  ### Calculate overlap
  clust_poly_sf <- gfbootstrap_polygons$polygons[[1]]
  ## st_intersection should give us the polygons covering both clusters and MPAs.
  ## Need to project both polygon sets into the same CRS, but details of CRS are not critical?
  clust_poly_sf_trans <- sf::st_transform(clust_poly_sf, sf::st_crs(mpa_polygons))
  clust_mpa_intersect <- sf::st_intersection(clust_poly_sf_trans,  mpa_polygons)

  clust_area_covered <- sf::st_area(clust_mpa_intersect)
  clust_area_total <- sf::st_area(clust_poly_sf_trans)
  clust_area_frac <- clust_area_covered / clust_area_total
  ## Plot? tabulate?
  ## Create data.frame with area, clust_id and plot_order.
  ## plot order is just 1 to k, added after sorting clust_id by area
  ## plot_order gives left to right, clust_id gives mapping to plots
  clust_area_table <- data.table::data.table(clust_id = clust_poly_sf_trans$clustering,
                                             area =  as.numeric(clust_area_frac))

  data.table::setorder(clust_area_table, "area")
  clust_area_table[, "plot_order" := seq_along(clust_area_table$clust_id)]

  pl_clust_coverage <- ggplot2::ggplot(clust_area_table ,  ggplot2::aes(x = reorder(clust_id, area), y = area)) +
    ggplot2::scale_x_discrete("Cluster") +
    ggplot2::scale_y_continuous("MPA Coverage (Fraction of total cluster area)", limits =  c(0, 1), breaks = seq(0, 1, 0.1 )) +
    ggplot2::ggtitle(paste0(gfbootstrap_polygons$surv_full_name[1], " MPA coverage by cluster") ) +
    ggplot2::geom_col() +
    ggthemes::theme_tufte()

  ggsave_wrapper(filename = pl_file, plot = pl_clust_coverage)
  return(pl_file)
}
