# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
plot_gfbootstrap_coverage <- function(
                                      gfbootstrap_polygons,
                                      env_biooracle_names,
                                      mpa_polygons,
                                      plot_description,
                                        output_folder
                                      ) {

    survey_specs <- gfbootstrap_polygons[
      ,
      c(
        "env_domain", "env_year", "env_pathway", "res_gf", "res_clust",
        "trophic",
        "survey",
        "depth_cat",
        "clust_method"
      )
    ]



    survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
    survey_specs <- as.character(survey_specs)

    pl_survey_name <- paste0(c(survey_specs, mpa_polygons$iucn_categories[[1]]$name, plot_description),
                                                 collapse = "_")

  pl_file_base <- file.path(output_folder, pl_survey_name)
  pl_file <-  paste0(pl_file_base, ".pdf")
  ### Skip failed surveys
  if (is.na(gfbootstrap_polygons$polygons)) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file, plot = no_plot)
    return(pl_file)
  }
    s2_used <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)

  ### Calculate overlap
  clust_poly_sf <- gfbootstrap_polygons$polygons[[1]]
  ## st_intersection should give us the polygons covering both clusters and MPAs.
  ## Need to project both polygon sets into the same CRS, but details of CRS are not critical?
  clust_poly_sf_trans <- sf::st_transform(clust_poly_sf, sf::st_crs(mpa_polygons$mpa_polys[[1]]))
  clust_mpa_intersect <- sf::st_intersection(clust_poly_sf_trans, mpa_polygons$mpa_polys[[1]])

  clust_area_covered <- sf::st_area(clust_mpa_intersect)
  clust_area_total <- sf::st_area(clust_poly_sf_trans)
  clust_area_frac <- clust_area_covered / clust_area_total
  ## Plot? tabulate?
  ## Create data.frame with area, clust_id and plot_order.
  ## plot order is just 1 to k, added after sorting clust_id by area
  ## plot_order gives left to right, clust_id gives mapping to plots
  clust_area_table <- data.table::data.table(clust_id = clust_poly_sf_trans$clustering,
                                             area =  as.numeric(clust_area_frac))

  data.table::setorder(clust_area_table, -area)
  clust_area_table[, "plot_order" := seq_along(clust_area_table$clust_id)]

    pl_clust_coverage <- ggplot2::ggplot(clust_area_table, ggplot2::aes(x = plot_order, y = area)) +
      geom_col() +
      ggplot2::scale_x_continuous(name = "Bioregion", breaks = seq_along(clust_area_table$clust_id), labels = clust_area_table$clust_id) +
      ggplot2::scale_y_continuous("Proportion of Bioregion covered by MPAs", limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      ggplot2::ggtitle(paste0(pl_survey_name, " MPA coverage by cluster")) +
      ggplot2::geom_hline(yintercept = c(0.1, 0.3), colour = "red") +
      ggthemes::theme_tufte() +
      ggplot2::theme(
                   axis.ticks.x = element_blank(),
                   axis.text.x = element_blank()
      )

  ggsave_wrapper(filename = pl_file, plot = pl_clust_coverage)
    sf::sf_use_s2(s2_used)
  return(pl_file)
}
