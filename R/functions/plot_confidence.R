plot_confidence <- function(cluster_env_assign_cluster,
                            env_domain_plot,
                            env_poly,
                            marine_map,
                            max_clust_prob_plot,
                            env_id_col,
                            spatial_vars,
                            plot_description,
                            output_folder) {

       survey_specs <- cluster_env_assign_cluster[,
                                                 c("env_domain",
                                                   "trophic",
                                                   "survey",
                                                   "depth_cat",
                                                   "clust_method")]
    survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
    survey_specs <- as.character(survey_specs)

    pl_file_base <- file.path(output_folder, paste0(survey_specs, collapse = "_"))

    if (all(is.na(cluster_env_assign_cluster$clust_ind))) {
        no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point() +
          ggplot2::ggtitle(paste0(paste0(c(survey_specs, plot_description), collapse = "_"),  " has not successfully clustered"))
        file_names <- paste0(pl_file_base, "_", plot_description, ".png")
        ggsave_wrapper(filename = file_names, plot = no_plot)
        return(file_names)
    }
    s2_used <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)

    cluster_prob <- data.table::rbindlist(lapply(seq_along(cluster_env_assign_cluster$pred_membership[[1]]$max),
                                     function(i, cluster_env_assign_cluster){
                                         out <- data.table::data.table(
                                           prob_cl = cluster_env_assign_cluster$pred_membership[[1]]$prob_cl[[i]]
                                           )
                                         out[, c(env_id_col) := cluster_env_assign_cluster$pred_membership[[1]]$max[[i]][[env_id_col]]]

                                     }, cluster_env_assign_cluster = cluster_env_assign_cluster))
    cluster_prob_long <- data.table::melt(cluster_prob, id.vars = env_id_col, value.name = "prob", variable.name = "clust")

    ## Preserves cluster membership, may lose cluster id
    cluster_prob_long[, clust := as.integer(clust)]

    cluster_prob_long[env_domain_plot[domain == cluster_env_assign_cluster$env_domain[[1]], data][[1]], on = c(env_id_col),
              c(spatial_vars, env_id_col) := mget(paste0("i.", c(spatial_vars, env_id_col)))]

    file_names <- character(min(length(unique(cluster_prob_long$clust)), max_clust_prob_plot))

    env_poly_local <- env_poly[name == cluster_env_assign_cluster$env_domain[[1]], data][[1]]
    plot_cols <- c(spatial_vars, "prob")
    max_prob <- max(cluster_prob_long$prob)
    for (cl in seq.int(length(file_names))) {
        clust_raster <- terra::rast(
            x = as.matrix(cluster_prob_long[clust == cl, ..plot_cols]),
            type = "xyz",
            crs = "+proj=longlat +datum=WGS84")

        env_bbox <-  sf::st_bbox(env_poly_local,
                             crs = "+proj=longlat +datum=WGS84")

        pl_conf <- tmap::tm_shape(clust_raster, bbox = env_bbox) +
          tmap::tm_raster("prob", palette = "seq", style = "cont", breaks = c(0, max_prob))

            if (cluster_env_assign_cluster$env_domain != "aus_eez") {
                pl_conf <- pl_conf + tmap::tm_shape(marine_map, bbox = env_bbox) +
                                       tmap::tm_borders(lwd = 1)
            }
            pl_conf <- pl_conf + tmap::tm_shape(env_poly_local, bbox = env_bbox) +
                tmap::tm_borders(lwd = 1)

        pl_file <- paste0(paste0(c(pl_file_base, paste0(c("cl", cl), collapse = ""), plot_description), collapse = "_"), ".png")
        file_names[cl] <- pl_file
        tmap_save_wrapper(tm = pl_conf, filename = pl_file, scale = 0.8, dpi = 1200)
        }
    sf::sf_use_s2(s2_used)
    return(file_names)
}
