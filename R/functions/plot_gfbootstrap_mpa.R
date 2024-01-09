plot_gfbootstrap_mpa <- function(
                             gfbootstrap_cluster,
                             gfbootstrap_polygons,
                             gfbootstrap_predicted,
                             env_poly,
                             mpa_polygons,
                             spatial_vars,
                             marine_map,
                             plot_clust_labels,
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
    pl_file <- file.path(output_folder, pl_survey_name)
    pl_file <- paste0(pl_file, ".png")

    if (is.na(gfbootstrap_cluster$best_clust)) {
        no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
        ggsave_wrapper(filename = pl_file, plot = no_plot)
        return(pl_file)
    }

    s2_used <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    k <- gfbootstrap_cluster$clust[[1]]$k[gfbootstrap_cluster$best_clust]
    imp_preds <- gfbootstrap_predicted$imp_preds[[1]]
    pred_string <- paste(
        sapply(
            split(imp_preds, rep(seq.int(1,length(imp_preds)), each = 5, length.out = length(imp_preds))),
            function(x){paste(x, collapse = ", ")}
        ),
        collapse = "\n"
    )

        pl_no_samp <- plot_clust_poly(cluster_polygons = gfbootstrap_polygons$polygons[[1]],
                                  gfbootstrap_polygons$polygons_no_clust[[1]],
                                  spatial_vars = spatial_vars,
                                  marine_map = env_poly[name == "aus_eez", data][[1]],
                                  plot_map = gfbootstrap_cluster$env_domain != "aus_eez",
                                  env_poly = env_poly[name == gfbootstrap_cluster$env_domain, data][[1]],
                                  labels = plot_clust_labels)  +
        tmap::tm_layout(main.title = glue::glue_data(gfbootstrap_cluster,
                                                     "Clustering for depth [{depth_cat}] in survey [{survey}]\n",
                                                     "studying trophic level [{trophic}], domain is {env_domain} at res {res_clust}.\n",
                                                     "Clustered with {clust_method} which found {k} clusters. Showing [{mpa_polygons$iucn_categories[[1]]$name}] MPAs. Predictors used:\n",
                                                     "{pred_string}"),
                        main.title.size = 0.5) +
            tmap::tm_shape(mpa_polygons$mpa_polys[[1]]) +
            tm_polygons(col = "grey", alpha = 0.7, border.col = "grey", border.alpha = 0.9)

    ## ggsave_wrapper(filename = pl_file["no_samp"], plot = pl_no_samp)
    tmap_save_wrapper(tm = pl_no_samp, filename = pl_file, scale = 0.1, dpi = 1200)

    sf::sf_use_s2(s2_used)
    return(pl_file)
}

