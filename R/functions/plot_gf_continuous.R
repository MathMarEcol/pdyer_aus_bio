plot_gf_continuous <- function(
                               gf_predicted,
                               marine_map,
                               env_poly,
                               pca_n_vars,
                               pca_scale,
                               plot_description,
                               regrid_resolution,
                               output_folder
                               ) {


    survey_specs <- gf_predicted[,
                                      c("env_domain",
                                        "trophic",
                                        "survey",
                                        "depth_cat"
                                        )]
  survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
  survey_specs <- as.character(survey_specs)

  pl_survey_name <- paste0(c(survey_specs, plot_description),
                                                 collapse = "_")
  pl_file_base <- file.path(output_folder, pl_survey_name)
  pl_file <- c(
    map = paste0(pl_file_base, "_map.png"),
    pca = paste0(pl_file_base, "_pca.png")
  )

  if (all(is.na(gf_predicted$imp_preds))) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file["map"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["pca"], plot = no_plot)
    return(pl_file)
  }

  p_comps <- prcomp(gf_predicted$comp_turnover[[1]][, c(gf_predicted$imp_preds[[1]]), with = FALSE])
  a1 <- p_comps$x[,1]
  a2 <- p_comps$x[,2]
  a3 <- p_comps$x[,3]
  r <- a1+a2
  g <- -a2
  b <- a3+a2-a1
  r <- (r-min(r)) / (max(r)-min(r)) * 255
  g <- (g-min(g)) / (max(g)-min(g)) * 255
  b <- (b-min(b)) / (max(b)-min(b)) * 255
  colours <- rgb(r, g, b, max = 255)


  ## Plot on map
  rast_mat <- terra::rast(as.matrix(
    data.frame(
      lon = gf_predicted$comp_turnover[[1]][, c(spatial_vars[1]), with = FALSE],
      lat = gf_predicted$comp_turnover[[1]][,
              c(spatial_vars[2]), with = FALSE] - regrid_resolution / 4,
      r = r,
      g = g,
      b = b)),
                          type="xyz",
                          crs = "+proj=longlat +datum=WGS84")


  sv1 <- spatial_vars[1]
  sv2 <- spatial_vars[2]
  env_bbox <- sf::st_bbox(c(xmin = min(gf_predicted$comp_turnover[[1]][, ..sv1]),
                            xmax = max(gf_predicted$comp_turnover[[1]][, ..sv1]),
                            ymin = min(gf_predicted$comp_turnover[[1]][, ..sv2]),
                            ymax = max(gf_predicted$comp_turnover[[1]][, ..sv2])),
                          crs = "+proj=longlat +datum=WGS84"
                          )

  imp_preds <- gf_predicted$imp_preds[[1]]
  pred_string <- paste(
      sapply(
        split(imp_preds, rep(seq.int(1,length(imp_preds)), each = 5, length.out = length(imp_preds))),
        function(x){paste(x, collapse = ", ")}
      ),
      collapse = "\n"
  )



  pl_tm <- tmap::tm_shape(rast_mat, bbox = env_bbox) +
    tmap::tm_rgb(r = 1, g = 2, b = 3 , interpolate = FALSE)
  if (gf_predicted$env_domain != "aus_eez"){
    pl_tm <- pl_tm + tmap::tm_shape(env_poly[name == "aus_eez", data][[1]], bbox = env_bbox) +
      tmap::tm_borders(lwd = 2, col = "black")
  }
  pl_tm <- pl_tm + tmap::tm_shape(env_poly[name == gf_predicted$env_domain, data][[1]], bbox = env_bbox) +
    tmap::tm_borders(lwd = 2, col = "black")

  pl_tm <- pl_tm +
    tmap::tm_layout(main.title = glue::glue_data(gf_predicted,
                                                 "Continuous Compositional turnover for depth [{depth_cat}] in survey [{survey}]\n",
                                                 "studying trophic level [{trophic}], domain is {env_domain}.\n",
                                                 "Predictors used:\n",
                                                 "{pred_string}"),
                    main.title.size = 0.5)

  tmap_save_wrapper(tm = pl_tm, filename = pl_file["map"], scale = 0.1, dpi = 1200)

  ## PCA

  pca_df <- data.frame(
    pc1 = p_comps$x[, 1],
    pc2 = p_comps$x[, 2],
    colours = colours
  )

  pca_n_vars <- min(pca_n_vars, length(imp_preds))

  pred_ind <- rownames(p_comps$rotation) %in% imp_preds[seq.int(pca_n_vars)]

  pl <- ggplot2::ggplot(pca_df, ggplot2::aes(x = pc1, y = pc2, colour = I(colours))) +
    ggplot2::geom_point(show.legend = FALSE) +
    ggplot2::geom_point(data = data.frame(p_comps$rotation[!pred_ind, 1:2]) * pca_scale, mapping = aes(x = PC1, y = PC2), inherit.aes = FALSE) +
      ggplot2::geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data.frame(p_comps$rotation[pred_ind, 1:2]) * pca_scale,
                            arrow = arrow(),
                            inherit.aes = FALSE) +
      ggrepel::geom_text_repel(
        data = data.frame(imp_preds, p_comps$rotation[, 1:2] * pca_scale)[pred_ind, ],
        mapping = aes(x = PC1, y = PC2, label = imp_preds),
        inherit.aes = FALSE
            ) +
    ggthemes::theme_tufte()

  ggsave_wrapper(filename = pl_file["pca"], plot = pl)

  return(pl_file)
}
