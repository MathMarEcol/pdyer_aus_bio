plot_gf_continuous <- function(
                               gf_predicted,
                               env_domain,
                               env_biooracle_names,
                               pred_importance_top,
                               marine_map,
                               env_poly,
                               pca_n_vars,
                               pca_scale,
                               plot_description,
                               output_folder
                               ) {



    survey_specs <- gf_predicted[,
                                 c("env_domain",
                                   "env_year",
                                   "env_pathway",
                                   "res_gf",
                                   "res_clust",
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
    map = paste0(pl_file_base, "_map.pdf"),
    pca = paste0(pl_file_base, "_pca.pdf")
  )

  if (all(is.na(gf_predicted$imp_preds))) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file["map"], plot = no_plot)
    ggsave_wrapper(filename = pl_file["pca"], plot = no_plot)
    return(pl_file)
  }

  if (gf_predicted$survey %in% c("envonly", "envmeanonly")) {
    return(plot_env_continuous(
      pl_file,
      gf_predicted,
      env_domain,
      env_biooracle_names,
      pred_importance_top,
      marine_map,
      env_poly,
      pca_n_vars,
      pca_scale,
      plot_description,
      output_folder
    ))
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
      lat = gf_predicted$comp_turnover[[1]][, c(spatial_vars[2]), with = FALSE],
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
                                                 "Continuous Compositional turnover"),
                    main.title.size = 0.5)

  tmap_save_wrapper(tm = pl_tm, filename = pl_file["map"])

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
        data = data.frame(imp_preds, p_comps$rotation[, 1:2] * pca_scale),
        mapping = aes(x = PC1, y = PC2, label = imp_preds),
        inherit.aes = FALSE
            ) +
    ggthemes::theme_tufte()

  ggsave_wrapper(filename = pl_file["pca"], plot = pl)

  return(pl_file)
}


plot_env_continuous <- function(
                                pl_file,
                                gf_predicted,
                                env_domain,
                                env_biooracle_names,
                                pred_importance_top,
                                marine_map,
                                env_poly,
                                pca_n_vars,
                                pca_scale,
                                plot_description,
                                output_folder
                                ) {

  ## survey has already been checked for validity and filenames
  ## are already generated
  env_names <- switch(gf_predicted$survey,
    "envmeanonly" = {
      ## Only means and bathymetry
      env_names <- env_biooracle_names[
        env_year == gf_predicted$env_year &
          env_pathway == gf_predicted$env_pathway,
        "env_biooracle_names"
      ][[1]][[1]]
      env_names <- env_names[grepl("mean_ss", env_names)]
      c(env_names, "MS_bathy_5m")
    },
    {
      ## default, all env
      env_biooracle_names[
        env_year == gf_predicted$env_year &
          env_pathway == gf_predicted$env_pathway,
        "env_biooracle_names"
      ][[1]][[1]]
    }
    )





  env_dom <- env_domain[domain == gf_predicted$env_domain &
    res == gf_predicted$res_clust &
    env_year == gf_predicted$env_year &
    env_pathway == gf_predicted$env_pathway, "data"][[1]][[1]]

  p_comps <- prcomp(env_dom[, ..env_names], center = TRUE, scale = TRUE)

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
      lat = gf_predicted$comp_turnover[[1]][, c(spatial_vars[2]), with = FALSE],
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

  imp_preds <- if (pred_importance_top >= 1) {
      names(
        sort(
          sqrt(
            p_comps$rotation[, 1]^2 +
              p_comps$rotation[, 2]^2
          ),
          decreasing = TRUE
        )
      )[seq.int(1, min(nrow(p_comps$rotation), pred_importance_top))]
    } else {
      imp_explained <- cumsum(p_comps$sdev) / sum(p_comps$sdev)
      ## Take all predictors below threshold, then one more
      n_preds <- sum(imp_explained < pred_importance_top) + 1
      names(
        sort(
          sqrt(
            p_comps$rotation[, 1]^2 +
              p_comps$rotation[, 2]^2
          ),
          decreasing = TRUE
        )
      )[seq.int(1, n_preds)]
    }

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

  tmap_save_wrapper(tm = pl_tm, filename = pl_file["map"])

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
      data = data.frame(imp_preds, p_comps$rotation[, 1:2] * pca_scale),
      mapping = aes(x = PC1, y = PC2, label = imp_preds),
      inherit.aes = FALSE
    ) +
    ggthemes::theme_tufte()

  ggsave_wrapper(filename = pl_file["pca"], plot = pl)


  return(pl_file)
}
