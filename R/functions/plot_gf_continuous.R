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

  ## get scale of lines right
  ## scale of eigenvectors and scale of points from dataset
  ## don't always line up.
  ## eigenvectors are normalised to 1.
  ## points are not
  ## Set scale of "rotation" so it is as large as possible without breaking the
  ## plot
  indicator_radius <- max(
    c(
      range(pca_df$pc1),
      range(pca_df$pc2)
    )
  )

  scaled_eigs <- p_comps$rotation * indicator_radius

  prominent_eig_vars <- sort(apply(scaled_eigs[, 1:2], 1, \(x){
    norm(x, type = "2")
  }), decreasing = TRUE)

  pca_n_vars_local <- min(pca_n_vars, length(imp_preds))

  pred_ind <- rownames(scaled_eigs) %in% imp_preds[seq.int(pca_n_vars_local)]
  prom_names <- names(prominent_eig_vars)[pred_ind]
  prom_ind <- rownames(scaled_eigs) %in% prom_names

  pl <- ggplot2::ggplot(pca_df, ggplot2::aes(x = pc1, y = pc2, colour = I(colours))) +
    ggplot2::geom_point(show.legend = FALSE, size = 0.2) +
    ggplot2::geom_point(data = data.frame(scaled_eigs[!prom_ind, 1:2]), mapping = aes(x = PC1, y = PC2), size = 0.25, inherit.aes = FALSE) +
    ggplot2::geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data.frame(scaled_eigs[prom_ind, 1:2]),
      arrow = grid::arrow(length = (grid::unit(0.25, "cm"))),
      linewidth = 0.2,
      inherit.aes = FALSE
    ) +
    ggrepel::geom_text_repel(
      data = data.frame(imp_preds[prom_ind], scaled_eigs[prom_ind, 1:2]),
      mapping = aes(x = PC1, y = PC2, label = imp_preds[prom_ind]),
      size = 2,
      inherit.aes = FALSE
    ) +
    ggthemes::theme_tufte() +
    xlab("PC1") +
    ylab("PC2")

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
                                                 "Continuous Compositional turnover"),
                    main.title.size = 0.5)

  tmap_save_wrapper(tm = pl_tm, filename = pl_file["map"])

  ## PCA

  pca_df <- data.frame(
    pc1 = p_comps$x[, 1],
    pc2 = p_comps$x[, 2],
    colours = colours
  )

  ## get scale of lines right
  ## scale of eigenvectors and scale of points from dataset
  ## don't always line up.
  ## eigenvectors are normalised to 1.
  ## points are not
  ## Set scale of "rotation" so it is as large as possible without breaking the
  ## plot
  indicator_radius <- max(
    c(
      range(pca_df$pc1),
      range(pca_df$pc2)
    )
  )

  scaled_eigs <- p_comps$rotation * indicator_radius

  prominent_eig_vars <- sort(apply(scaled_eigs[, 1:2], 1, \(x){
    norm(x, type = "2")
  }), decreasing = TRUE)

  pca_n_vars_local <- min(pca_n_vars, length(imp_preds))

  pred_ind <- rownames(p_comps$rotation) %in% imp_preds[seq.int(pca_n_vars_local)]

  prom_names <- names(prominent_eig_vars)[pred_ind]
  prom_ind <- rownames(scaled_eigs) %in% prom_names

  pl <- ggplot2::ggplot(pca_df, ggplot2::aes(x = pc1, y = pc2, colour = I(colours))) +
    ggplot2::geom_point(show.legend = FALSE, size = 0.2) +
    ggplot2::geom_point(data = data.frame(scaled_eigs[!prom_ind, 1:2]), mapping = aes(x = PC1, y = PC2), size = 0.25, inherit.aes = FALSE) +
    ggplot2::geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), data.frame(scaled_eigs[prom_ind, 1:2]),
                          arrow = grid::arrow(length = (grid::unit(0.25, "cm"))),
                          linewidth = 0.2,
                          inherit.aes = FALSE
                          ) +
    ggrepel::geom_text_repel(
      data = data.frame(imp_preds[prom_ind], scaled_eigs[prom_ind, 1:2]),
      mapping = aes(x = PC1, y = PC2, label = imp_preds[prom_ind]),
      size = 2,
      inherit.aes = FALSE
    ) +
    ggthemes::theme_tufte() +
    xlab("PC1") +
    ylab("PC2")

  ggsave_wrapper(filename = pl_file["pca"], plot = pl)


  return(pl_file)
}
