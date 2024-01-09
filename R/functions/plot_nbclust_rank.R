plot_nbclust_rank <- function(
                              gf_cluster_nbclust,
                              plot_description,
                              output_folder
                              ) {


    survey_specs <- gf_cluster_nbclust[,
      c(
          "env_domain",
          "res_gf",
          "res_clust",
          "trophic",
          "survey",
          "depth_cat",
          "method",
          "dist"
      )
    ]
    survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
    survey_specs <- as.character(survey_specs)

    pl_file_base <- file.path(output_folder, paste0(survey_specs, collapse = "_"))


    if (all(is.na(gf_cluster_nbclust$best_nc[[1]]))) {
        no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste0(paste0(c(survey_specs, plot_description), collapse = "_"),  " has not successfully clustered"))
        file_names <- paste0(pl_file_base, "_", plot_description, ".png")
        ggsave_wrapper(filename = file_names, plot = no_plot)
        return(file_names)
    }


    ranked_scores <- data.frame(
        heuristic = names(gf_cluster_nbclust$best_nc[[1]]),
        nclust = gf_cluster_nbclust$best_nc[[1]]
        )
    ranked_scores <- ranked_scores[order(ranked_scores$nclust), ]
    ranked_scores <- cbind(ranked_scores, list(x = seq.int(to = nrow(ranked_scores))))

    pl_nbclust_rank <- ggplot2::ggplot(ranked_scores, aes(x = x, y = nclust, labels = heuristic)) +
      ggplot2::geom_point() +
        ggplot2::scale_x_discrete(limits = factor(ranked_scores$x),
                                  labels = ranked_scores$heuristic,
                                  name = "Heuristic",
                                  guide = ggplot2::guide_axis(angle = 45)) +
        ggplot2::ggtitle("Recommended number of clusters by hyperparameter tuning heuristics, ranked") +
          ggplot2::geom_hline(yintercept = 41, colour = "red") +
          ggthemes::theme_tufte()

    pl_file <- paste0(pl_file_base, "_", plot_description, ".png")
    ggsave_wrapper(
      filename = pl_file,
      plot = pl_nbclust_rank
    )

    return(pl_file)
}
