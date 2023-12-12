plot_nbclust_rank <- function(
                              gf_predicted,
                              env_domain_plot,
                              env_poly,
                              spatial_vars
                              marine_map,
                              plot_description = "nbclust_rank",
                              output_folder
                              ) {


    survey_specs <- gf_predicted[,
      c(
        "env_domain",
        "trophic",
        "survey",
        "depth_cat"
      )
    ]
    survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
    survey_specs <- as.character(survey_specs)

    pl_file_base <- file.path(output_folder, paste0(survey_specs, collapse = "_"))


    if (all(is.na(gf_predicted$comp_turnover))) {
        no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste0(paste0(c(survey_specs, plot_description), collapse = "_"),  " has not successfully clustered"))
        file_names <- paste0(pl_file_base, "_", plot_description, ".png")
        ggsave_wrapper(filename = file_names, plot = no_plot)
        return(file_names)
    }


return(NULL)
}