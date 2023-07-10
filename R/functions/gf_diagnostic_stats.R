gfbootstrap_diagnostic_stats <- function(gfbootstrap_combined,
                                         gfbootstrap_predicted
                                         ) {
  if (all(is.na(gfbootstrap_combined$gfbootstrap))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
      species_total = NA,
      species_mean = NA,
      species_mean_sd = NA,
      nsites_total = NA,
      nsites_mean = NA,
      nsites_sd = NA,
      spec_r2_median = NA,
      spec_r2_mean = NA,
      spec_r2_sd = NA,
      survey_r2_mean = NA,
      survey_r2_sd = NA,
      nsurveys = NA,
      var_spheq = list(NA),
      var_spheq_mean = NA,
      var_spheq_sd = NA,
      sd_spheq = list(NA),
      sd_spheq_mean = NA,
      sd_spheq_sd = NA,
      cumimp_magnitude = NA,
      cumimp_magnitude_sd = NA,
      mean_similarity = NA
    ))
  }

  gf_list <- gfbootstrap_combined$gfbootstrap[[1]]$gf_list

  ## Variance at each env site in "sph"erical "eq"uivalent
  ## Essentially a comparable measure that represents volume of the
  ## variance
  var_spheq <-
    ## Get variance matrix at each env site
    apply(
      gfbootstrap_predicted$env_pred_stats[[1]]$site_sigma,
      1, \(sigma){
        prod(diag(sigma))^(1 / nrow(sigma))
      }
    )

  ## Position of a site in compositional turnover space
  ## is manhattan.
  cumimp_magnitude <- apply(
      gfbootstrap_predicted$env_pred_stats[[1]]$site_mean,
      1, \(cumimp){
          sum(cumimp)
      }
  )


  sim_mat <- gfbootstrap_predicted$sim_mat[[1]][[1]]
  mean_similarity <- mean(sim_mat[upper.tri(sim_mat)])

      ## Get nthroot(diag(sigma)




      stats <- data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat)],
        ## Number of species
        ## total of all surveys
        species_total = mean(vapply(
          gf_list, \(gf){
            sum(gf$nspec)
          },
          numeric(1)
        )),
        ## mean of all surveys
        species_mean = mean(vapply(
          gf_list, \(gf){
            mean(gf$nspec)
          },
          numeric(1)
        )),
        species_mean_sd = mean(vapply(
          gf_list, \(gf){
            sd(gf$nspec)
          },
          numeric(1)
        )),

        ## Number of sites
        nsites_total = mean(vapply(
          gf_list, \(gf){
            sum(vapply(
              gf$X, \(x){
                nrow(x)
              },
              numeric(1)
            ))
          },
          numeric(1)
        )),
        nsites_mean = mean(vapply(
          gf_list, \(gf){
            mean(vapply(
              gf$X, \(x){
                nrow(x)
              },
              numeric(1)
            ))
          },
          numeric(1)
        )),
        nsites_sd = mean(vapply(
          gf_list, \(gf){
            sd(vapply(
              gf$X, \(x){
                nrow(x)
              },
              numeric(1)
            ))
          },
          numeric(1)
        )),

        ## Species R^2
        spec_r2_median = mean(vapply(
          gf_list, \(gf){
            median(gf$rsq)
          },
          numeric(1)
        )),
        spec_r2_mean = mean(vapply(
          gf_list, \(gf){
            mean(gf$rsq)
          },
          numeric(1)
        )),
        spec_r2_sd = mean(vapply(
          gf_list, \(gf){
            sd(gf$rsq)
          },
          numeric(1)
        )),
        ## Survey R^2
        survey_r2_mean = mean(vapply(
          gf_list, \(gf){
            sum(importance(gf))
          },
          numeric(1)
        )),
        survey_r2_sd = sd(vapply(
          gf_list, \(gf){
            sum(importance(gf))
          },
          numeric(1)
        )),

        ## Number of surveys
        nsurveys = mean(vapply(
          gf_list, \(gf){
            length(gf$X)
          },
          numeric(1)
        )),
        var_spheq = list(var_spheq),
        var_spheq_mean = mean(var_spheq),
        var_spheq_sd = sd(var_spheq),
        sd_spheq = list(sqrt(var_spheq)),
        sd_spheq_mean = mean(sqrt(var_spheq)),
        sd_spheq_sd = sd(sqrt(var_spheq)),
        cumimp_magnitude = mean(cumimp_magnitude),
        cumimp_magnitude_sd = sd(cumimp_magnitude),
        mean_similarity = mean_similarity
        )

  return(stats)

}






gfbootstrap_diagnostic_plots <- function(gfbootstrap_combined,
                                         gfbootstrap_diagnostics,
                                         plot_description,
                                         output_folder) {
  survey_specs <- gfbootstrap_combined[
    ,
    c(
      "env_domain",
      "trophic",
      "survey",
      "depth_cat"
    )
  ]
  survey_specs$depth_cat <- as.character(survey_specs$depth_cat)
  survey_specs <- as.character(survey_specs)
   pl_survey_name <- paste0(c(survey_specs, plot_description),
                                                 collapse = "_")

  pl_file_base <- file.path(output_folder, paste0(survey_specs, collapse = "_"))

  if (inherits(gfbootstrap_combined$gfbootstrap[[1]], "combinedBootstrapGF")) {
    gg_plots <- gfbootstrap::gg_combined_bootstrapGF(gfbootstrap_combined$gfbootstrap[[1]])
    file_names <- vapply(names(gg_plots), function(n, gg_plots, pl_file_base) {
      pl_file <- paste0(c(paste0(c(pl_file_base, n, plot_description), collapse = "_"), ".png"), collapse = "")
      ggsave_wrapper(filename = pl_file, plot = gg_plots[[n]])
      return(pl_file)
    }, character(1), gg_plots = gg_plots, pl_file_base = pl_file_base)
  } else if (inherits(gfbootstrap_combined$gfbootstrap[[1]], "bootstrapGradientForest")) {
    gg_plots <- gfbootstrap::gg_bootstrapGF(gfbootstrap_combined$gfbootstrap[[1]])
    file_names <- vapply(names(gg_plots), function(n, gg_plots, pl_file_base) {
      pl_file <- paste0(c(paste0(c(pl_file_base, n, plot_description), collapse = "_"), ".png"), collapse = "")
      ggsave_wrapper(filename = pl_file, plot = gg_plots[[n]])
      return(pl_file)
    }, character(1), gg_plots = gg_plots, pl_file_base = pl_file_base)
  } else {
    file_names <- paste0(c(paste0(c(pl_file_base, "nofit", plot_description), collapse = "_"), ".png"), collapse = "")
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(pl_survey_name, " has not successfully clustered"))
    ggsave_wrapper(filename = file_names, plot = no_plot)
  }
}
