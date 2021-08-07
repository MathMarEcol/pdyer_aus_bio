fit_gfbootstrap <- function(all_bio_env,
                            env_biooracle_names,
                            gf_trees,
                            gf_compact,
                            gf_bins,
                            gf_corr_thres) {
  if (is.na(all_bio_env$wide_taxa_env)) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(all_bio_env[, .(env_domain, trophic, survey, depth_cat)],
      gfbootstrap = list(NA)
    ))
  }
  gf_safe <- purrr::possibly(gfbootstrap::bootstrapGradientForest, otherwise = NA, quiet = FALSE)
  gf_fit <- gf_safe(
    x = all_bio_env$wide_taxa_env[[1]],
    predictor.vars = env_biooracle_names,
    response.vars = unique(all_bio_env$obs_env[[1]]$taxon_id_chr),
    nbootstrap = gf_trees,
    compact = gf_compact,
    nbin = gf_bins,
    transform = NULL,
    corr.threshold = gf_corr_thres,
    maxLevel = floor(log2(length(unique(all_bio_env$obs_env[[1]]$taxon_id_chr)) * 0.368 / 2)),
    trace = TRUE
  )

  return(data.table(all_bio_env[, .(env_domain, trophic, survey, depth_cat)],
    gfbootstrap = gf_fit,
    is_combined = FALSE,
    frac_valid = as.numeric(!is.na(gf_fit))
  ))
}
