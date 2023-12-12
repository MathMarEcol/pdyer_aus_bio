fit_gf <- function(
                   all_bio_env,
                   env_biooracle_names,
                   gf_trees,
                   gf_compact,
                   gf_bins,
                   gf_corr_thres
                   ) {
  if (all(is.na(all_bio_env$wide_taxa_env))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(all_bio_env[,.(env_domain, trophic, survey, depth_cat)],
                      gf = list(NA)
                      ))
  }
  gf_safe <- purrr::possibly(gradientForest::gradientForest, otherwise = NA, quiet = FALSE)
  gf_fit <- gf_safe(
    data = all_bio_env$wide_taxa_env[[1]],
    predictor.vars = env_biooracle_names,
    response.vars = unique(all_bio_env$obs_env[[1]]$taxon_id_chr),
    ntree = gf_trees,
    compact = gf_compact,
    nbin = gf_bins,
    transform = NULL,
    corr.threshold = gf_corr_thres,
    maxLevel = floor(log2(length(unique(all_bio_env$obs_env[[1]]$taxon_id_chr)) * 0.368 / 2)),
    trace = TRUE
  )

  ## save the gf objects into the targets cache
  ## To make operating over all gf objects more
  ## managable from a memory perspective.
  hashed <- stringr::str_sub(digest::digest(gf_fit), 1, 8)
  outdir <- file.path(targets::tar_path_store(), "gfs")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  outfile <- file.path(outdir, paste0("gf_", hashed, "___", surv_full_names, ".qs"))

  qs::qsave(gf_fit, outfile, "high")

  return(data.table::data.table(all_bio_env[, .(env_domain, trophic, survey, depth_cat)],
                                gf = outfile,
                                is_combined = FALSE,
                                surv_full_name = surv_full_names,
                                frac_valid = as.numeric(all(!is.na(gf_fit)))
                                ))

}
