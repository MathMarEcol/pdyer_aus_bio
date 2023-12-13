fit_gfbootstrap <- function(all_bio_env,
                            env_biooracle_names,
                            gf_trees,
                            gf_bootstrap_iters,
                            gf_compact,
                            gf_bins,
                            gf_corr_thres) {
    surv_cols <- c("env_domain", "trophic", "survey", "depth_cat")
    surv_full_names <- apply(all_bio_env[, ..surv_cols], 1, function(x){paste0(x, collapse = "__")})
    if (all(is.na(all_bio_env$wide_taxa_env))) {
        ## Upstream target decided survey was not usable.
        ## Propagating
        ##
        return(data.table(all_bio_env[, .(env_domain, trophic, survey, depth_cat)],
                          gfbootstrap = NA,
                          is_combined = FALSE,
                          surv_full_name = surv_full_names,
                          frac_valid = 0
                          ))
    }
    gf_safe <- purrr::possibly(gfbootstrap::bootstrapGradientForest, otherwise = NA, quiet = FALSE)
    gf_fit <- gf_safe(
        x = all_bio_env$wide_taxa_env[[1]],
        predictor.vars = env_biooracle_names,
        response.vars = unique(all_bio_env$obs_env[[1]]$taxon_id_chr),
        nbootstrap = gf_bootstrap_iters,
        compact = gf_compact,
        nbin = gf_bins,
        transform = NULL,
        corr.threshold = gf_corr_thres,
        maxLevel = floor(log2(length(unique(all_bio_env$obs_env[[1]]$env_id)) )),
        trace = TRUE,
        trees_per_iter = gf_trees
    )

    ## save the gfbootstrap objects into the targets cache
    ## To make operating over all gfbootstrap objects more
    ## managable from a memory perspective.
    hashed <- stringr::str_sub(digest::digest(gf_fit), 1, 8)
    outdir <- file.path(targets::tar_path_store(), "gfbootstraps")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

    outfile <- file.path(outdir, paste0("gfbootstrap_", hashed, "___", surv_full_names, ".qs"))

    qs::qsave(gf_fit, outfile, "high")

    return(data.table::data.table(all_bio_env[, .(env_domain, trophic, survey, depth_cat)],
                                  gfbootstrap = outfile,
                                  is_combined = FALSE,
                                  surv_full_name = surv_full_names,
                                  frac_valid = as.numeric(all(!is.na(gf_fit)))
                                  ))
}
