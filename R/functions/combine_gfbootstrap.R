combine_gfbootstrap <- function(
                                gfbootstrap_survey,
gf_bins,
gf_trees
                                ) {

  surv_cols <- c("env_domain", "trophic", "survey", "depth_cat")

gfboot_surv <- data.table::copy(gfbootstrap_survey)

  gfboot_surv[,surv_full_name := apply(.SD, 1, function(x){paste0(x, collapse = "__")}), .SDcols = surv_cols]
  all_surveys <- gfboot_surv[,.SD, .SDcols = c(surv_cols, "surv_full_name")]

  fraction_valid <-  rbind(
    gfboot_surv[,
                .(frac_valid = sum(!is.na(gfbootstrap))/.N,
                  survey = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "depth_cat")],

    gfboot_surv[,
                .(frac_valid = sum(!is.na(gfbootstrap))/.N,
                survey = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic")],

    gfboot_surv[,
                .(frac_valid = sum(!is.na(gfbootstrap))/.N,
                survey = "all",
                trophic = "all" ,
                is_combined = TRUE),
                by = c("env_domain", "depth_cat")],

    gfboot_surv[,
                .(frac_valid = sum(!is.na(gfbootstrap))/.N,
                survey = "all",
                trophic = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain")]
    )


  gfboot_surv <- gfboot_surv[!is.na(gfbootstrap)]

  gfboot_combined <- rbind(
    gfboot_surv[,
                .(gfbootstrap = bootstrap_with_names(gf_bins,
                                 gf_trees,
                                 surv_full_name,
                                 gfbootstrap) ,
                survey = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic", "depth_cat")],

    gfboot_surv[,
                .(gfbootstrap = bootstrap_with_names(gf_bins,
                                 gf_trees,
                                 surv_full_name,
                                 gfbootstrap) ,
                survey = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic")],

    gfboot_surv[,
                .(gfbootstrap = bootstrap_with_names(gf_bins,
                                 gf_trees,
                                 surv_full_name,
                                 gfbootstrap) ,
                survey = "all",
                trophic = "all" ,
                is_combined = TRUE),
                by = c("env_domain", "depth_cat")],

    gfboot_surv[,
                .(gfbootstrap = bootstrap_with_names(gf_bins,
                                 gf_trees,
                                 surv_full_name,
                                 gfbootstrap) ,
                survey = "all",
                trophic = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain")]
  )
  out <- gfboot_combined[fraction_valid, on = c(surv_cols, "is_combined")]
  out[sapply(out$gfbootstrap, is.null), gfbootstrap := NA]
  out <- rbind(out, gfbootstrap_survey)
  return(out)
}

bootstrap_with_names <- function(gf_bins,
                                 gf_trees,
                                 surv_full_name,
                                 gfbootstrap_ob) {
  print(surv_full_name)
  if (length(surv_full_name) > 1) {
    names(gfbootstrap_ob) <- surv_full_name
    combine_args <- c(nbin = gf_bins, n_samp = gf_trees, gfbootstrap_ob )
    out <- list(do.call(gfbootstrap::combinedBootstrapGF, combine_args))
    return(out)
  } else {
    return(list(NA))
  }
}
