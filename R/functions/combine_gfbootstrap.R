combine_gfbootstrap_p1 <- function(
                                gfbootstrap_survey
                                ) {

  surv_cols <- c("env_domain", "trophic", "survey", "depth_cat")

  gfboot_surv <- data.table::copy(gfbootstrap_survey)


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
                .(
                  gfbootstrap_list =
                    list(
                      stats::setNames(gfbootstrap,
                                      nm = surv_full_name)
                    ),
                  survey = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "depth_cat")],

    gfboot_surv[,
                .(
                  gfbootstrap_list =
                    list(
                      stats::setNames(gfbootstrap,
                                      nm = surv_full_name)
                    ),
                survey = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic")],

    gfboot_surv[,
                .(
                  gfbootstrap_list =
                    list(
                      stats::setNames(gfbootstrap,
                                      nm = surv_full_name)
                    ),
                survey = "all",
                trophic = "all" ,
                is_combined = TRUE),
                by = c("env_domain", "depth_cat")],

    gfboot_surv[,
                .(
                  gfbootstrap_list =
                    list(
                      stats::setNames(gfbootstrap,
                                      nm = surv_full_name)
                    ),
                survey = "all",
                trophic = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain")]
  )
  out <- gfboot_combined[fraction_valid, on = c(surv_cols, "is_combined")]
  out[sapply(out$gfbootstrap_list, is.null), gfbootstrap_list := NA]
  out[, surv_full_name := apply(.SD, 1, function(x){paste0(x, collapse = "__")}), .SDcols = surv_cols]
  return(out)
}

combine_gfbootstrap_p2 <- function(
                                   gfbootstrap_combined_p1,
                                   gf_bins,
                                   gf_trees
                                   ) {
  #Recommended way to modify plan locally
  oplan <- future::plan(future::sequential)
  on.exit(future::plan(oplan))

  gfb <- gfbootstrap_combined_p1$gfbootstrap_list[[1]]
  print(names(gfb))
  if(is.na(gfb) | length(gfb) < 2) {
    return(data.table(gfbootstrap_combined_p1[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
                      gfbootstrap = list(NA))
           )
  } else {
    combine_args <- c(nbin = gf_bins, n_samp = gf_trees, gfb)
    out <- do.call(gfbootstrap::combinedBootstrapGF, combine_args)
    for (i in seq_along(out$gf_list)) {
      out$gf_list[[i]]$call <- NULL
    }
    return(data.table(gfbootstrap_combined_p1[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
                      gfbootstrap = list(out))
           )
  }
}
