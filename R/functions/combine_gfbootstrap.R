combine_gfbootstrap_p1 <- function(
                                   gfbootstrap_survey,
                                   custom_combinations) {

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
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic", "survey")],

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
                  gfbootstrap =
                    list(
                      stats::setNames(gfbootstrap,
                                      nm = surv_full_name)
                    ),
                  survey = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "depth_cat")],

    gfboot_surv[,
                .(
                  gfbootstrap =
                    list(
                      stats::setNames(gfbootstrap,
                                      nm = surv_full_name)
                    ),
                  depth_cat = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "survey")],

    gfboot_surv[,
                .(
                  gfbootstrap =
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
                  gfbootstrap =
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
                  gfbootstrap =
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
  out[sapply(out$gfbootstrap, is.null), gfbootstrap := NA]

  out <- rbindlist(use.names = TRUE,
    list(out,
    rbindlist(lapply(
      custom_combinations,
      \(x, gfbootstrap_survey){
        custom_combinations_helper(x, gfbootstrap_survey)
      },
      gfbootstrap_survey = gfbootstrap_survey
    )
  )
  ))


    out[, surv_full_name := apply(.SD, 1, function(x){paste0(x, collapse = "__")}), .SDcols = surv_cols]
    out <- out[vapply(out$gfbootstrap, length, integer(1)) > 1 & frac_valid > 0,]

  out <- rbind(out, gfbootstrap_survey)
  return(out)
}

combine_gfbootstrap_p2 <- function(gfbootstrap_combined_tmp,
                                   gf_bins,
                                   gf_bootstrap_combinations) {

    if (inherits(future::plan(), "sequential")) {
      ## We are in a tar_make_clustermq worker
      ## which has not propagated the future plan
      oplan <- future::plan(future.callr::callr, workers = as.numeric(Sys.getenv("FUTURE_WORKERS", "1")))
      on.exit(future::plan(oplan))
    }

    gfb <- lapply(
      gfbootstrap_combined_tmp$gfbootstrap[[1]],
      \(gf_file) {
        if (is.na(gf_file)) {
          return(NA)
        } else {
          return(qs::qread(gf_file))
        }
      }
    )

  if (all(is.na(gfb))) {
    return(data.table(gfbootstrap_combined_tmp[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
      gfbootstrap = NA
    ))
  } else if (gfbootstrap_combined_tmp$is_combined) {
      gfb <- gfb[!is.na(gfb)]
      combine_args <- c(
      nbin = gf_bins,
      n_samp = gf_bootstrap_combinations,
      gfb
    )
    out <- do.call(gfbootstrap::combinedBootstrapGF, combine_args)
    for (i in seq_along(out$gf_list)) {
      out$gf_list[[i]]$call <- NULL
    }

    ## save the gfbootstrap objects into the targets cache
    ## To make operating over all gfbootstrap objects more
    ## managable from a memory perspective.
    hashed <- stringr::str_sub(digest::digest(out), 1, 8)
    outdir <- file.path(targets::tar_path_store(), "gfbootstraps")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    surv_cols <- c("env_domain", "trophic", "survey", "depth_cat")
    surv_full_names <- apply(gfbootstrap_combined_tmp[, ..surv_cols], 1, function(x){paste0(x, collapse = "__")})

    outfile <- file.path(outdir, paste0("combinedgfbootstrap_", hashed, "___", surv_full_names, ".qs"))

    qs::qsave(out, outfile, "high")


    return(data.table(gfbootstrap_combined_tmp[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
      gfbootstrap = outfile
    ))
  } else {
    return(
      data.table(gfbootstrap_combined_tmp[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
        gfbootstrap = list(gfbootstrap_combined_tmp$gfbootstrap[[1]])
      )
    )
  }
}

## combine_gfbootstrap_p2 expects a data.table that has
## the description columns and a list of gf objects to combine.

## The custom combinations should have a clear description for the
## description column (eg survey == "imos" for all phy and zoo datasets)
## and a set of character vectors for matching gf objects.
## Character vectors that are NA will match all values.

## Two data.tables.
## one data.table is the descriptions.
## The other data.table is the matching sets.
## Use CJ to get all combinations



## Provide a data.table that can be matched

## Takes a list of character vectors `gf_custom' where
## names(gf_custom) -> c("trophic", "depth_cat", "survey")
## For each vector, specify the
custom_combinations_helper <- function(
                                       custom_row,
                                       gfbootstrap_survey
                                       ) {

    gf_combined_pre <- gfbootstrap_survey[,
                                          by = env_domain, {
              gf_obs <- .SD[custom_row$matches, on = .NATURAL, nomatch = NULL]
              frac_valid <- sum(!is.na(gf_obs$gfbootstrap)) / length(gf_obs$gfbootstrap)

              if (length(gf_obs$gfbootstrap) > 1) {
                  is_combined <- TRUE
              } else {
                  is_combined <- FALSE
              }
              data.table::data.table(custom_row$descriptions,
                                     gfbootstrap = list(gf_obs$gfbootstrap),
                                     is_combined = is_combined,
                                     frac_valid = frac_valid
                                     )

                                          }
              ]

}
