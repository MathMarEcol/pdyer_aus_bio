combine_gf_p1 <- function(
                                   gf_survey,
                                   custom_combinations) {

    surv_cols <- c("env_domain", "trophic", "survey", "depth_cat")

  gf_surv <- data.table::copy(gf_survey)


  fraction_valid <-  rbind(
    gf_surv[,
                .(frac_valid = sum(!is.na(gf))/.N,
                  survey = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "depth_cat")],

    gf_surv[,
                .(frac_valid = sum(!is.na(gf))/.N,
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic", "survey")],

    gf_surv[,
                .(frac_valid = sum(!is.na(gf))/.N,
                survey = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic")],

    gf_surv[,
                .(frac_valid = sum(!is.na(gf))/.N,
                survey = "all",
                trophic = "all" ,
                is_combined = TRUE),
                by = c("env_domain", "depth_cat")],

    gf_surv[,
                .(frac_valid = sum(!is.na(gf))/.N,
                survey = "all",
                trophic = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain")]
    )


  gf_surv <- gf_surv[!is.na(gf)]

  gf_combined <- rbind(
    gf_surv[,
                .(
                  gf =
                    list(
                      stats::setNames(gf,
                                      nm = surv_full_name)
                    ),
                  survey = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "depth_cat")],

    gf_surv[,
                .(
                  gf =
                    list(
                      stats::setNames(gf,
                                      nm = surv_full_name)
                    ),
                  depth_cat = "all",
                  is_combined = TRUE),
                by = c("env_domain", "trophic", "survey")],

    gf_surv[,
                .(
                  gf =
                    list(
                      stats::setNames(gf,
                                      nm = surv_full_name)
                    ),
                survey = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain", "trophic")],

    gf_surv[,
                .(
                  gf =
                    list(
                      stats::setNames(gf,
                                      nm = surv_full_name)
                    ),
                survey = "all",
                trophic = "all" ,
                is_combined = TRUE),
                by = c("env_domain", "depth_cat")],

    gf_surv[,
                .(
                  gf =
                    list(
                      stats::setNames(gf,
                                      nm = surv_full_name)
                    ),
                survey = "all",
                trophic = "all",
                depth_cat = "all",
                is_combined = TRUE),
                by = c("env_domain")]
  )
  out <- gf_combined[fraction_valid, on = c(surv_cols, "is_combined")]
  out[sapply(out$gf, is.null), gf := NA]

  out <- rbindlist(use.names = TRUE,
    list(out,
    rbindlist(lapply(
      custom_combinations,
      \(x, gf_survey){
        custom_combinations_helper(x, gf_survey)
      },
      gf_survey = gf_survey
    )
  )
  ))


    out[, surv_full_name := apply(.SD, 1, function(x){paste0(x, collapse = "__")}), .SDcols = surv_cols]
    out <- out[vapply(out$gf, length, integer(1)) > 1 & frac_valid > 0,]

  out <- rbind(out, gf_survey)
  return(out)
}

combine_gf_p2 <- function(gf_combined_tmp,
                                   gf_bins,
                                   ) {

    if (inherits(future::plan(), "sequential")) {
      ## We are in a tar_make_clustermq worker
      ## which has not propagated the future plan
      oplan <- future::plan(future.callr::callr, workers = as.numeric(Sys.getenv("FUTURE_WORKERS", "1")))
      on.exit(future::plan(oplan))
    }

    gfs <- lapply(
      gf_combined_tmp$gf[[1]],
      \(gf_file) {
        if (is.na(gf_file)) {
          return(NA)
        } else {
          return(qs::qread(gf_file))
        }
      }
    )

  if (all(is.na(gfs))) {
    return(data.table(gf_combined_tmp[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
      gf = NA
    ))
  } else if (gf_combined_tmp$is_combined) {
      gfs <- gfs[!is.na(gfs)]
      combine_args <- c(
      nbin = gf_bins,
      gfs
    )
    out <- do.call(gradientForest::combinedGradientForest, combine_args)
    rm(combine_args)
    rm(gfs)
    for (i in seq_along(out$gf_list)) {
      out$gf_list[[i]]$call <- NULL
    }

    ## save the gf objects into the targets cache
    ## To make operating over all gf objects more
    ## managable from a memory perspective.
    hashed <- stringr::str_sub(digest::digest(out), 1, 8)
    outdir <- file.path(targets::tar_path_store(), "gfs")
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    surv_cols <- c("env_domain", "trophic", "survey", "depth_cat")
    surv_full_names <- apply(gf_combined_tmp[, ..surv_cols], 1, function(x){paste0(x, collapse = "__")})

    outfile <- file.path(outdir, paste0("combinedgf_", hashed, "___", surv_full_names, ".qs"))

    qs::qsave(out, outfile, "high")


    return(data.table(gf_combined_tmp[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
      gf = outfile
    ))
  } else {
    return(
      data.table(gf_combined_tmp[, .(env_domain, trophic, depth_cat, survey, is_combined, frac_valid, surv_full_name)],
        gf = list(gf_combined_tmp$gf[[1]])
      )
    )
  }
}

## combine_gf_p2 expects a data.table that has
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
                                       gf_survey
                                       ) {

    gf_combined_pre <- gf_survey[,
                                          by = env_domain, {
              gf_obs <- .SD[custom_row$matches, on = .NATURAL, nomatch = NULL]
              frac_valid <- sum(!is.na(gf_obs$gf)) / length(gf_obs$gf)

              if (length(gf_obs$gf) > 1) {
                  is_combined <- TRUE
              } else {
                  is_combined <- FALSE
              }
              data.table::data.table(custom_row$descriptions,
                                     gf = list(gf_obs$gf),
                                     is_combined = is_combined,
                                     frac_valid = frac_valid
                                     )

                                          }
              ]

}
