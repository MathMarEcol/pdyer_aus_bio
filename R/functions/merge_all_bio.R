
merge_all_bio <- function(
                          ..., ## all biological datasets in the proper form

      spatial_vars
                          ) {



    data.table::rbindlist(
                  list(...),
                 use.names = TRUE
                )[,
                  tar_group := .GRP,
                  by = c("survey",  "trophic")] %>%
     data.table::setkeyv(spatial_vars) ->
      all_bio_long

  ## Aggregate biological samples into grid cells
  spatial_vars_raw <- paste0(spatial_vars, "_raw")
  ## all_bio_long is a data.table, using := updates in place GLOBALLY
  #all_bio_long[, c(spatial_vars_raw) := .SD, .SDcols = spatial_vars]
  all_bio_long[, c(spatial_vars) := round(.SD / regrid_res + env_offset), .SDcols = spatial_vars]

  ## Aggregate abundances within grid cells
  all_bio_long_agg <- all_bio_long[, lapply(.SD, agg_fun),  by = c(spatial_vars, "survey", "trophic", "depth_cat", "taxon")]

  ## Clean up names
  all_bio_long_agg[, taxon := clean_sp_names(taxon)]

  return(all_bio_long_agg)

}

clean_sp_names <- function(x) {
length_pre <- length(unique(x))
x %>%
    stringr::str_replace_all("\\(.*\\)", "") %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    stringr::str_replace_all("/", "") %>%
    stringr::str_replace_all("__", "_") -> y
length_post <- length(unique(y))
assertthat::assert_that(length_pre == length_post)
return(y)
}
