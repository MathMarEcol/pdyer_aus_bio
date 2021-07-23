merge_all_bio <- function(
                          dataset_list, ## all biological datasets in the proper form
                          regrid_res,
                          env_offset,
                          agg_fun,
                          spatial_vars
                          ) {



    data.table::rbindlist(
                  dataset_list,
                 use.names = TRUE
                ) -> all_bio_long

  all_bio_round <- all_bio_long[, {
  ## Aggregate biological samples into grid cells
    copy_SD <- data.table::copy(.SD)
    sites_round <- data.table::data.table(
                                 round((copy_SD$sites[[1]][, c(spatial_vars), with=FALSE]-env_offset) / regrid_res) * regrid_res +  env_offset)
    sites_round[, `:=`(site_id = copy_SD$sites[[1]]$site_id,
                       depth = copy_SD$sites[[1]]$depth)]
    ## Convert all longitudes to range [0, 360]
    sites_round[, c(spatial_vars[1]) := .(.SD[[spatial_vars[1]]] %% 360)]
  ## Clean up names
    taxa_clean <- data.table::data.table(taxon = clean_sp_names(.SD$taxa[[1]]$taxon), taxon_id = .SD$taxa[[1]]$taxon_id)

    out <- data.table(sites = list(sites_round), obs = list(.SD$obs[[1]]), taxa = list(taxa_clean))

    },
    by = c("trophic", "survey", "depth_cat")]

  ## Aggregate biological samples into grid cells
  #spatial_vars_raw <- paste0(spatial_vars, "_raw")
  ## all_bio_long is a data.table, using := updates in place GLOBALLY
  #all_bio_long[, c(spatial_vars_raw) := .SD, .SDcols = spatial_vars]
 # all_bio_long[, c(spatial_vars) := (round((.SD-env_offset) / regrid_res) * regrid_resolution +  env_offset), .SDcols = spatial_vars]
  ## ~25% memory savings, but confuses group iterations in targets
  ## grid_lut <- unique(all_bio_long[, c(spatial_vars, "depth_cat"), with=FALSE])
  ## grid_lut[ , grid_id := seq.int(1, nrow(grid_lut))]
  ## all_bio_long[grid_lut, grid_id := i.grid_id, on = c(spatial_vars, "depth_cat")]
  ## all_bio_long[, c(spatial_vars, "depth_cat", "depth") := NULL]

  ## Taxon lut takes up a lot of space, but total saving was small
  ## taxon_lut <- data.table(taxon = unique(all_bio_long[, taxon]))
  ## taxon_lut[ , taxon_id := seq.int(1, nrow(taxon_lut))]

  ## Aggregate abundances within grid cells
  ## all_bio_long_agg <- all_bio_long[, lapply(.SD, agg_fun),  by = c(grid_id, "survey", "trophic", "taxon")]
  ## all_bio_long_agg <- all_bio_long[, lapply(.SD, agg_fun),  by = c(spatial_vars, "depth_cat", "survey", "trophic", "taxon")]
## Now aggregating at the env merge stage
  ## Clean up names
 ## all_bio_long_agg[, taxon := clean_sp_names(taxon)]

  ## all_bio_test[, taxon := clean_sp_names(taxon)]
  ## all_bio_test[grid_lut, grid_id := i.grid_id,on = c(spatial_vars, "depth_cat")]
  ## all_bio_test[taxon_lut, taxon_id := i.taxon_id, on = "taxon"]
  ## all_bio_test[, c(spatial_vars, "depth_cat", "depth", "taxon") := NULL]

  ## ## Add grouping column for targets package
  ## all_bio_long_agg[,
  ##                 tar_group := .GRP,
  ##                 by = c("survey",  "trophic")]

  return(all_bio_round)

}

clean_sp_names <- function(x) {
length_pre <- length(unique(x))
## Remove punctuation. No collisions expected
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
