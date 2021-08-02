#' Loads in zooplankton data,
#' adds survey and trophic identifiers,
#' and converts to long form
#' The final colNames are
#' c("survey", "trophic", "depth", "depth_cat", spatial_vars, "taxon", "abund")
#' New approach:
#' data.table with list cols
#' each row contains "trophic", "survey" and "depth_cat" character cols as grouping variables
#' and samples, obs and taxa list cols.
#' taxa contains taxon and taxon_id cols
#' obs contains abund samp_id taxon_id
#' samps contains lon   lat samp_id depth
load_zoo_long <- function(
                          zoo_load_script,
                          zoo_data_dir,
                          zoo_matching,
                          zoo_names,
                          spatial_vars,
                          depth_names,
                          depth_range
                          ) {
  source(zoo_load_script)
  zoo_raw <- load_zoo_data(zoo_data_dir)
  setDT(zoo_raw)
  for (surv in zoo_names) {
    zoo_raw[ProjectNumber %in% zoo_matching[[surv]],
            survey := surv]
  }
  zoo_raw <- zoo_raw[survey %in% zoo_names,]
  new_names <- tibble::tribble(
                       ~old, ~new,
                       "Longitude", spatial_vars[1],
                       "Latitude",spatial_vars[2],
                       "Species", "taxon",
                       "ZAbund_m3", "abund"
                       )
  setnames(zoo_raw,
              new_names$old,
              new_names$new
              )

  zoo_raw[,
          c("trophic", "depth", "depth_cat") :=
               .("zoo",
                 0,
                 depth_names[1]  ## all samples are epipelagic
               )
          ]
  zoo_raw[, `:=`(
            TaxonGroup = NULL,
            ProjectNumber = NULL)]


    obs <- data.table::copy(zoo_raw)
  obs[is.na(abund), abund := 0]
  samps <- unique(obs[, c(spatial_vars, "depth", "depth_cat", "SampleDateUTC"), with=FALSE])
  samps[ , samp_id := seq.int(1, nrow(samps))]
  obs[samps, samp_id := i.samp_id, on = c(spatial_vars, "depth", "depth_cat", "SampleDateUTC")]
  obs[, c(spatial_vars, "depth", "SampleDateUTC") := NULL]

  taxa <- data.table::data.table(taxon = unique(obs[, taxon]))
  taxa[ , taxon_id := seq.int(1, nrow(taxa))]
  obs[taxa, taxon_id := i.taxon_id, on = "taxon"]
  obs[ , taxon := NULL]

  zoo_out <- obs[, {
    obs_local <- .SD
    samps_local <- samps[samp_id %in% unique(obs_local$samp_id)]
    samps_local[, depth_cat := NULL]
    taxa_local <- taxa[taxon_id %in% unique(obs_local$taxon_id)]
    data.table::data.table(samps = list(samps_local),
                    taxa = list(taxa_local),
                    obs = list(obs_local)
                    )
    },
    by = c("survey", "trophic", "depth_cat")]



## SampleDateUTC = NULL,
##   zoo_rows <- zoo_raw[,
##                       normalise_bio(.SD, spatial_vars),
##                       by = c("survey", "trophic",  "depth_cat")]

  return(zoo_out)
}
  ## ##do this for global list
  ## zoo_raw[ , .(Species := clean_sp_names(Species))]

  ## ##Do this AFTER merging with env
  ## ##Find species that were not consistently sampled
  ## zoo_raw[
  ## get_uncertain_sp_names(zooplank_surv, -999, "Abund_m3"),

  ## ##Before converting to wide, extract list of species names, but after global list
