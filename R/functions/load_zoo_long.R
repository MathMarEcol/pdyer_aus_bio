#' Loads in zooplankton data,
#' adds survey and trophic identifiers,
#' and converts to long form
#' The final colNames are
#' c("survey", "trophic", "depth", "depth_cat", spatial_vars, "taxon", "abund")
load_zoo_long <- function(
                          zoo_load_script,
                          zoo_data_dir,
                          zoo_matching,
                          zoo_names,
                          spatial_vars,
                          depth_names,
                          <#part type="application/octet-stream" filename=clean_sp_names.R disposition=attachment>
<#/part>
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
  zoo_raw[, `:=`(SampleDateUTC = NULL,
            TaxonGroup = NULL,
            ProjectNumber = NULL)]



  return(zoo_raw)
}
  ## ##do this for global list
  ## zoo_raw[ , .(Species := clean_sp_names(Species))]

  ## ##Do this AFTER merging with env
  ## ##Find species that were not consistently sampled
  ## zoo_raw[
  ## get_uncertain_sp_names(zooplank_surv, -999, "Abund_m3"),

  ## ##Before converting to wide, extract list of species names, but after global list
