#' Loads in phytoplankton data,
#' adds survey and trophic identifiers,
#' and converts to long form
#' The final colNames are
#' c("survey", "trophic", "depth", spatial_vars, "taxon", "abund")
load_phy_long <- function(
                          phy_load_script,
                          phy_data_dir,
                          phy_matching,
                          phy_names,
                          spatial_vars,
                          depth_names,
                          depth_range
                          ) {
  source(phy_load_script)
  phy_raw <- load_phyto_data(phy_data_dir)
  setDT(phy_raw)
  phy_raw[ , `:=`(abund = Cells_L*1000,
                  Cells_L = NULL,
               trophic = "phy")]
  for (surv in phy_names) {
    phy_raw[ProjectNumber %in% phy_matching[[surv]],
            survey := surv]
  }
  phy_raw <- phy_raw[survey %in% phy_names,]
  new_names <- tibble::tribble(
                       ~old, ~new,
                       "Longitude", spatial_vars[1],
                       "Latitude",spatial_vars[2],
                       "TaxonName", "taxon"
                       )
  setnames(phy_raw,
              new_names$old,
              new_names$new
              )
  phy_raw[ , `:=`(
    depth = 0,
    SampleDateUTC = NULL,
    ProjectNumber = NULL)]

  return(phy_raw)
}

  ## ##do this for global list
  ## phy_raw[ , .(Species := clean_sp_names(Species))]

  ## ##Do this AFTER merging with env
  ## ##Find species that were not consistently sampled
  ## phy_raw[
  ## get_uncertain_sp_names(phyplank_surv, -999, "Abund_m3"),

  ## ##Before converting to wide, extract list of species names, but after global list