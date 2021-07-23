
## Assumes the data.table had the following structure:
#' c("survey", "trophic", "depth", "depth_cat", spatial_vars, "taxon", "abund")
##
## Also asusmes that this function was called something like:
  ## phy_rows <- phy_raw[,
  ##                     normalise_bio(.SD, spatial_vars),
  ##                     by = c("survey", "trophic",  "depth_cat")]
## returns a data.table with list cols: site, obs, taxa.
  ## Other cols are inclued in obs
  normalise_bio <- function(dt, spatial_vars) {

    obs <- data.table::copy(dt)
  sites <- unique(obs[, c(spatial_vars, "depth"), with=FALSE])
  sites[ , site_id := seq.int(1, nrow(sites))]
  obs[sites, site_id := i.site_id, on = c(spatial_vars, "depth")]
  obs[, c(spatial_vars, "depth") := NULL]

  taxa <- data.table::data.table(taxon = unique(obs[, taxon]))
  taxa[ , taxon_id := seq.int(1, nrow(taxa))]
  obs[taxa, taxon_id := i.taxon_id, on = "taxon"]
  obs[ , taxon := NULL]

  return(data.table(sites = list(sites),
                    taxa = list(taxa),
                    obs = list(obs)
                    )
         )
}
