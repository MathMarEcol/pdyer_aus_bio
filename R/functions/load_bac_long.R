#' Loads in microbe data,
#' adds survey and trophic identifiers,
#' and converts to long form
#' The final colNames are
#' c("survey", "trophic", "depth", "depth_cat", spatial_vars, "taxon", "abund")
load_bac_long <- function(
                          bac_otu_csv,
                          bac_site_csv,
                          spatial_vars,
                          depth_names,
                          depth_range
                          ) {

  bac_sites <- data.table::fread(
                                 bac_site_csv,
                                 sep = ",",
                                 select =c(
                                   "Sample ID",
                                   "Depth [m]",
                                   "Microbial Abundance [cells per ml]",
                                   "Date Sampled",
                                   "Latitude [decimal degrees]",
                                   "Longitude [decimal degrees]"),
                                 col.names = c(
                                   "Sample_ID",
                                   "depth",
                                   "Microbial.Abundance",
                                   "Date",
                                   spatial_vars[2],
                                   spatial_vars[1]),
                                 check.names = TRUE,
                                 key = c("Sample_ID"),
                                 data.table = TRUE,
                                 stringsAsFactors = FALSE)


  ## For reasons I don't understand, wrapping the fread call in microbenchmark was preventing failure
  ## fread takes ~270s, readr::read_csv take ~500s, but read_csv is more stable.
  ## fread required me to wrap it in a call to microbenchmark, which is non-obvious and sounds fragile.
  ## fread is working now. Maybe a bug was fixed.
  bac_otu <- data.table::fread(bac_otu_csv,
                               sep = ",",
                               select =1:3,
                               check.names = TRUE,
                               key = c("Sample.ID", "OTU"),
                               data.table = TRUE,
                               stringsAsFactors = FALSE)

  ## NA depths are not meaningful, drop
  ## Only 3 samples, all from Geoffrey bay, Magnetic Island.
  ## 286 other samples at identical lat and lon coordinate.
  ## due to data.table update-in-place rules, I can't do an
  ## in place update that changes the number of rows
  ## in the target dt, and multimatches are silently
  ## collapsed to one match!
  ## bac_otu[bac_sites,  c(spatial_vars, "depth")  := mget(paste0("i.", c(spatial_vars, "depth")))]
  bac_sites <- bac_sites[!is.na(depth)]
  bac_otu <- bac_otu[bac_sites, nomatch = NA]
  gc()
  sites_sample_id <- bac_sites$Sample_ID
  otu_sample_id <- bac_otu$Sample.ID
  bac_otu[ , c("survey", "trophic", "Sample.ID", "Microbial.Abundance", "Date") := .("bac", "microbe", NULL, NULL, NULL)]
  bac_otu[ , depth_cat := data.table::fcase(
                                    depth >= depth_range[[depth_names[1]]][1] & depth < depth_range[[depth_names[1]]][2], depth_names[1],
                                    depth >= depth_range[[depth_names[2]]][1] & depth < depth_range[[depth_names[2]]][2], depth_names[2],
                                    depth >= depth_range[[depth_names[3]]][1] & depth < depth_range[[depth_names[3]]][2], depth_names[3],
                                    default = NA
                                    )]
  data.table::setnames(bac_otu, c("OTU", "OTU.Count"), c("taxon", "abund"))

  if(length(setdiff(sites_sample_id, otu_sample_id)) != 0) {
    stop("Bacteria sites found with no taxa. Needs investigation and a No taxa category")
  }
  return(bac_otu)
}
