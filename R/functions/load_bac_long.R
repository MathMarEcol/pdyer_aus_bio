#' Loads in microbe data,
#' adds survey and trophic identifiers,
#' and converts to long form
#' The final colNames are
#' c("survey", "trophic", "depth" spatial_vars, "taxon", "abund")
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

  ## For reasons I don't understand, wrapping the fread call in microbenchmark prevents failure
  dt_time <- microbenchmark::microbenchmark({
    ## micro_dt <- data.table::fread(microbe_bacteria_csv, sep = ",", select =1:3, check.names = TRUE, key = c("Sample.ID", "OTU" ), data.table = TRUE, stringsAsFactors = FALSE)
    bac_otu <- data.table::fread(bac_otu_csv,
                             sep = ",",
                             select =1:3,
                             check.names = TRUE,
                             key = c("Sample.ID", "OTU"),
                             data.table = TRUE,
                             stringsAsFactors = FALSE)
  }, times= 1)

  bac_otu[bac_sites,  c(spatial_vars, "depth")  := mget(paste0("i.", c(spatial_vars, "depth")))]
  bac_otu[ , c("survey", "trophic", "Sample.ID") := .("bac", "microbe", NULL)]

  bac_otu[ , c("survey", "trophic", "Sample.ID") := .("bac", "microbe", NULL)]
  setnames(bac_otu, c("OTU", "OTU.Count"), c("taxon", "abund"))



  return(bac_otu)
}
