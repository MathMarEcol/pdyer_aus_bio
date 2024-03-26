# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
#' Loads in microbe data,
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



  ## NA depths are not meaningful, drop
  ## Only 3 samples, all from Geoffrey bay, Magnetic Island.
  ## 286 other samples at identical lat and lon coordinate.
  ## due to data.table update-in-place rules, I can't do an
  ## in place update that changes the number of rows
  ## in the target dt, and multimatches are silently
  ## collapsed to one match!
  ## bac_otu[bac_sites,  c(spatial_vars, "depth")  := mget(paste0("i.", c(spatial_vars, "depth")))]
  ## These are all in shallow coastal waters, setting depth to 0
  bac_sites[Sample_ID %in% c("102.100.100/34923", "102.100.100/34924", "102.100.100/34925"), depth := 0]
  bac_sites <- bac_sites[!is.na(depth)]
  bac_sites[ , depth_cat := data.table::fcase(
                                    depth >= depth_range[[depth_names[1]]][1] & depth < depth_range[[depth_names[1]]][2], depth_names[1],
                                    depth >= depth_range[[depth_names[2]]][1] & depth < depth_range[[depth_names[2]]][2], depth_names[2],
                                    depth >= depth_range[[depth_names[3]]][1] & depth < depth_range[[depth_names[3]]][2], depth_names[3],
                                    default = NA
                                    )]
  bac_sites[, c("Microbial.Abundance", "Date") := NULL]
  bac_sites[, `:=`(samp_id = seq.int(1, nrow(bac_sites)))]
  ## n_total <- bac_sites[, .(n_total = nrow(.SD)), by = c("depth_cat", spatial_vars)]
  ## bac_sites[n_total, on = c("depth_cat", spatial_vars), n_total := i.n_total ]
  ## unique_sites <- unique(bac_sites[,c(spatial_vars, "depth_cat"), with = FALSE])
  ## unique_sites[, site_id := seq.int(1, nrow(unique_sites))]
  ## sample_sites <- unique_sites[bac_sites, on = c(spatial_vars, "depth_cat"), c("site_id", "Sample_ID", spatial_vars, "depth_cat" ), with=FALSE ]
  ## bac_sites[sample_sites, on = "Sample_ID"]


  ## Depths are not unique at a site, merge
  ## unique(sample_sites[bac_sites, on = "Sample_ID"][,.(site_id, depth)])

  ## bac_sites[unique_sites, by = c("Sample_ID", spatial_vars), with = FALSE])



  ## bac_sites[ ,
  ##           `:=`(Sample_ID_list = list(c(Sample_ID)),
  ##                n_total = unique(n_total) ## should break if logic is wrong above
  ##                Sample_ID = NULL
  ##                ),
  ##           by = c("depth_cat", spatial_vars)]

  ## For reasons I don't understand, wrapping the fread call in microbenchmark was preventing failure
  ## fread takes ~270s, readr::read_csv take ~500s, but read_csv is more stable.
  ## fread required me to wrap it in a call to microbenchmark, which is non-obvious and sounds fragile.
  ## fread is working now. Maybe a bug was fixed.
  bac_otu <- data.table::fread(bac_otu_csv,
                               sep = ",",
                               select =1:3,
                               check.names = TRUE,
                               col.names = c(
                                 "Sample_ID",
                                 "taxon",
                                 "abund"
                                 ),
                               key = c("Sample_ID", "taxon"),
                               data.table = TRUE,
                               stringsAsFactors = FALSE)

  out <- purrr::map_df(depth_names, ~{
    samps <- bac_sites[depth_cat == .x, -c("depth_cat")]
    if( nrow(samps) == 0) {
      return( data.table::data.table(trophic = "microbe",
                                survey = "bac",
                                depth_cat = .x,
                                obs = NA,
                                samps = NA,
                                taxa = NA)
             )
    }
    obs <- bac_otu[samps, .(samp_id, taxon, abund), on = "Sample_ID", nomatch = NULL]
    if( nrow(obs) == 0) {
      return( data.table::data.table(trophic = "microbe",
                                survey = "bac",
                                depth_cat = .x,
                                obs = NA,
                                samps = NA,
                                taxa = NA)
             )
    }
    taxa <- data.table::data.table(taxon = unique(obs[, taxon]))
    taxa[ , taxon_id := seq.int(1, nrow(taxa))]
    obs[taxa, taxon_id := i.taxon_id, on = "taxon"]
    obs[ , taxon := NULL]
    samps[, Sample_ID := NULL]

  return(data.table::data.table(trophic = "microbe",
                                survey = "bac",
                                depth_cat = .x,
                                obs = list(obs),
                                samps = list(samps),
                                taxa = list(taxa)
                                )
         )
    })






  ## bac_otu <- bac_otu[bac_sites, nomatch = NA]

  ## gc()
  ## sites_sample_id <- bac_sites$Sample_ID
  ## otu_sample_id <- bac_otu$Sample.ID
  ## bac_otu[ , c("survey", "trophic", "Sample.ID", "Microbial.Abundance", "Date") := .("bac", "microbe", NULL, NULL, NULL)]
  ## bac_otu[ , depth_cat := data.table::fcase(
  ##                                   depth >= depth_range[[depth_names[1]]][1] & depth < depth_range[[depth_names[1]]][2], depth_names[1],
  ##                                   depth >= depth_range[[depth_names[2]]][1] & depth < depth_range[[depth_names[2]]][2], depth_names[2],
  ##                                   depth >= depth_range[[depth_names[3]]][1] & depth < depth_range[[depth_names[3]]][2], depth_names[3],
  ##                                   default = NA
  ##                                   )]
  ## data.table::setnames(bac_otu, c("OTU", "OTU.Count"), c("taxon", "abund"))

  ## if(length(setdiff(sites_sample_id, otu_sample_id)) != 0) {
  ##   stop("Bacteria sites found with no taxa. Needs investigation and a No taxa category")
  ## }



  ## bac_rows <- bac_otu[,
  ##                     normalise_bio(.SD, spatial_vars),
  ##                     by = c("survey", "trophic",  "depth_cat")]
  return(out)
}
