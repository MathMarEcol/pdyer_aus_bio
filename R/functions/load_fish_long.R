#' Loads in fish data,
#' adds survey and trophic identifiers,
#' and converts to long form
#' The final colNames are
#' c("survey", "trophic", "depth", "depth_cat", spatial_vars, "taxon", "abund")
load_fish_long <- function(
                          fish_taxon_file,
                          fish_data_dir,
                          fish_years,
                          spatial_vars,
                          depth_names,
                          depth_range,
                          regrid_resolution,
                          env_offset
                          ) {

   fish_taxon  <- data.table::setDT(readRDS(fish_taxon_file), key = "TaxonKey")



 ##Species/Subspecies level, but exclude some enstries (~8) with NA in Rank.
  fish_taxon_sp <- fish_taxon[TaxLevel == 6 & !is.na(Rank),
                              ## not all taxa are found in the range fish_years,
                              ## which is not a problem,
                              ## but absent species don't need further processing
                  ]
fish_taxon_depth <-
  data.table::setDT(
    rfishbase::species(
      fish_taxon_sp$TaxonName,
      fields = c("Species", "DepthRangeShallow", "DepthRangeDeep"),
      server = "fishbase"
    ),
    key = "Species"
  )

 fish_taxon_depth[is.na(DepthRangeShallow), "DepthRangeShallow" := -Inf]
 fish_taxon_depth[is.na(DepthRangeDeep), "DepthRangeDeep" := -Inf]

 fish_taxon_depth_sealife <-
   data.table::setDT(
     rfishbase::species(fish_taxon_sp$TaxonName,
       fields = c("Species", "DepthRangeShallow", "DepthRangeDeep"),
       server = "sealifebase"
     ),
     key = "Species"
   )
 fish_taxon_depth_sealife[is.na(DepthRangeShallow), "DepthRangeShallow" := -Inf]
 fish_taxon_depth_sealife[is.na(DepthRangeDeep), "DepthRangeDeep" := -Inf]

  fish_taxon_depth[fish_taxon_depth_sealife,
                   c("DepthRangeShallow", "DepthRangeDeep") :=
                     .(
                       pmax(DepthRangeShallow, i.DepthRangeShallow),
                       pmax(DepthRangeDeep, i.DepthRangeDeep)
                       ),
                   on="Species"]

  fish_taxon_sp[fish_taxon_depth,
                c("DepthRangeShallow", "DepthRangeDeep") :=
                  .(
                   i.DepthRangeShallow,
                   i.DepthRangeDeep
                   ),
                on = c(TaxonName = "Species")]

  fish_taxon_sp[, which(
      !(colnames(fish_taxon_sp) %in%
        c("TaxonKey", "TaxonName",
          "DepthRangeShallow", "DepthRangeDeep")
      )
  ) := NULL]

  ## if a species only has one recorded depth,
  ## set both depths to the recorded depth
  fish_taxon_sp[is.infinite(DepthRangeShallow), DepthRangeShallow := DepthRangeDeep]
  fish_taxon_sp[is.infinite(DepthRangeDeep), DepthRangeDeep := DepthRangeShallow]
  ## keep species that have depth info
  fish_taxon_sp <- fish_taxon_sp[is.finite(DepthRangeShallow), ]

  for (d in depth_names) {
    fish_taxon_sp[, c(d) := (max(depth_range[[d]]) >= DepthRangeShallow) &
                    (min(depth_range[[d]]) <= DepthRangeDeep)
                ]
    }

  ## convert is.<depth> to long form
  fish_taxon_sp_depth <- data.table::melt(fish_taxon_sp,
                                       measure.vars =  depth_names,
                                       variable.name = "depth_cat",
                                       value.name = "depth_keep"
                                      )[depth_keep == TRUE]
  data.table::setkey(fish_taxon_sp_depth, "TaxonKey")
  fish_taxon_sp_depth[, which(
      !(colnames(fish_taxon_sp_depth) %in%
        c("TaxonKey", "TaxonName",
          "depth_cat")
      )
  ) := NULL]
  fish_taxon_sp_depth[, depth := sapply(depth_cat, function(x){depth_range[[x]][1]})]

## have the taxon keys, but not spatial vars or abundances
  ## Local function with closure over fish_data_dir
  fish_file_fn <- function(yr){paste0(fish_data_dir, "/Watson_", yr, "_TotalCatch_bySpecies.csv")}

   fish_catch <- data.table::rbindlist(
     purrr::map(fish_years,
                  ~ {data.table::fread(fish_file_fn(.x),
                                       select = c("TaxonKey", "LatCentre", "LonCentre", "TotalCatch"),
                                                     col.names = c("TaxonKey", "lat","lon", "abund")
                                       )[ , c("Year") := list(.x)]
                  }
                )
     )
  data.table::setkey(fish_catch, "TaxonKey")
  fish_catch_mean <- fish_catch[, .(abund = mean(abund)), by = .(TaxonKey, lat, lon)]
  data.table::setkey(fish_catch_mean, "TaxonKey")

 fish_long <- fish_taxon_sp_depth[fish_catch_mean,
                     allow.cartesian=TRUE, nomatch = NULL, on = "TaxonKey"]
  fish_long[, `:=` (TaxonKey = NULL,
                    TaxonName = NULL,
                    taxon = TaxonName,
                    survey = "watson",
                    trophic = "fish")]
  fish_no_taxa <- data.table::CJ(lat = seq(-90, 90, regrid_resolution)+env_offset, lon = seq(0, 360-regrid_resolution, regrid_resolution)+env_offset, depth_cat = depth_names)
  fish_no_taxa[, `:=`(
    depth = sapply(depth_cat, function(x){depth_range[[x]][1]}),
    taxon = "No taxa",
    abund = NA,
    survey = "watson",
    trophic = "fish"
  )]
  fish_long <- data.table::rbind(fish_long, fish_no_taxa)

  return(fish_long)
}


