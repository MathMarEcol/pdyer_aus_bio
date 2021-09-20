#' Loads in fish data,
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
load_fish_long <- function(
                          fish_taxon_file,
                          fish_data_dir,
                          fish_years,
                          spatial_vars,
                          depth_names,
                          depth_range,
                          regrid_resolution,
                          env_offset,
                          biooracle_folder
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
  fish_taxon_depth[, DepthRangeShallow :=  as.numeric(DepthRangeShallow)]
  fish_taxon_depth[, DepthRangeDeep :=  as.numeric(DepthRangeDeep)]

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
  fish_taxon_depth_sealife[, DepthRangeShallow :=  as.numeric(DepthRangeShallow)]
  fish_taxon_depth_sealife[, DepthRangeDeep :=  as.numeric(DepthRangeDeep)]
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
  rm(fish_catch)
  data.table::setkey(fish_catch_mean, "TaxonKey")

 fish_long <- fish_taxon_sp_depth[fish_catch_mean,
                     allow.cartesian=TRUE, nomatch = NULL, on = "TaxonKey"]
  rm(fish_catch_mean)
  fish_long[, `:=` (TaxonKey = NULL,
                    TaxonName = NULL,
                    taxon = TaxonName,
                    survey = "watson",
                    trophic = "fish")]
  ## fish_long[, c(spatial_vars[1]) := (.SD %% 360), .SDcols = spatial_vars[1]]
  ## Fish are sampled globally
  ##
  ## data("wrld_simpl", package = 'maptools')
  ## world_poly <- st_as_sf(wrld_simpl)

  bathy_raster <- terra::rast(suppressWarnings(sdmpredictors::load_layers("MS_bathy_5m",
                                           datadir = biooracle_folder,
                                           rasterstack = FALSE))[[1]])
  bathy_sites <- lapply(depth_names, function(d, bathy_raster, depth_range, fish_long, spatial_vars){

    fish_sites <- data.table::CJ(lat = seq(-89.75, 89.75, 0.5), lon = seq(-179.75, 179.75, 0.5))
    fish_sites_vec <- terra::vect(fish_sites, geom =  spatial_vars, crs =  "+proj=longlat +datum=WGS84 +no_defs")
    depth_sites <- terra::extract(bathy_raster,  fish_sites_vec, xy =  TRUE)
    data.table::setDT(depth_sites )
    depth_sites <- depth_sites[!is.nan(MS_bathy_5m_lonlat) &
                               MS_bathy_5m_lonlat <= -depth_range[[d]][1] ]
    depth_sites[, c("ID",  "MS_bathy_5m_lonlat") := NULL]
    names(depth_sites) <- spatial_vars
    depth_sites[, site_id := seq.int(1,nrow(depth_sites))]
## tmap::qtm(sf::st_as_sf(depth_poly), fill = "blue")
}, bathy_raster = bathy_raster, depth_range=depth_range, fish_long=fish_long, spatial_vars=spatial_vars)
  names(bathy_sites) <- depth_names

  ## fish_sites <- fish_sites[!sf::st_intersects(st_as_sf(fish_sites, coords = spatial_vars, crs = st_crs(world_poly)), world_poly) %>% lengths > 0, ]
  ## fish_sites <- unique(rbind(fish_long[, ..spatial_vars], fish_sites[, ..spatial_vars]))
  ## fish_sites[, site_id := seq.int(1,nrow(fish_sites))]
  ##verified with:
  ## ggplot(fish_sites, aes(x=lon, y=lat))+geom_raster()
  ## ggplot(fish_long, aes(x=lon, y=lat))+geom_raster()
  ## ggplot(fish_long[, aes(x=lon, y=lat))+geom_raster()
  ## fish_remerge <- fish_long[fish_sites, on = c(spatial_vars)]

  ##                         obs <- data.table::copy(fish_long)
  ##                         sites <- data.table(fish_sites,  depth = depth_range[["epi"]][1])
  ##                         obs[sites, site_id := i.site_id, on = c(spatial_vars)]
  #fish_raw[fish_no_taxa, site_id := i.site_id, on = c(spatial_vars)],
  fish_rows <- fish_long[,
                        {
                          obs <- data.table::copy(.SD)
                          ## sites <- data.table(fish_sites,  depth = depth_range[[.BY$depth_cat]][1])
                          sites <- bathy_sites[[.BY$depth_cat]]
                          sites[, depth := depth_range[[.BY$depth_cat]][1]]
                          obs[sites, site_id := i.site_id, on = c(spatial_vars)]
                          obs[, c(spatial_vars, "depth") := NULL]

                        taxa <- data.table::data.table(taxon = unique(obs[, taxon]))
                        taxa[ , taxon_id := seq.int(1, nrow(taxa))]
                        obs[taxa, taxon_id := i.taxon_id, on = "taxon"]
                        obs[ , taxon := NULL]

                          obs[ , c("site_id", "samp_id") := .(NULL, site_id)]
                          sites[ , c("site_id", "samp_id") := .(NULL, site_id)]

                          data.table::data.table(samps = list(sites),
                                            taxa = list(taxa),
                                            obs = list(obs)
                                            )
                        },
                      by = c("survey", "trophic",  "depth_cat")]
## ggplot(fish_rows$obs[[2]][fish_rows$sites[[2]], on = "site_id", nomatch = NULL], aes(x=lon, y= lat, size = abund)) + geom_point()


  ## fish_no_taxa <- data.table::CJ(lat = seq(-90, 90, regrid_resolution)+env_offset, lon = seq(0, 360-regrid_resolution, regrid_resolution)+env_offset, depth_cat = depth_names)
  ## fish_no_taxa[, `:=`(
  ##   depth = sapply(depth_cat, function(x){depth_range[[x]][1]}),
  ##   taxon = "No taxa",
  ##   abund = NA,
  ##   survey = "watson",
  ##   trophic = "fish"
  ## )]
  ## fish_long <- rbind(fish_long, fish_no_taxa)

  return(fish_rows)
}

