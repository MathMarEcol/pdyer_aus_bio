# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only
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
    feeding_data <- planktonr::pr_get_PlanktonInfo("Z")
    data.table::setDT(feeding_data)
    ## CPR

    zoo_planktonr <- planktonr::pr_get_CPRData("Z", "abundance", "species")
    ## Clean up
    ## Keep Longitute, Latitude, SampleTime_UTC
    sample_data <- zoo_planktonr[,
                                 c("Latitude",
                                   "Longitude",
                                   "SampleTime_UTC",
                                   "SampleVolume_m3")]

    data.table::setDT(sample_data)
    ## Filter species data
    species_data <- data.table::setDT(zoo_planktonr[, -(1:16)])

    ## Resolve to species level, drop spp. and genus only

    ## Simplest way to filter is by converting to
    ## species by site data.table, then
    ## filtering and grouping rows
    sp_data_t <- data.table::transpose(species_data,keep.names = "species")
    sp_data_t[feeding_data, feeding := Diet,  on = c(species = "Taxon Name")]
    keep_feeding_class <- sp_data_t$feeding %in% c("Herbivore", "Omnivore", "Carnivore")
    keep_specific <- sp_data_t$species %in% c()

    sp_data_t <- sp_data_t[keep_feeding_class | keep_specific, ]



    sp_name_clip <- function(x) {
        stringr::str_remove(x, " gravid$") |>
          stringr::str_remove(" \\(.*\\)") |>
          stringr::str_remove(" f$") |>
          stringr::str_remove(" m$") |>
          stringr::str_remove(" j$") |>
          stringr::str_remove(" agg$") |>
          stringr::str_remove(" sol$") |>
            stringr::str_remove(" i$") |>
            stringr::str_remove(" gp$")   -> y
    }

    sp_name_drop_subspecies <- function(x, sep = " ", join = ".") {
        ret <- stringr::str_c(
                     stringr::str_split_1(x, sep)[1:2],
                     collapse = join
                     )

    }

    sp_names <- sp_data_t$species
    sp_names_genus_sp <- vapply(sp_names, sp_name_drop_subspecies, character(1))
    names(sp_names_genus_sp)<-NULL
    sp_names_drop_spp <- !grepl("spp.|spp", sp_names)
    sp_names_drop_genus <- !is.na(sp_names_genus_sp)
    ## \\b means word boundary, so \\bbody\\b matches
    ## body but not "somebody"

    drop_regexp <- "\\bbody\\b|\\bshell\\b|\\b[Ee]ggs*\\b|\\bagg\\b|\\bCalanid\\b|\\bdamaged\\b|\\bunidentified\\b|\\b[Ff]ish\\b|\\bnectophore\\b|\\binvert\\b|\\bworm\\b|\\bcrab\\b|\\bdecapod\\b|\\bRadiolarian\\b|\\bForam\\b|\\bamphipod\\b|\\bGrp[1-9]\\b|\\b[Cc]yclopoid\\b|\\bascidian\\b|\\bterrestrial\\b|\\bCalanoid\\b|\\blarvae\\b|\\blarva\\b|\\bBarnacle\\b|\\bmegalopa\\b|\\b[Zz]oea\\b|\\bprotozoea\\b|\\b[Ee]uphausiid\\b|\\b[Nn]auplii\\b|\\bmetanauplii\\b|\\bcyprid\\b|\\b[Gg]astropod\\b|\\b[Mm]ollusc\\b|\\bPterotracheoidea\\b|\\bvicina/macrura\\b|\\bBracht\\b|\\bcf\\b|\\bunid\\b|\\b[Pp]olychaeta\\b"

    sp_names_drop_descriptors <- !grepl(
                                      drop_regexp,
                                      sp_names)
    ## Merge subspecies back into species



    sp_keep <- sp_names_drop_spp &
        sp_names_drop_genus &
        sp_names_drop_descriptors


    sp_data_t[, sp_names_new := make.names(sp_names_genus_sp)]


    sp_data_t <- sp_data_t[sp_keep,]
    sp_data_t[, species := NULL]
    sp_data_t[, feeding := NULL]

    sp_data_t <- sp_data_t[,
      by = sp_names_new,
      lapply(.SD, sum)
      ]

sp_data <- data.table::transpose(sp_data_t, make.names = "sp_names_new")

sample_data[, SampleVolume_m3 := NULL]
## Merge cols

zoo_cpr <- cbind(sample_data, sp_data)
zoo_cpr_long <- data.table::melt(zoo_cpr,
                                 id.vars = c(
                                   "Latitude",
                                   "Longitude",
                                   "SampleTime_UTC"
                                 ),
  variable.name = "taxon",
  value.name = "abund"
)
zoo_cpr_long[, survey := "cpr"]
new_names <- tibble::tribble(
                         ~old, ~new,
                         "Longitude", spatial_vars[1],
                         "Latitude",spatial_vars[2]
                     )
setnames(zoo_cpr_long,
         new_names$old,
         new_names$new
         )

zoo_cpr_long[,
        c("trophic", "depth", "depth_cat") :=
            .("zoo",
              0,
              depth_names[1]  ## all samples are epipelagic
              )
        ]

## NRS

zoo_planktonr <- planktonr::pr_get_NRSData("Z", "abundance", "species")
## Clean up
## Keep Longitute, Latitude, SampleTime_UTC
sample_data <- data.table::setDT(zoo_planktonr[
  ,
  c(
    "Latitude",
    "Longitude",
    "SampleTime_Local",
    "SampleDepth_m"
  )
])
sample_data[, SampleTime_UTC :=
                  lubridate::with_tz(lubridate::force_tzs(
    SampleTime_Local,
    lutz::tz_lookup_coords(Latitude, Longitude, method = "fast")
  ), "UTC")]

sample_data[, SampleTime_Local := NULL]

## Filter species data
species_data <- data.table::setDT(zoo_planktonr[, -(1:18)])

## Resolve to species level, drop spp. and genus only

## Simplest way to filter is by converting to
## species by site data.table, then
## filtering and grouping rows
sp_data_t <- data.table::transpose(species_data,keep.names = "species")
    sp_data_t[feeding_data, feeding := Diet,  on = c(species = "Taxon Name")]
    keep_feeding_class <- sp_data_t$feeding %in% c("Herbivore", "Omnivore", "Carnivore")
    keep_specific <- sp_data_t$species %in% c()

    sp_data_t <- sp_data_t[keep_feeding_class | keep_specific, ]

sp_names <- sp_data_t$species
    sp_names_genus_sp <- vapply(sp_names, sp_name_drop_subspecies, character(1))
    names(sp_names_genus_sp)<-NULL

sp_names_drop_spp <- !grepl("spp.|spp", sp_names)
    sp_names_drop_genus <- !is.na(sp_names_genus_sp)
## \\b means word boundary, so the following matches
## body or shell or ... but not "somebody"
    sp_names_drop_descriptors <- !grepl(drop_regexp, sp_names)



sp_keep <- sp_names_drop_spp &
    sp_names_drop_genus &
    sp_names_drop_descriptors

sp_data_t[, sp_names_new := make.names(sp_names_genus_sp)]

sp_data_t <- sp_data_t[sp_keep,]
sp_data_t[, species := NULL]
    sp_data_t[, feeding := NULL]

sp_data_t <- sp_data_t[,
                       by = sp_names_new,
                       lapply(.SD, sum)
                       ]

sp_data <- data.table::transpose(sp_data_t, make.names = "sp_names_new")


## Merge cols

zoo_nrs <- cbind(sample_data, sp_data)
zoo_nrs_long <- data.table::melt(zoo_nrs,
                                 id.vars = c(
                                     "Latitude",
                                     "Longitude",
                                     "SampleTime_UTC",
                                     "SampleDepth_m"
                                 ),
                                 variable.name = "taxon",
                                 value.name = "abund"
                                 )
zoo_nrs_long[, survey := "nrs"]
new_names <- tibble::tribble(
                         ~old, ~new,
                         "Longitude", spatial_vars[1],
                         "Latitude",spatial_vars[2],
                         "SampleDepth_m", "depth"

                     )
setnames(zoo_nrs_long,
         new_names$old,
         new_names$new
         )

    zoo_nrs_long[, depth := as.numeric(depth)]


zoo_nrs_long[,
             c("trophic", "depth_cat") :=
               .("zoo", {
                 in_depth <- sapply(depth_range,
                   \(x, depth){
                     depth >= min(x) & depth < max(x)
                   },
                   depth = depth
                 )
                 depth_out <- depth_names[apply(in_depth, 1, \(x){
                   min(which(x))
                 })]
               })
             ]

    ## Other datasets
##zoo_raw <- load_zoo_data(zoo_data_dir)
zoo_raw <- zoo_process_other(zoo_data_dir)

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
                             "ZAbund_m3", "abund",
                             "SampleDateUTC", "SampleTime_UTC"
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
      ProjectNumber = NULL,
      taxon = make.names(taxon)
    )]

## Merge all surveys

    zoo_raw_all <- rbindlist(use.names = TRUE,
                             list(
  zoo_raw,
  zoo_cpr_long,
  zoo_nrs_long
))

    obs <- data.table::copy(zoo_raw_all)
    obs[is.na(abund), abund := 0]
    samps <- unique(obs[, c(spatial_vars, "depth", "depth_cat", "SampleTime_UTC"), with=FALSE])
    samps[ , samp_id := seq.int(1, nrow(samps))]
    obs[samps, samp_id := i.samp_id, on = c(spatial_vars, "depth", "depth_cat", "SampleTime_UTC")]
    obs[, c(spatial_vars, "depth", "SampleTime_UTC") := NULL]

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


    ## Temporary fix for rows that appear duplicated in McKinnon Survey
    zoo_out[survey == "mckinnon" & depth_cat == "epi",]$obs <- list(zoo_out[survey == "mckinnon" & depth_cat == "epi",]$obs[[1]][, .(abund = sum(abund)), by = c("samp_id", "taxon_id")])


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
