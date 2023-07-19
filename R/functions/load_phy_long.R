#' Loads in phytoplankton data,
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

    ## CPR
    phy_planktonr <- planktonr::pr_get_CPRData("P", "abundance", "raw")
    ## Clean up
    ## Keep Longitute, Latitude, SampleTime_UTC
    sample_data <- phy_planktonr[,
                                 c("Latitude",
                                   "Longitude",
                                   "SampleTime_UTC",
                                   "SampleVolume_m3")]

    data.table::setDT(sample_data)
    ## Filter species data
    species_data <- data.table::setDT(phy_planktonr[, -(1:15)])


    ## Resolve to species level, drop spp. and genus only

    ## Simplest way to filter is by converting to
    ## species by site data.table, then
    ## filtering and grouping rows
    sp_data_t <- data.table::transpose(species_data,keep.names = "species")

    sp_name_clip <- function(x) {
        stringr::str_remove(x, " gravid$") |>
            stringr::str_remove(" \\(.*\\)") |>
            stringr::str_remove(" f$") |>
            stringr::str_remove(" m$") |>
            stringr::str_remove(" j$") |>
            stringr::str_remove(" agg$") |>
            stringr::str_remove(" sol$") |>
            stringr::str_remove(" larva$") |>
            stringr::str_remove(" larvae$") |>
            stringr::str_remove(" protozoea$") |>
            stringr::str_remove(" i$") |>
            stringr::str_remove(" cf. ") |>
            stringr::str_remove("<") |>
            stringr::str_remove("=") |>
            stringr::str_remove(">") |>
            stringr::str_remove("-") |>
            stringr::str_remove("-") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("µm") |>
            stringr::str_remove(" width$") |>
            stringr::str_remove(" cell$") |>
            stringr::str_remove(" zoea$")-> y
    }

    sp_names <- sp_name_clip(sp_data_t$species)

    sp_names_drop_spp <- !grepl("spp.|spp", sp_names)
    sp_names_drop_genus <- grepl(" ", sp_names)
    ## \\b means word boundary, so the following matches
    ## body or shell or ... but not "somebody"
    sp_names_drop_descriptors <- !grepl(
                                      "\\bbody\\b|\\bshell\\b|\\b[Ee]ggs*\\b|\\bagg\\b|\\bCalanid\\b|\\bdamaged\\b|\\bunidentified\\b|\\b[Ff]ish\\b|\\bnectophore\\b|\\binvert\\b|\\bworm\\b|\\bcrab\\b|\\bdecapod\\b|\\bRadiolarian\\b|\\bForam\\b|\\bamphipod\\b|\\bGrp[1-9]\\b|\\b[Cc]yclopoid\\b|\\badult\\b|\\bdiatom\\b|\\b[uU]nid\\b|\\bSpikey\\b|[Ff]lagellate\\b|\\bNavicula[ 0-9][ 0-9]|\\bCiliate\\b|\\bpollen\\b|\\bGold\\b|\\bFungal\\b|\\bFilamentous\\b|\\bFragilaria\\b", sp_names)

    sp_keep <- sp_names_drop_spp &
        sp_names_drop_genus &
        sp_names_drop_descriptors

    sp_data_t[, sp_names_new := make.names(sp_names)]

    sp_data_t <- sp_data_t[sp_keep,]
    sp_data_t[, species := NULL]

    sp_data_t <- sp_data_t[,
                           by = sp_names_new,
                           lapply(.SD, sum)
                           ]


    sp_data <- data.table::transpose(sp_data_t, make.names = "sp_names_new")

    sp_data_m3 <- sp_data/sample_data$SampleVolume_m3
    sample_data[, SampleVolume_m3 := NULL]
    ## Merge cols

    phy_cpr <- cbind(sample_data, sp_data_m3)
    phy_cpr_long <- data.table::melt(phy_cpr,
                                     id.vars = c(
                                         "Latitude",
                                         "Longitude",
                                         "SampleTime_UTC"
                                     ),
                                     variable.name = "taxon",
                                     value.name = "abund"
                                     )
    phy_cpr_long[, survey := "cpr"]
    new_names <- tibble::tribble(
                             ~old, ~new,
                             "Longitude", spatial_vars[1],
                             "Latitude",spatial_vars[2]
                         )
    setnames(phy_cpr_long,
             new_names$old,
             new_names$new
             )

    phy_cpr_long[,
                 c("trophic", "depth", "depth_cat") :=
                     .("phy",
                       0,
                       depth_names[1]  ## all samples are epipelagic
                       )
                 ]

    ## NRS
    phy_planktonr <- planktonr::pr_get_NRSData("P", "abundance", "raw")
    ## Clean up
    ## Keep Longitute, Latitude, SampleTime_UTC
    sample_data <- phy_planktonr[,
                                 c("Latitude",
                                   "Longitude",
                                   "SampleTime_UTC",
                                   "SampleDepth_m")]

    data.table::setDT(sample_data)
    ## Filter species data
    species_data <- data.table::setDT(phy_planktonr[, -(1:17)])


    ## Resolve to species level, drop spp. and genus only

    ## Simplest way to filter is by converting to
    ## species by site data.table, then
    ## filtering and grouping rows
    sp_data_t <- data.table::transpose(species_data,keep.names = "species")

    sp_name_clip <- function(x) {
        stringr::str_remove(x, " gravid$") |>
            stringr::str_remove(" \\(.*\\)") |>
            stringr::str_remove(" f$") |>
            stringr::str_remove(" m$") |>
            stringr::str_remove(" j$") |>
            stringr::str_remove(" agg$") |>
            stringr::str_remove(" sol$") |>
            stringr::str_remove(" larva$") |>
            stringr::str_remove(" larvae$") |>
            stringr::str_remove(" protozoea$") |>
            stringr::str_remove(" i$") |>
            stringr::str_remove(" cf. ") |>
            stringr::str_remove("<") |>
            stringr::str_remove("=") |>
            stringr::str_remove(">") |>
            stringr::str_remove("-") |>
            stringr::str_remove("-") |>
            stringr::str_remove("~") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("[0-9]") |>
            stringr::str_remove("µm") |>
            stringr::str_remove("\\bum\\b") |>
            stringr::str_remove(" width$") |>
            stringr::str_remove(" cell$") |>
            stringr::str_remove(" length$") |>
            stringr::str_remove("morphotype") |>
            stringr::str_remove(" zoea$") |>
            stringr::str_replace("^Emiliania huxleyi.*", "Emiliania huxleyi") |>
            stringr::str_trim() -> y
    }

    sp_names <- sp_name_clip(sp_data_t$species)

    sp_names_drop_spp <- !grepl("spp.|spp", sp_names)
    sp_names_drop_genus <- grepl(" ", sp_names)
    ## \\b means word boundary, so the following matches
    ## body or shell or ... but not "somebody"
    sp_names_drop_descriptors <- !grepl(
                                      "\\bbody\\b|\\bshell\\b|\\b[Ee]ggs*\\b|\\bagg\\b|\\bCalanid\\b|\\bdamaged\\b|\\bunidentified\\b|\\b[Ff]ish\\b|\\bnectophore\\b|\\binvert\\b|\\bworm\\b|\\bcrab\\b|\\bdecapod\\b|\\bRadiolarian\\b|\\bForam\\b|\\bamphipod\\b|\\bGrp[1-9]\\b|\\b[Cc]yclopoid\\b|\\badult\\b|\\bdiatom\\b|\\b[uU]nid\\b|\\bSpikey\\b|[Ff]lagellate\\b|\\bNavicula[ 0-9][ 0-9]|\\bCiliate\\b|\\bpollen\\b|\\bGold\\b|\\bFungal\\b|\\bFilamentous\\b|\\bFragilaria\\b|hairy flap|\\bCyclotella\\b", sp_names)

    sp_keep <- sp_names_drop_spp &
        sp_names_drop_genus &
        sp_names_drop_descriptors


    sp_data_t[, sp_names_new := make.names(sp_names)]

    sp_data_t <- sp_data_t[sp_keep,]
    sp_data_t[, species := NULL]

    sp_data_t <- sp_data_t[,
                           by = sp_names_new,
                           lapply(.SD, sum)
                           ]


    sp_data <- data.table::transpose(sp_data_t, make.names = "sp_names_new")

    ## Merge cols

    phy_nrs <- cbind(sample_data, sp_data)
    phy_nrs_long <- data.table::melt(phy_nrs,
                                     id.vars = c(
                                         "Latitude",
                                         "Longitude",
                                         "SampleTime_UTC",
                                         "SampleDepth_m"
                                     ),
                                     variable.name = "taxon",
                                     value.name = "abund"
                                     )
    phy_nrs_long[, survey := "nrs"]
    phy_nrs_long[is.na(SampleDepth_m), SampleDepth_m := 0]
    new_names <- tibble::tribble(
                             ~old, ~new,
                             "Longitude", spatial_vars[1],
                             "Latitude",spatial_vars[2],
                             "SampleDepth_m", "depth"
                         )
    setnames(phy_nrs_long,
             new_names$old,
             new_names$new
             )
    phy_nrs_long[, depth := as.numeric(depth)]

    phy_nrs_long[,
                 c("trophic", "depth_cat") :=
                     .("phy", {
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

    ## Other surveys

    phy_other <- phyto_process_other(phy_data_dir)
    data.table::setDT(phy_other)

    date_UTC <- phy_other[, list(mapply(\(x, lon, lat){
        data.table::data.table(SampleDate_UTC =
                                   lubridate::as_datetime(
                                                  x,
                                                  tz = lutz::tz_lookup_coords(
                                                                 lat, lon,
                                                                 method = "fast"
                                                             ),
                                                  format = "%d/%m/%Y %H:%M"
                                              ))
    }, SampleDateUTC, Longitude, Latitude, SIMPLIFY = FALSE))]
    date_UTC <- rbindlist(date_UTC[[1]])
    phy_other[, `:=`(
        SampleDate_UTC = date_UTC$SampleDate_UTC,
        SampleDateUTC = NULL
    )]


    
    phy_brett <- phyto_process_brett(phy_data_dir)
    data.table::setDT(phy_brett)
    setnames(phy_brett, old = "SampleDateUTC", new = "SampleDate_UTC")

    phy_raw <- rbindlist(use.names = TRUE,
      list(
        phy_other,
        phy_brett
      )
      )

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
        depth = 0, ## all samples are epipelagic
        depth_cat = depth_names[1], ## all samples are epipelagic
        taxon = make.names(stringr::str_remove(taxon, "\xb5m")),
        ProjectNumber = NULL)]

    phy_raw_all <- rbindlist(
      use.names = TRUE,
      list(
        phy_raw,
        phy_cpr_long,
        phy_nrs_long
      )
    )


    
    obs <- data.table::copy(phy_raw_all)
    obs[is.na(abund), abund := 0]
    samps <- unique(obs[, c(spatial_vars, "depth", "depth_cat", "SampleDate_UTC"), with=FALSE])
    samps[ , samp_id := seq.int(1, nrow(samps))]
    obs[samps, samp_id := i.samp_id, on = c(spatial_vars, "depth", "depth_cat", "SampleDate_UTC")]
    obs[, c(spatial_vars, "depth", "SampleDate_UTC") := NULL]

    taxa <- data.table::data.table(taxon = unique(obs[, taxon]))
    taxa[ , taxon_id := seq.int(1, nrow(taxa))]
    obs[taxa, taxon_id := i.taxon_id, on = "taxon"]
    obs[ , taxon := NULL]

    phy_out <- obs[, {
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


    ## Temporary fix for rows that appear duplicated
    for(.x in phy_out$survey) {
        print(.x)
        original <- nrow(phy_out[survey == .x & depth_cat == "epi",]$obs[[1]] )
        dups <-nrow(phy_out[survey == .x & depth_cat == "epi",]$obs[[1]][, .(abund = sum(abund)), by = c("samp_id", "taxon_id")])

        print(original)
        print(dups)

        if(original != dups) {
            print(.x)
            print("fixing")
            phy_out[survey == .x & depth_cat == "epi",]$obs <- list(phy_out[survey == .x & depth_cat == "epi",]$obs[[1]][, .(abund = sum(abund)), by = c("samp_id", "taxon_id")])
        }
    }

    return(phy_out)
}

  ## ##do this for global list
  ## phy_raw[ , .(Species := clean_sp_names(Species))]

  ## ##Do this AFTER merging with env
  ## ##Find species that were not consistently sampled
  ## phy_raw[
  ## get_uncertain_sp_names(phyplank_surv, -999, "Abund_m3"),

  ## ##Before converting to wide, extract list of species names, but after global list

