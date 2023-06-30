get_mpa_polys <- function(
                          country_code,
                          iucn_cat_target,
                          marine_categories,
                          mpa_folder
                          ) {
    s2_used <- sf::sf_use_s2()
    sf::sf_use_s2(FALSE)
    country_mpa <- rbind(
      sf::st_read(paste0(mpa_folder, "/aus_mpa_0")),
      sf::st_read(paste0(mpa_folder, "/aus_mpa_1")),
      sf::st_read(paste0(mpa_folder, "/aus_mpa_2"))
    )
  ## This line is sensitive to internet quality in wdpar version 1.3.1.3
  ## country_mpa <- wdpar::wdpa_fetch(country_code, wait = TRUE)
  ## country_mpa <- wdpar::wdpa_read(mpa_zip)
    ## world_mpas <- sf::st_read(mpa_folder)
    ## country_mpa <- world_mpas[world_mpas$PARENT_ISO == country_code, ]
    ## slow, definitely worth spinning out into a separate target
    # country_mpa_clean <- wdpar::wdpa_clean(country_mpa, erase_overlaps = FALSE)
    # data.table::setDT(country_mpa_clean)
    country_mpa_sub <- country_mpa[country_mpa$IUCN_CAT %in% iucn_cat_target$categories[[1]] & country_mpa$MARINE != 0, ]
    ## country_mpa_sub <- country_mpa_clean[IUCN_CAT %in% iucn_categories,]
    ## country_mpa_clean_sub <- country_mpa_clean[IUCN_CAT %in% iucn_categories,] ## could also look at NO_TAKE
  #country_mpa_clean_sub <- country_mpa_clean_sub[MARINE %in% marine_categories,]

  ## wdpa_dissolve has a default precision of 1500, and recommends this for
  ## national scale analysis
  ## We only care about total area covered by MPAs,
  ## not individual MPA identities
    country_mpa_dissolved <- sf::st_union(country_mpa_sub)
    ## country_mpa_dissolved <- wdpar::wdpa_dissolve(sf::st_as_sf(country_mpa_clean_sub))
    sf::sf_use_s2(s2_used)
  ## aus_mpa is already an sf object
    return(data.table::data.table(
        iucn_categories = list(iucn_cat_target),
        mpa_polys = list(country_mpa_dissolved)
    ))
}
