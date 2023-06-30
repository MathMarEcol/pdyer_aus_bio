get_mpa_polys <- function(
                          country_code,
                          iucn_categories,
                          marine_categories,
                          mpa_folder
                          ) {
  ## This line is sensitive to internet quality in wdpar version 1.3.1.3
  ## country_mpa <- wdpar::wdpa_fetch(country_code, wait = TRUE)
    world_mpas <- sf::st_read(mpa_folder)
    country_mpa <- world_mpas[world_mpas$PARENT_ISO == country_code, ]
    ## slow, definitely worth spinning out into a separate target
  #country_mpa_clean <- wdpar::wdpa_clean(country_mpa, erase_overlaps = FALSE)
  #data.table::setDT(country_mpa_clean)
    country_mpa_sub <- country_mpa[IUCN_CAT %in% iucn_categories$categories,]
    ## country_mpa_sub <- country_mpa_clean[IUCN_CAT %in% iucn_categories,]
    ## country_mpa_clean_sub <- country_mpa_clean[IUCN_CAT %in% iucn_categories,] ## could also look at NO_TAKE
  #country_mpa_clean_sub <- country_mpa_clean_sub[MARINE %in% marine_categories,]

  ## wdpa_dissolve has a default precision of 1500, and recommends this for
  ## national scale analysis
  ## We only care about total area covered by MPAs,
  ## not individual MPA identities
    country_mpa_dissolved <- wdpar::wdpa_dissolve(sf::st_as_sf(country_mpa_sub))
    ## country_mpa_dissolved <- wdpar::wdpa_dissolve(sf::st_as_sf(country_mpa_clean_sub))
  ## aus_mpa is already an sf object
    return(data.table(
        iucn_categories = list(iucn_categories),
        mpa_polys = list(country_mpa_dissolved)
    ))
}
