get_mpa_polys <- function(
                          country_code,
                          iucn_categories,
                          marine_categories
                          ) {
  ## NixOS is beiong used to pin R packages to a known state.
  ## However, the package wdman expects to be able to download
  ## and run arbitrary binaries, and breaks under NixOS.
  ## The following snippet patches wdman to avoid breaking
  ## wdpar, but only when we are running in NixOS or in the
  ## singularity image created in NixOS for this pipeline.
  ## TODO: submit patch to nixos

  if (grepl("nixos", Sys.info()["version"], ignore.case = TRUE) |
      Sys.getenv("SINGULARITY_NAME") ==  "r-singularity-aus-bio.img") {

        phantom_ver <- function(platform, version){
            phantomver <- binman::list_versions("phantomjs")[[platform]]
            phantomver <- if(identical(version, "latest")){
                            as.character(max(semver::parse_version(phantomver)))
                        }else{
                            mtch <- match(version, phantomver)
                            if(is.na(mtch) || is.null(mtch)){
                            stop("version requested doesnt match versions available = ",
                                paste(phantomver, collapse = ","))
                            }
                            phantomver[mtch]
                        }
            phantomsysver <- as.character(max(semver::parse_version(gsub("\n", "", (processx::run("phantomjs",  "--version")$stdout)))))
            if(phantomsysver != phantomver) {
            stop(paste0("This is a patched version of wdman for NixOS systems, NixOS does not play well with external binaries.\n",
                        "You must specify a phantomjs version that matches your currently installed version: [", ifelse(length(phantomsysver) > 0, phantomsysver, "PhantomJS not found on system"), "]."
                ))
            }
            phantompath <- strsplit(processx::run("which", "phantomjs")$stdout, "\n")[[1]]
            phantomdir <- dirname(phantompath)
            list(version = phantomver, dir = phantomdir, path = phantompath)
        }
        assignInNamespace("phantom_ver", phantom_ver, ns="wdman")
    }
  ## This line is sensitive to internet quality in wdpar version 1.3.1.3
  country_mpa <- wdpar::wdpa_fetch(country_code, wait = TRUE)
  ## slow, definitely worth spinning out into a separate target
  country_mpa_clean <- wdpar::wdpa_clean(country_mpa)
  data.table::setDT(country_mpa_clean)
  country_mpa_clean_sub <- country_mpa_clean[IUCN_CAT %in% iucn_categories,] ## could also look at NO_TAKE
  country_mpa_clean_sub <- country_mpa_clean_sub[MARINE %in% marine_categories,]

  ## wdpa_dissolve has a default precision of 1500, and recommends this for
  ## national scale analysis
  ## We only care about total area covered by MPAs,
  ## not individual MPA identities
  country_mpa_clean_dissolved <- wdpar::
    wdpa_dissolve(sf::st_as_sf(country_mpa_clean_sub))
  ## aus_mpa is already an sf object
  return(country_mpa_clean_dissolved)
}
