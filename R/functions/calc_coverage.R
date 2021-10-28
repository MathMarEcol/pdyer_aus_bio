calc_coverage <- function(
      gfbootstrap_caster,
      gfbootstrap_predicted,
      spatial_vars,
      output_folder

                          ) {
  ### Skip failed surveys

  pl_file_base <- file.path(output_folder, gfbootstrap_caster$surv_full_name[1])
  pl_file <-  paste0(pl_file_base, "_cluster_mpa_coverage.png")
  if (is.na(gfbootstrap_caster$best_clust)) {
    no_plot <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point() +
      ggplot2::ggtitle(paste0(gfbootstrap_caster$surv_full_name[1], " has not successfully clustered"))
    ggsave_wrapper(filename = pl_file, plot = no_plot)
    return(pl_file)
  }

  ### Load in cluster polygons
  sites<- data.table::as.data.table(gfbootstrap_caster$clust_ind[[1]][, ..spatial_vars])
  clust_raster <- terra::rast(
            x = as.matrix(data.frame(sites[, ..spatial_vars], gfbootstrap_caster$clust_ind[[1]]$cl)),
            type = "xyz",
            crs = "+proj=longlat +datum=WGS84")

  names(clust_raster) <- c("clustering")
  clust_multipoly <- terra::as.polygons(clust_raster)
  clust_poly_sf <- sf::st_as_sf(clust_multipoly)

  ### Load in MPAs
  if (grepl("nixos", Sys.info()["version"], ignore.case = TRUE)){

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
      stop(paste0("This is a hacked version of wdman for NixOS systems, NixOS does not play well with external binaries.\n",
                  "You must specify a phantomjs version of [", phantomsysver, "]. If that number is empty, phantomjs may not be installed on the system")
           )
    }
    phantompath <- strsplit(processx::run("which", "phantomjs")$stdout, "\n")[[1]]
    phantomdir <- dirname(phantompath)
    list(version = phantomver, dir = phantomdir, path = phantompath)
  }

  assignInNamespace("phantom_ver", phantom_ver, ns="wdman")

  }
  library(wdpar)
  aus_mpa <- wdpar::wdpa_fetch("Australia", wait = TRUE) # This line is sensitive to internet quality in wdpar version 1.3.1.3
  aus_mpa_clean <- wdpar::wdpa_clean(aus_mpa) #slow, definitely worth spinning out into a separate target
  data.table::setDT(aus_mpa_clean)
  aus_mpa_clean_sub <- aus_mpa_clean[IUCN_CAT %in% c("Ia", "Ib", "II"),] ## could also look at NO_TAKE
  aus_mpa_clean_sub<- aus_mpa_clean_sub[MARINE %in% c("marine"),]

  ##wdpa_dissolve has a default precision of 1500, and recommends this for national scale analysis
  ## We only care about total area covered by MPAs, not individual MPA identities
  aus_mpa_clean_dissolved <- wdpar::wdpa_dissolve(sf::st_as_sf(aus_mpa_clean_sub))
  ## aus_mpa is already an sf object

  ### Calculate overlap

  ## st_intersection should give us the polygons covering both clusters and MPAs.
  ## Need to project both polygon sets into the same CRS, but details of CRS are not critical?
  clust_poly_sf_trans <- sf::st_transform(clust_poly_sf, sf::st_crs(aus_mpa_clean_dissolved))
  clust_mpa_intersect <- sf::st_intersection(clust_poly_sf_trans,  aus_mpa_clean_dissolved)

  clust_area_covered <- sf::st_area(clust_mpa_intersect)
  clust_area_total <- sf::st_area(clust_poly_sf_trans)
  clust_area_frac <- clust_area_covered / clust_area_total
  ## Plot? tabulate?
  ## Create data.frame with area, clust_id and plot_order.
  ## plot order is just 1 to k, added after sorting clust_id by area
  ## plot_order gives left to right, clust_id gives mapping to plots
  clust_area_table <- data.table::data.table(clust_id = clust_poly_sf_trans$clustering,
                                             area =  as.numeric(clust_area_frac))

  data.table::setorder(clust_area_table, "area")
  clust_area_table[, "plot_order" := seq_along(clust_area_table$clust_id)]

  pl_clust_coverage <- ggplot2::ggplot(clust_area_table ,  ggplot2::aes(x = reorder(clust_id, area), y = area)) +
    ggplot2::scale_x_discrete("Cluster") +
    ggplot2::scale_y_continuous("MPA Coverage (Fraction of total cluster area)", limits =  c(0, 1), breaks = seq(0, 1, 0.1 )) +
    ggplot2::geom_col() +
    ggthemes::theme_tufte()


  ggsave_wrapper(filename = pl_file, plot = pl_clust_coverage)
  return(pl_file)
}
