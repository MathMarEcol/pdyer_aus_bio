find_indicator_species <- function(
                                   #gfbootstrap_cluster,
                                   #gfbootstrap_combined,
                                   extrap_polygons,
                                   #gfbootstrap_predicted,
                                  # all_bio_env,
                                   all_bio_long,
                                   spatial_vars
                                   ) {
  ## Flag if biology and clustering are matched
  ## Useful for later evaluation of results
  matched <- all_bio_long$survey == extrap_polygons$survey &&
    all_bio_long$trophic == extrap_polygons$trophic &&
    all_bio_long$depth_cat == extrap_polygons$depth_cat


  if(all(is.na(extrap_polygons$polygons[[1]])) ||
       all(is.na(all_bio_long$samps))) {
         return(data.table::data.table(extrap_polygons[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat, clust_method)],
                                                               matched = matched,
                                                               spec_ind = list(NA),
                                                               spec_ind_solo = list(NA)))
         }

         
  ## indicspecies can only take a single
  ## survey.
  ## Do we match the bioregions to the survey
  ## which means not using combined surveys
  ## or cross all bioregions with all surveys.

  ## I prefer crossing, then filtering later.

  ## Only need polygons

  ## Turn off spherical geom for world map
  s2_used <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)


  ## TODO: turns out some samples had NA coordinates. Remove and checlk
  keep_rows <- complete.cases(all_bio_long$samps[[1]])

  ## Find clustering of each sample
  bio_as_sf <- sf::st_as_sf(all_bio_long$samps[[1]][keep_rows], coords = spatial_vars, crs = sf::st_crs(extrap_polygons$polygons[[1]]))

    bio_clustering <- sf::st_intersection(bio_as_sf, extrap_polygons$polygons[[1]])

  ## Plot to demonstrate alignment of clusters and samples
  ## env_bbox <- sf::st_bbox(extrap_polygons$polygons[[1]])

  ## tmap::tm_shape(extrap_polygons$polygons[[1]], bbox = env_bbox) +
  ##   tmap::tm_polygons(col = "clustering", style = "cont") +
  ##   tmap::tm_shape(bio_clustering) +
  ##   tmap::tm_symbols(col = "clustering", style = "cont", border.col = "green")

  ## Apply indicspecies

  ## Need site by species matrix
  ## and vector of clusters
  ## clusters is bio_clustering$clustering

  taxa_col_names <- all_bio_long$taxa[[1]]
  taxa_col_names[,taxon_col_name := make.names(taxon)]
  obs_col_names <- all_bio_long$obs[[1]][taxa_col_names, on = "taxon_id"]

  site_sp_wide <- data.table::dcast(obs_col_names, samp_id ~ taxon_col_name, value.var = "abund", fill = 0)
  ## TODO, drop NA rows or set to 0?
  site_sp_wide[site_sp_wide < 0] <- 0

  site_sp_wide <- site_sp_wide[keep_rows, ]
  site_sp_wide <- site_sp_wide[samp_id %in% bio_clustering$samp_id]
  clust_to_samp <- data.table::as.data.table(bio_clustering[, c("samp_id", "clustering")])
  clust_to_samp[, geometry := NULL]
  setkey(clust_to_samp, samp_id)

  ## Sometimes there is no overlap between biological samples and
  ## clustered region, or the samples all fall in one bioregion.

  if (length(unique(clust_to_samp$clustering)) < 2) {
      sf::sf_use_s2(s2_used)
      return(data.table::data.table(extrap_polygons[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat, clust_method)],
                                    matched = matched,
                                    spec_ind = list(NA),
                                    spec_ind_solo = list(NA)
                                    ))
  }

  ## samp_id matches between site species matrix and cluster vector
  ## sum(clust_to_samp$samp_id == site_sp_wide$samp_id)

  site_sp_wide[, samp_id := NULL]

  spec_ind <- indicspecies::multipatt(site_sp_wide, clust_to_samp$clustering)
  spec_ind_solo <- indicspecies::multipatt(site_sp_wide, clust_to_samp$clustering, duleg = TRUE)
  # site_coverage <- indicspecies::coverage(site_sp_wide, spec_ind_solo)
    sf::sf_use_s2(s2_used)
         return(data.table::data.table(extrap_polygons[, .(env_domain, env_year, env_pathway, res_gf, res_clust, trophic, survey, depth_cat, clust_method)],
                                                               matched = matched,
                                                               spec_ind = list(spec_ind),
                                                               spec_ind_solo = list(spec_ind_solo)))

  ## Pass it to indicspecies

  ## publish results

}


## Relevant plots

## Clusters that have samples. Maybe colour by number of samples
## This is complementary to env extrapolation
## Could also do env extrapolation score for cluster mean/median

## Clusters that have indicator species

## Best IndVal for each cluster, may be NA

## Alpha overlay of each cluster combination that has indicator species
## The idea is that groups of clusters can be shown, without

## Shiny app that shows species and cluster combinations when each
## cluster is selected
## For a polygon, pop up
## Cluster number
## indicator species for solo cluster
## cluster combinations that include this cluster, along with indicator species
