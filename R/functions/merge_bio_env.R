merge_bio_env <- function(
                          env_poly,
                          res_gf_target,
                          all_bio_long,
                          env_domain,
                          spatial_vars,
                          env_offset,
                          agg_fun,
                          env_id_col,
                          freq_range,
                          cov_min,
                          min_occurrence,
                          max_taxa,
                          env_fitting

                          ) {

  env_year <- env_fitting
  env_pathway <- env_fitting

  env_domain <- env_domain[
    domain == env_poly$name &
      res == res_gf_target &
      env_year == env_year  &
      env_pathway == env_pathway, "data"][[1]][[1]]

  all_bio_long <- all_bio_long[, {
  ## Aggregate biological samples into grid cells
    copy_SD <- data.table::copy(.SD)
    samps_round <- data.table::data.table(
                                 round(
                                   (
                                     copy_SD$samps[[1]][,
                                                     c(spatial_vars),
                                                     with=FALSE]-
                                  env_offset
                                   ) / res_gf_target
                                 ) *
                                 res_gf_target +  env_offset)
    samps_round[, `:=`(samp_id = copy_SD$samps[[1]]$samp_id,
                       depth = copy_SD$samps[[1]]$depth)]
    ## Convert all longitudes to range [0, 360]
    samps_round[, c(spatial_vars[1]) := .(.SD[[spatial_vars[1]]] %% 360)]
  ## Clean up names
    taxa_clean <- data.table::data.table(taxon = clean_sp_names(.SD$taxa[[1]]$taxon), taxon_id = .SD$taxa[[1]]$taxon_id)

    out <- data.table(samps = list(samps_round), obs = list(.SD$obs[[1]]), taxa = list(taxa_clean))

    },
    by = c("trophic", "survey", "depth_cat")]
  ## Operates over each row of all_bio_long
  obs <- data.table::copy(all_bio_long$obs[[1]])
  samps <- data.table::copy(all_bio_long$samps[[1]])
  taxa <- data.table::copy(all_bio_long$taxa[[1]])


  ## Right outer join env to sites
  ## inner join won't work, because
  ## each env_id will match to multiple
  ## sites.
  ## sites with no env will be
  ## NA, and can be dropped

  grids <- env_domain[samps,
                      c("samp_id", spatial_vars, env_id_col),
                      on = spatial_vars,
                      with = FALSE]

  ## Add env_id_col to obs
  obs[grids, c(env_id_col) := (get(paste0("i.", env_id_col))), on = "samp_id"]


  ## Drop sites and observations with no associated env_id
  grids_env <- grids[!is.na(get(env_id_col))]
  grids_env[, c(spatial_vars) := NULL]
  obs_env <- obs[!is.na(get(env_id_col))]

  if(nrow(grids_env) == 0 | nrow(obs_env) == 0) {
    return(data.table::data.table(all_bio_long[,.(trophic, survey, depth_cat)],
                      env_domain = env_poly$name,
                      env_year = env_year,
                      env_pathway = env_pathway,
                      res_gf = res_gf_target,
                      wide_taxa_env = list(NA),
                      taxa = list(NA),
                      obs_env = list(NA),
                      grids_env = list(NA)
                      ))
  }


  ## drop "No_taxa"
  if ( length(taxa[taxon == "No_taxa"]$taxon_id) > 0) {
    obs_env <- obs_env[!(taxon_id == taxa[taxon == "No_taxa"]$taxon_id)]
  }


  ## Remove uncertain species
  obs_env %>%
    dplyr::filter(abund == -999 | abund == -999000 | abund < 0) %>% ##uncertain val for zoo and phyto is -999
    dplyr::select(taxon_id) %>%
    dplyr::distinct() %>%
    dplyr::pull(taxon_id) -> uncertain_sp

  obs_env <- obs_env[!(taxon_id %in% uncertain_sp)]
  taxa <- taxa[!(taxon_id %in% uncertain_sp)]


  ## Merge sites into grid cells

  ## Need to know how many samples total, not just non-zeros
  #site_times_est <- obs[, .(n_times = .N), by = c("site_id", "taxon_id")][,
  #                                                                        .(n_times = max(n_times)), by = site_id]
  grids_env <- grids_env[, .(n_samps = .N), by = env_id_col]
  ## grids_env <- grids_env[, .(site_id = list(c(site_id)), n_sites = length(c(site_id))), by = env_id_col]
  ## grid_times_est <- purrr::map2_df(sites_env$site_id, sites_env[[env_id_col]], ~{
  ##   out <- sum(site_times_est[site_id %in% .x, n_times])
  ##   ret <- data.table::data.table(n_total = out, .y)
  ##   names(ret) <- c("n_total", env_id_col)
  ##   return(ret)
  ##   })
  ## sites_env[grid_times_est , n_total := .(i.n_total), on = env_id_col]

  ## For each env_id and taxon combination, find the merged abundance
  ## Also need to merge across time.
  ## Don't have time factor, need to estimate
  grid_taxa_cross <- data.table::CJ(grids_env[[env_id_col]], taxa$taxon_id)
  names(grid_taxa_cross) <- c(env_id_col, "taxon_id")
  grid_taxa_cross[grids_env, c("n_samps")  := .(i.n_samps), on = env_id_col]
  data.table::setkeyv(obs_env, c("taxon_id", env_id_col))
  data.table::setkeyv(grid_taxa_cross, c("taxon_id", env_id_col))
  ## Trying a new approach
  ## Previous approach was to get a group inside DT `[`
  ## then use that group to look up rows in another DT.
  ## If the group existed, then do a DT `[` within the
  ## outer DT `[`, extracting the relevant rows and aggregating.
  ## Empty groups returned 0.
  ## However, DT `[` operations are very powerful, but expensive to set up.
  ## In other places, I've found they take as much time as inverting a 28x28 matrix.
  ## Previous approach called DT `[` twice for every group, and one of those times
  ## was just a check for a non-empty set.
  ## New approach is merge, to eliminate empty groups, then a single, simple DT `[`
  ## to apply an aggregating function to one col of each non-empty group.
  obs_env_total <- obs_env[grid_taxa_cross, nomatch = NULL]
  obs_env_agg <- obs_env_total[, .(abund = agg_fun(
                            c(.SD$abund,
                              rep(0, unique(.SD$n_samps) - length(.SD$abund))
                              )
                          )),
                  by = c("taxon_id",env_id_col)]
  grid_env_bio <- obs_env_agg[grid_taxa_cross, on= c("taxon_id",env_id_col), nomatch=NA]
  grid_env_bio[, c("n_samps") := NULL]
  grid_env_bio[, abund := data.table::fifelse(is.na(abund), 0, abund)]

  ## grid_env <- grid_taxa_cross[,
  ##                 {
  ##                   n_total <- .SD$n_total
  ##                   grp_tax <- .BY$taxon_id
  ##                   grp_env <- .BY[[env_id_col]]
  ##                   subdata <- obs_env[.(grp_tax, grp_env)]
  ##                   if(nrow(subdata) == 0) {
  ##                     ret <- 0
  ##                   } else {
  ##                     ret <- subdata[,
  ##                     {
  ##                       .(abund = agg_fun(
  ##                           c(abund,
  ##                             rep(0, n_total - length(abund))
  ##                             )
  ##                         ))
  ##                     }
  ##                     ]
  ##                   }
  ##                 },
  ##                 by = c(env_id_col, "taxon_id")]

  ## names(grid_env)[3] <- "abund"


  ## Filter species by
  ## frequency
  ## coefficient of variation
  ## min_occurrences
  n_sites <- nrow(grids_env)

  taxa_keep <- grid_env_bio[ ,
                        .(cov = sd(abund) / mean(abund),
                          freq = sum(abund != 0)/.N,
                          occ = sum(abund != 0)
                          ),
                        by = c("taxon_id")]

  taxa_keep[, keep :=
                occ >= min_occurrence & # within survey and depth, taxon occurs in more than min_occurrences locations
                cov >= cov_min &
                freq >= min(freq_range) &
                freq <= max(freq_range)]

  taxa_keep <- taxa_keep[keep == TRUE]

  ## if the number of taxa is still huge, keep the top
  ## max_taxa with the best coefficient of variation
  if (nrow(taxa_keep) > max_taxa) {
      taxa_keep <- head(taxa_keep[order(-cov)], n = max_taxa)
  }

  grid_env_bio <- grid_env_bio[taxon_id %in% taxa_keep$taxon_id]


## Species data and env data are ready to be merged

  taxa[, taxon_id_chr := paste0("sp.",taxon_id)]
  grid_env_bio[, taxon_id_chr := paste0("sp.",taxon_id)]

  wide_surv <- env_domain[get(env_id_col) %in% grids_env[[env_id_col]]]
    ## wide_surv <- rbind(wide_surv, env_domain[1,])
  if (nrow(grid_env_bio) == 0) {
    return(data.table::data.table(all_bio_long[,.(trophic, survey, depth_cat)],
                                  env_domain = env_poly$name,
                                  env_year = env_year,
                                  env_pathway = env_pathway,
                                  res_gf = res_gf_target,
                                  wide_taxa_env = list(NA),
                                  taxa = list(NA),
                                  obs_env = list(NA),
                                  grids_env = list(NA)
                                  ))
  }

    wide_taxa <- data.table::dcast(grid_env_bio, env_id ~ taxon_id_chr, value.var = "abund", fill = 0)
    wide_taxa_env <- wide_taxa[wide_surv, on = env_id_col, nomatch = NA]

    ## wide_taxa_env[, (unique(obs_env$taxon_id_chr)) :=
    ##                   lapply(.SD, function(x){x[is.na(x)] <- 0}),
    ##               .SDcols = unique(obs_env$taxon_id_chr) ]

  return(data.table::data.table(all_bio_long[,.(trophic, survey, depth_cat)],
                                env_domain = env_poly$name,
                                env_year = env_year,
                                env_pathway = env_pathway,
                                res_gf = res_gf_target,
                                wide_taxa_env = list(wide_taxa_env),
                                taxa = list(taxa),
                                obs_env = list(grid_env_bio),
                                grids_env = list(grids_env)
                                ))
}
