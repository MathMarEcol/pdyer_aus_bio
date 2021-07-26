merge_bio_env <- function(
                          env_domain,
                          all_bio_long,
                          spatial_vars,
                          regrid_res,
                          env_offset,
                          agg_fun,
                          env_id_col,
                          freq_range,
                          cov_min,
                          min_occurrence,
                          max_taxa

                          ) {

  ## Operates over each row of all_bio_long

  obs <- data.table::copy(all_bio_long$obs[[1]])
  sites <- data.table::copy(all_bio_long$sites[[1]])
  taxa <- data.table::copy(all_bio_long$taxa[[1]])


  ## Right outer join env to sites
  ## inner join won't work, because
  ## each env_id will match to multiple
  ## sites.
  ## sites with no env will be
  ## NA, and can be dropped

  sites <- env_domain$data[[1]][sites,
                      c("site_id", spatial_vars, env_id_col),
                      on = spatial_vars,
                      with = FALSE]

  ## Add env_id_col to obs
  obs[sites, c(env_id_col) := (get(paste0("i.", env_id_col))), on = "site_id"]


  ## Drop sites and observations with no associated env_id
  sites_env <- sites[!is.na(get(env_id_col))]
  sites_env[, c(spatial_vars) := NULL]
  obs_env <- obs[!is.na(get(env_id_col))]


  ## drop "No_taxa"
  obs_env <- obs_env[!(taxon_id == taxa[taxon == "No_taxa"]$taxon_id)]


  ## Remove uncertain species
  obs_env %>%
    filter(abund == -999 | abund == -999000 | abund < 0) %>% ##uncertain val for zoo and phyto is -999
    dplyr::select(taxon_id) %>%
    dplyr::distinct() %>%
    dplyr::pull(taxon_id) -> uncertain_sp

  obs_env <- obs_env[!(taxon_id %in% uncertain_sp)]
  taxa <- taxa[!(taxon_id %in% uncertain_sp)]


  ## Merge sites into grid cells

  ## Need to know how many samples total, not just non-zeros
  site_times_est <- obs[, .(n_times = .N), by = c("site_id", "taxon_id")][,
                                                                          .(n_times = max(n_times)), by = site_id]
  sites_env <- sites_env[, .(site_id = list(c(site_id)), n_sites = length(c(site_id))), by = env_id_col]
  grid_times_est <- purrr::map2_df(sites_env$site_id, sites_env[[env_id_col]], ~{
    out <- sum(site_times_est[site_id %in% .x, n_times])
    ret <- data.table(n_total = out, .y)
    names(ret) <- c("n_total", env_id_col)
    return(ret)
    })
  sites_env[grid_times_est , n_total := .(i.n_total), on = env_id_col]

  ## For each env_id and taxon combination, find the merged abundance
  ## Also need to merge across time.
  ## Don't have time factor, need to estimate
  grid_taxa_cross <- data.table::CJ(sites_env[[env_id_col]], taxa$taxon_id)
  names(grid_taxa_cross) <- c(env_id_col, "taxon_id")
  grid_taxa_cross[sites_env, c("n_sites", "n_total")  := .(i.n_sites, i.n_total), on = env_id_col]
  grid_env <- grid_taxa_cross[,
                  {
                    n_total <- .SD$n_total
                    if(nrow(obs_env[taxon_id == .BY$taxon_id &
                            obs_env[[env_id_col]] == .BY[[env_id_col]]]) == 0) {
                      ret <- 0
                    } else {
                      ret <- obs_env[taxon_id == .BY$taxon_id &
                                     obs_env[[env_id_col]] == .BY[[env_id_col]],
                      {
                        .(abund = agg_fun(
                            c(abund,
                              rep(0, n_total - length(abund))
                              )
                          ))
                      }
                      ]
                    }
                  },
                  by = c(env_id_col, "taxon_id")]

  names(grid_env)[3] <- "abund"


  ## Filter species by
  ## frequency
  ## coefficient of variation
  ## min_occurrences
  n_sites <- nrow(sites_env)

  taxa_keep <- grid_env[ ,
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

  grid_env <- grid_env[taxon_id %in% taxa_keep$taxon_id]


## Species data and env data are ready to be merged

  taxa[, taxon_id_chr := paste0("sp.",taxon_id)]
  grid_env[, taxon_id_chr := paste0("sp.",taxon_id)]

  wide_surv <- env_domain$data[[1]][get(env_id_col) %in% sites_env[[env_id_col]]]
    ## wide_surv <- rbind(wide_surv, env_domain[1,])
    if (nrow(grid_env) == 0) {
    return(data.table(all_bio_long[,.(trophic, survey, depth_cat)],
                    wide_taxa_env = NA,
                    taxa = NA,
                    obs_env = NA,
                    sites_env = NA
                    ))
    }

    wide_taxa <- data.table::dcast(grid_env, env_id ~ taxon_id_chr, value.var = "abund", fill = 0)
    wide_taxa_env <- wide_taxa[wide_surv, on = env_id_col, nomatch = NA]

    ## wide_taxa_env[, (unique(obs_env$taxon_id_chr)) :=
    ##                   lapply(.SD, function(x){x[is.na(x)] <- 0}),
    ##               .SDcols = unique(obs_env$taxon_id_chr) ]

 return(data.table(all_bio_long[,.(trophic, survey, depth_cat)],
                   env_domain = env_domain$domain,
                   wide_taxa_env = list(wide_taxa_env),
                   taxa = list(taxa),
                   obs_env = list(grid_env),
                   sites_env = list(sites_env)
                   ))
}
