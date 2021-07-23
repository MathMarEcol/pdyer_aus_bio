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

  sites <- env_domain[sites,
                      c("site_id", spatial_vars, env_id_col),
                      on = spatial_vars,
                      with = FALSE]

  ## Add env_id_col to obs
  obs[sites, c(env_id_col) := (get(paste0("i.", env_id_col))), on = "site_id"]


  ## Drop sites and observations with no associated env_id
  sites_env <- sites[!is.na(get(env_id_col))]
  sites_env[, c(spatial_vars) := NULL]
  obs_env <- obs[!is.na(get(env_id_col))]



  ## Remove uncertain species
  obs_env %>%
    filter(abund == -999 | abund == -999000 | abund < 0) %>% ##uncertain val for zoo and phyto is -999
    dplyr::select(taxon_id) %>%
    dplyr::distinct() %>%
    dplyr::pull(taxon_id) -> uncertain_sp

  obs_env <- obs_env[!(taxon_id %in% uncertain_sp)]
  taxa <- taxa[!(taxon_id %in% uncertain_sp)]


  ## Merge sites into grid cells

  obs_env <- obs_env[, .(abund = agg_fun(abund)), by = c("taxon_id", "env_id")]
  sites_env <- sites_env[, .(site_id = list(c(site_id))), by = c("env_id")]


  ## Filter species by
  ## frequency
  ## coefficient of variation
  ## min_occurrences
  n_sites <-nrow(sites_env)

taxa_keep <- obs_env[ ,
        .(keep =
  .N >= min_occurrence & # within survey and depth, taxon occurs in more than min_occurrences locations
   sd(c(abund, rep(0, n_sites - .N))) / mean(c(abund, rep(0, n_sites - .N))) >= cov_min &
  .N/n_sites >= min(freq_range) &
  .N/n_sites <= max(freq_range),
  cov = sd(c(abund, rep(0, n_sites - .N))) / mean(c(abund, rep(0, n_sites - .N))),
  freq = .N/n_sites,
  min_occ = .N
        ),
    by = c("taxon_id")]

taxa_keep <- taxa_keep[keep == TRUE]

## if the number of taxa is still huge, keep the top
## max_taxa with the best coefficient of variation
  if (nrow(taxa_keep) > max_taxa) {
      taxa_keep <- head(taxa_keep[order(-cov)], n = max_taxa)
  }

obs_env <- obs_env[taxon_id %in% taxa_keep$taxon_id]


## Species data and env data are ready to be merged

taxa[, taxon_id_chr := paste0("sp.",taxon_id)]
obs_env[, taxon_id_chr := paste0("sp.",taxon_id)]

wide_surv <- env_domain[get(env_id_col) %in% sites_env[[env_id_col]]]
## wide_surv <- rbind(wide_surv, env_domain[1,])
wide_taxa <- data.table::dcast(obs_env, env_id ~ taxon_id_chr, value.var = "abund", fill = 0)
wide_taxa_env <- wide_taxa[wide_surv, on = env_id_col, nomatch = NA]

wide_taxa_env[, (unique(obs_env$taxon_id_chr)) :=
                  lapply(.SD, function(x){x[is.na(x)] <- 0}),
              .SDcols = unique(obs_env$taxon_id_chr) ]

 return(data.table(all_bio_long[,.(trophic, survey, depth_cat)],
                   wide_taxa_env = list(wide_taxa_env),
                   taxa = list(taxa),
                   obs_env = list(obs_env),
                   sites_env = list(sites_env)
                   ))


  ## Inner join biology and environment by grid cells
  ## bio_env <- all_bio_long[
  ##   env_domain[, c(spatial_vars, env_id_col), with = FALSE],
  ##   keyby = .(spatial_vars),
  ##   nomatch = NULL
  ## ]

  ## Save memory, drop spatial vars from bio_env
  ## look up spatial vars from env_domain when needed
  ## bio_env[, (spavtial_vars) := NULL] #

  ## Need to identify NO TAXA rows
  ## for zoo and phyto, they are "No Taxa" species
  ## microbes? Probably no such thing as an empty sample, but will check by comparing sites to otu
  ## Watson data should have all grid cells within the polygon.
  ##
  ## Create "No taxa" rows in all datasets to
  ## prevent dropping of sites unintentionally
  ## all_sites <- bio_env[,
  ##                      unique(.SD)
  ##                      by = c("survey", "trophic", "depth_cat"),
  ##                      .SDcols = env_id_col)

  ## ## with all_sites, the "No taxa" rows are redundant.
  ## bio_env <- bio_env[taxon != "No taxa"]


  ## Remove uncertain species
  ## bio_env %>%
  ##   filter(abund == -999 | abund == -999000) %>% ##uncertain val for zoo and phyto is -999
  ##   dplyr::select(taxon) %>%
  ##   dplyr::distinct() %>%
  ##   dplyr::pull(taxon) -> uncertain_sp

  ## bio_env <- bio_env[!taxon %in% uncertain_sp]


  ## Filter species by
  ## frequency
  ## coefficient of variation
  ## min_occurrences

##   ## Precalculate number of grid cells in each survey/trophic level at each depth.
##   n_grids <- all_sites[ , .(n_grids = .N), by = c("survey", "trophic", "depth_cat")]
## ## Data table of unique lat-lons per survey
##   ## datatable of freq (as ratio, using n_grids), cov, occ, per species within each survey, ignore "No taxa"
##   ## keep col for each species.
##   ## merge to keep good species

## bio_env[ ,
##         keep := (
##   .N >= min_occurrence & # within survey and depth, taxon occurs in more than min_occurrences locations
##    sd(abund) / mean(abund) >= cov_min &
##   .N/n_grids[.BY$survey == n_grids$survey &
##              .BY$trophic == n_grids$trophic &
##              .BY$depth_cat == n_grids$depth_cat]$n_grids >=
##      min(freq_range) &
##   .N/n_grids[.BY$survey == n_grids$survey &
##              .BY$trophic == n_grids$trophic &
##              .BY$depth_cat == n_grids$depth_cat]$n_grids <=
##    max(freq_range)
##         ),
##     by = c("survey", "trophic", "depth_cat", "taxon")]


##   keep_capped <- head(keep[order(-cov)], n = max_otu)

##   otu_short <- data.table(OTU = keep_capped$OTU,
##                           OTU_short = paste0("R16s.", seq.int(1, nrow(keep_capped))),
##                           key = "OTU"
##                           )

##   ret <- merge(microbe_samples, otu_short, by = "OTU")

}

