#' Libraries
## Tidyverse
library(purrr)
##Parallel
library(future)
library(furrr)
##Analysis
library(cluster)
library(readr)
library(tidyverse)
library(sf)
library(raster)
library(gradientForest)
library(gfbootstrap)
library(castcluster)
##Data
library(sdmpredictors)
##Plots
library(ggplot2)
library(ggthemes)
##Support
library(here)
##Drake
library(drake)

#' Custom Functions
split_surv <- function(combined_copepod,
                       matching) {
  return(dplyr::filter(combined_copepod, PROJECT_ID %in% matching))
}

remove_meso <- function(surv,
                        depth) {
  dplyr::filter(surv, SAMPLE_DEPTH < depth | is.na(SAMPLE_DEPTH))
}

clean_sp_names <- function(surv) {
  surv %>%
    dplyr::filter(Z_TYPE != "No taxa") %>%
    dplyr::mutate(SPECIES = stringr::str_replace(SPECIES, " ", "_")) %>%
    dplyr::select(SPECIES) %>%
    dplyr::distinct(SPECIES) %>%
    dplyr::pull(SPECIES)
}

surv_to_wide <- function(surv) {
  surv %>%
    dplyr::group_by_at(1:7) %>%
    dplyr::summarise(ABUNDANCE_M3 = sum(ABUNDANCE_M3)) %>%
    dplyr::ungroup() %>%
    ##First, replace spaces in species names with underscore
    dplyr::mutate(SPECIES = stringr::str_replace(SPECIES, " ", "_")) %>%
    tidyr::spread(key = SPECIES, value = ABUNDANCE_M3, fill = 0) %>%
    ##May need a column rename here
    dplyr::mutate(No_taxa = NULL,
                  SAMPLE_DEPTH = NULL,
                  Z_TYPE = NULL,
                  PROJECT_ID = NULL,
                  SAMPLE_DATE = NULL)
}

env_round_label <- function(env_data,
                            spatial_vars,
                            env_res,
                            env_offset,
                            env_id_col) {
  env_round <- rphildyerphd::align_sp(env_data, spatial_cols = spatial_vars,
                                    res = env_res, offset = env_offset,
                                    fun = mean)
  env_round[[env_id_col]] <- seq_len(nrow(env_round))
  return(env_round)
}

get_env_names <- function(env_round, spatial_vars, env_id_col){
  names(env_round)[!names(env_round) %in% c(spatial_vars, env_id_col)]
}

align_env_samp <- function(surv,
                           spatial_vars,
                           env_res,
                           env_offset,
                           env_round) {
  ##only keep sites in both env_round and surv_round
  surv %>%
    dplyr::rename(lat = LATITUDE, lon = LONGITUDE) %>%
    rphildyerphd::align_sp(spatial_cols = spatial_vars,
                           res = env_res, offset = env_offset,
                           fun = mean) %>%
    merge(env_round, by = spatial_vars) -> surv_env
  return(surv_env)
}

foc_cov_filter <- function(surv_env,
                           sp_names,
                           freq_range,
                           cov_min,
                           min_occurrence
                           ) {

  surv_sp_freq <- surv_env %>%
    dplyr::select(all_of(sp_names)) %>%
      dplyr::summarise_all(
               function(x) {
                 sum(x != 0) / length(x)
               }
             ) %>%
        tidyr::gather(key = "species", value = "freq")

  surv_sp_occ <- surv_env %>%
    dplyr::select(all_of(sp_names)) %>%
    dplyr::summarise_all(
             function(x) {
               sum(x != 0)
             }
           ) %>%
    tidyr::gather(key = "species", value = "occ")

  surv_sp_cov <- surv_env %>%
    dplyr::select(all_of(sp_names)) %>%
    dplyr::summarise_all(
             function(x) {
               sd(x) / mean(x)
             }
           ) %>%
    tidyr::gather(key = "species", value = "cov")


  surv_sp_f_cov <- inner_join(surv_sp_freq, surv_sp_cov, by = "species")
  surv_sp_f_cov_occ <- inner_join(surv_sp_f_cov, surv_sp_occ, by = "species")

  surv_sp_names  <- surv_sp_f_cov_occ %>% dplyr::filter(freq >= freq_range[1] &
                         freq <= freq_range[2] &
                         cov >= cov_min &
                         occ > min_occurrence) %>%
      dplyr::pull(species)

  return(surv_sp_names)

}

filter_surv_env <- function(surv_env,
                            surv_sp_names,
                            env_id_col,
                            spatial_vars,
                            env_vars) {
  surv_env_filter <- surv_env %>%
    dplyr::select(!!env_id_col, !!spatial_vars, !!env_vars, !!surv_sp_names)
}
env_aus_eez <- function(bio_oracle_cache,
                        env_vars,
                        env_modes,
                        env_extent,
                        max_depth,
                        regrid_res,
                        spatial_vars,
                        bio_oracle_str_template = "BO2_%s%s_ss"
                        ) {


  env_pairs <- data.table::as.data.table(
                             merge.data.frame(env_vars, env_modes, all = TRUE)
                           )

  env_bio_oracle_names <- apply(env_pairs[x != "depth"], 1,
                                function(x) {
                                  sprintf(bio_oracle_str_template, x[1], x[2])
                                })
  ## Add bathymetry separately
  env_bio_oracle_names <- c(env_bio_oracle_names, "MS_bathy_5m")


  env_raster <- sdmpredictors::load_layers(env_bio_oracle_names,
                                           datadir = bio_oracle_cache,
                                           rasterstack = FALSE)

  aeez_target_grid <- raster::raster(x = raster::extent(env_extent),
                                     resolution = regrid_res,
                                     crs = "+proj=longlat +datum=WGS84")

  raster_crop <- raster::brick(lapply(env_raster,
                                      function(r) {
                                        raster::crop(r, env_extent)
                                      }))
  raster_rescale <- raster::resample(raster_crop,
                                     aeez_target_grid,
                                     method = "bilinear")

  env <- raster::rasterToPoints(raster_rescale)
  env_complete <- as.data.frame(env[complete.cases(env), ])
  return(env_complete)
}

env_log_transform <- function(env_data, env_log) {
  ## Transformations by log10
  cv_min <- min(env_data$BO2_curvelrange_ss)
  cv_min_offset <- cv_min + cv_min / 10

  env_data$BO2_curvelrange_ss <- env_data$BO2_curvelrange_ss - cv_min_offset
  purrr::walk(env_log, ~{
    env_data[[.x]] <- log10(env_data[[.x]])
  })
  return(env_data)
}

env_clip_extremes_tuned <- function(env_data, env_limits) {

  ## Clipping extremes, post logging
  purrr::walk(names(env_limits), ~ {
    min_x <- min(env_limits[[.x]])
    max_x <- max(env_limits[[.x]])
    env_data[[.x]] <- pmax(min_x, pmin(env_data[[.x]], max_x))
  })
  return(env_data)
}
env_clip_extremes <- function(env_data, std_thres) {

  ## Clipping extremes, post logging
  purrr::walk(names(env_data), ~{
    x_sd <- sd(env_data[[.x]])
    x_mean <- mean(env_data[[.x]])
    min_x <- x_mean - x_sd * std_thres
    max_x <- x_mean + x_sd * std_thres
    env_data[[.x]] <- pmax(min_x, pmin(env_data[[.x]], max_x))
  })
  return(env_data)
}

env_name_spatial <- function(env_data, spatial_vars) {
  names(env_data)[1:2] <- spatial_vars
  return(env_data)
}

env_add_x_row <- function(env_data) {
  cbind(x_row = seq.int(1, nrow(env_data)), env_data)
}

env_merge_spatial <- function(env_predicted, env_spatial, spatial_vars) {
  assertthat::assert_that(assertthat::has_name(env_predicted, "x_row"))
  assertthat::assert_that(assertthat::has_name(env_spatial, spatial_vars))

  env_spatial_x_row <- cbind(x_row = seq.int(1, nrow(env_spatial)), env_spatial)
  return(merge(env_predicted, env_spatial_x_row[, c("x_row", spatial_vars)]))
}

env_wide_list <- function(env_data){
  type_names <- unique(env_data$type)
  type_names_compact <- type_names[type_names != "points" ]
  env_wide <- lapply(type_names_compact, function(ty, env_data) {
    tmp <- tidyr::pivot_wider(env_data[env_data$type == ty, c("var", "x_row", "y") ], names_from = "var", values_from = "y")
    tmp <- tmp[order(tmp$x_row), ]
    return(tmp)
  }, env_data = env_data)
  names(env_wide) <- type_names_compact
  ##add in points, as a wide-long
  if("points" %in% type_names){
    env_wide$points <- tidyr::pivot_wider(
                                env_data[env_data$type == "points",
                                         c("var", "x_row", "y", "gf_model") ],
                                names_from = "var", values_from = "y")
  }
  return(env_wide)
}

cast_compact_stats <- function(aff_thres, sim_mat) {
  cast_combi <- castcluster::cast_alg(sim_mat = sim_mat, aff_thres = aff_thres)
  cast_combi_sta <- castcluster::cast_stabilize(cast_obj = cast_combi, sim_mat = sim_mat, aff_thres = aff_thres)
  cast_combi_com <- castcluster::cast_compact(cast_ob = cast_combi_sta, sim_mat = sim_mat, aff_thres = aff_thres)

  mem_mat <- castcluster::membership_mat(cast_combi_com)
  h_com <- gfbootstrap::hubert_gamma(sim_mat, mem_mat, norm_z = TRUE)
  k_com <- length(cast_combi_com)

  stats <- data.frame(aff = aff_thres, type = "compact", norm_z = TRUE, h = h_com, k = k_com)
  return(list(stats = stats, aff_thres = aff_thres, cast_compact = cast_combi_com))
}

get_cast_stats <- function(cast_sweep){
  all_stats <- do.call("rbind", lapply(cast_sweep, function(x) {
    x$stats
  }))
  return(all_stats)
}

get_cast_spatial <- function(cast_ob, spatial_env, spatial_vars){
  clustering <- do.call("rbind",
                        lapply(seq_along(cast_ob), function(i, cast) {
                          data.frame(x_row = cast[[i]], clust = rep(i, length(cast[[i]])))
                        }, cast = cast_ob)
                        )

  clust_spat <- unique(merge(x = spatial_env[,c("x_row", spatial_vars)], y = clustering, by = "x_row" ))
  return(clust_spat)
}

##TODO: this is not scaleable to more than about 10,000 sites
##More sites should increase the sparsity, but would require a lot of
##code and algorithm modifications
all_pairs_diag <- function(n) {

  d_ij <- expand.grid(i = seq.int(n), j = seq.int(n))

  d_ij_diag <- d_ij[d_ij$i <= d_ij$j, ]

}

sort_between <- function(sim_mat, cast_ob) {
  aff_btw <- aff_cluster_between(sim_mat = sim_mat, cast_obj = cast_ob)

  aff_btw_wide <- matrix(aff_btw$affs, sqrt(nrow(aff_btw)), sqrt(nrow(aff_btw)))

  aff_dist <- dist(aff_btw_wide)
  aff_sort <- hclust(aff_dist)

  return(aff_sort$order)
}

##awkward version, assumes that the env_trans_wide is
##just the points
pair_dist <- function(pairs, env_trans_wide, env_vars) {

  assertthat::assert_that(all(env_vars %in% names(env_trans_wide)))

  avg_points <- nrow(env_trans_wide) / length(unique(env_trans_wide$x_row))
  assertthat::assert_that(length(env_vars) <= avg_points)

  dists <- apply(pairs, 1, function(p, env_trans, env_vars){
    i <- p[1]
    j <- p[2]
    if (i == j) {
      return(data.frame(i = i, j = j, dist = 1))
    }

    i_points <- env_trans[env_trans$x_row == i, env_vars]
    j_points <- env_trans[env_trans$x_row == j, env_vars]

    dist <- rrcov::T2.test(x = i_points, y = j_points)
    return(data.frame(i = c(i, j), j = c(j, i), dist = dist$p.value))
  }, env_trans = env_trans_wide, env_vars = env_vars)
  return(dplyr::bind_rows(dists))
}

plot_extents <- function(marine_map,
                         env_extent,
                         out_file,
                         units = "cm",
                         width = 16,
                         height = 9,
                         dpi = 300
                         ) {
  ext_pl <- ggplot(marine_map, aes(x = x, y = y)) +
    geom_sf(data = marine_map,
            inherit.aes = FALSE,
            color = "black",
            fill = NA) +
    coord_sf(xlim = env_extent$x, ylim = env_extent$y) +
    theme_tufte()

  ggsave(out_file,
         plot = ext_pl,
         units = units,
         width = width, #widescreen
         height = height,
         dpi = dpi
         )
}

plot_temp <- function(env_data,
                      spatial_vars,
                      marine_map,
                      env_extent,
                      out_file,
                      units = "cm",
                      width = 16,
                      height = 9,
                      dpi = 300
                      ) {

  ext_pl_biooracle <- ggplot(env_data[, c(spatial_vars, "BO2_tempmean_ss")],
         aes(x = lon, y = lat, fill = BO2_tempmean_ss)) +
    geom_raster() +
    geom_sf(data = marine_map, inherit.aes = FALSE,
            color = "black", fill = NA) +
    labs(fill = "Mean ss Temp") +
    coord_sf(xlim = env_extent$x, ylim = env_extent$y) +
    theme_tufte()

  ggsave(out_file,
    plot = ext_pl_biooracle,
    units = units,
    width = width, #widescreen
    height = height,
    dpi = dpi
  )

}

ggsave_wrapper <- function(filename, plot,
                           units = "cm",
                           width = 16,
                           height = 9,
                           dpi = 300,
                           scale = 2
                           ){
  ggsave(filename =  filename,
         plot = plot,
         units = units,
         width = width,
         height = height,
         dpi = dpi,
         scale = scale
         )
}

gf_plot_wrapper <- function(gf_model,
                            plot_type,
                            vars,
                            out_file,
                            units = "cm",
                            width = 16,
                            height = 9,
                            dpi = 300
) {
  png(filename = out_file,
      units = units,
      width = width,
      height = height,
      res = dpi)
  plot(gf_model, plot.type = plot_type, imp.vars = names(importance(gf_model))[vars])
  dev.off()
}

state_rds <- function(rds_path, yaml_path) {
  total_state <- rutilities::track_all_states()
  saveRDS(file = rds_path, object = total_state)
  cat(yaml::as.yaml(total_state, line.sep = "\n", column.major = FALSE), file = yaml_path)
}

## I am taking my code from here:
## [[file:/vmshare/phd/projects/aus_bioregions_paper/experiments/2019-06-28-1618_gf_models_kmeans/method_copepod.Rmd][file:/vmshare/phd/projects/aus_bioregions_paper/experiments/2019-06-28-1618_gf_models_kmeans/method_copepod.Rmd]]
#' Plan

## Set up variables

mapfile_location <- here::here(
                            "..", "..", "..",
                            "Q1215", "ShapeFiles", "World_EEZ_v8"
                          )
biooracle_folder <- here::here(
                            "..", "..", "..",
                            "Q1215", "bioORACLE"
                          )
copepod_data <- here::here(
                        "..", "..", "..",
                        "Q1215", "AusCPR", "combined_copeped_jul19.csv"
                      )
ext_pl_temp_file <- here::here("outputs", "temps.png")
ext_pl_map_file <- here::here("outputs", "extents.png")
state_rds_file <- here::here("outputs", "state.rds")
state_yaml_file <- here::here("outputs", "state.yaml")

pl_gf_range_file <- here::here("outputs", "gf_range.png")
pl_gf_density_file <- here::here("outputs", "gf_density.png")
pl_gf_cumimp_file <- here::here("outputs", "gf_cumimp.png")
pl_gf_perf_file <- here::here("outputs", "gf_perf.png")

pl_gfboot_range_file <- here::here("outputs", "gfboot_range.png")
pl_gfboot_density_file <- here::here("outputs", "gfboot_density.png")
pl_gfboot_cumimp_file <- here::here("outputs", "gfboot_cumimp.png")
pl_gfboot_perf_file <- here::here("outputs", "gfboot_perf.png")

pl_gfboot_cumimp_file <- here::here("outputs", "gfboot_cumimp.png")

pl_copepod_aff_h_file <- here::here("outputs", "copepod_aff_h.png")
pl_copepod_clust_map_file <- here::here("outputs", "copepod_clust_map.png")
pl_copepod_p_mat_diag_file <- here::here("outputs", "copepod_p_mat_diag.png")

pl_copepod_aff_h_full_file <- here::here("outputs", "copepod_aff_h_full.png")
pl_copepod_clust_map_full_file <- here::here("outputs", "copepod_clust_map_full.png")
pl_copepod_p_mat_full_file <- here::here("outputs", "copepod_p_mat_full.png")


jobs <- 5

## parameters

               epi_depth = 200
               freq_range = c(0.05, 1)
               min_occurrence = 6
               cov_min = 1.0

               mapLayer =  "World_EEZ_v8_2014_HR"
               pred = list()
               env_vars = c("depth",
                            "temp",
                            "nitrate",
                            "silicate",
                            "chlo",
                            "iron",
                            "salinity",
                            "curvel")
               env_modes = c("min", "max", "mean", "range")
               spatial_vars =  c("lon", "lat")
               env_id_col = "env_id"
               bio_oracle_str_template = "BO2_%s%s_ss"
               env_offset = 0
               max_depth = 1500
               ##in lat lon degrees, use 1/integer fraction
               ##for proper rastering later,
               ##currently 1/12 to allign with BioORACLE
               regrid_resolution = 1 / 4#TODO: 1 / 12,
               ##Extent chosen to match the largest extents of
               ##the Aus EEZ polygon and the FRDC benthic data
               ##FRDC is not being used, but previous effort
               ##has used this extent and the full sampling of the GoC is useful
               env_extent = list(x = c(109 + 1 / 24, 163 + 23 / 24),
                                 y = c(-47 - 23 / 24, -8 - 1 / 24))
               gf_trees = 200
               gf_bins = 201
               gf_corr_thres = 0.5
               extrap = 1 / 4
               ##The following variables work better in log scale.
               ##If a variable is log transformed,
               ##clipping will take place on the log scale
               env_log = c(
                 "BO2_nitratemin_ss",
                 "BO2_silicatemin_ss",
                 "BO2_ironmin_ss",
                 "BO2_curvelmin_ss",
                 "BO2_nitratemax_ss",
                 "BO2_silicatemax_ss",
                 "BO2_ironmax_ss",
                 "BO2_curvelmax_ss",
                 "BO2_nitratemean_ss",
                 "BO2_silicatemean_ss",
                 "BO2_ironmean_ss",
                 "BO2_curvelmean_ss",
                 "BO2_nitraterange_ss",
                 "BO2_silicaterange_ss",
                 "BO2_ironrange_ss",
                 "BO2_curvelrange_ss"
               )
               ##For each predictor, I have specified limits.
               ##Not all variables hit the limits, shwon in comment
               ##The limits are generally around 3 standard deviations
               ##from the mean, unless specified in comments
               env_limits_sd <- 3
               env_limits = list(
                 ##no clipping
                 "BO2_tempmin_ss" = c(0, 30),
                 ##Log, within limits
                 "BO2_nitratemin_ss" = c(-16, 3),
                 ##Log, upper limits in SO and in a few coastal sites,
                 ##lower limits off east coast Tas, ~2std.
                 ##Setting scale to avoid clipping lower limits
                 "BO2_silicatemin_ss" = c(-Inf, 2.5),
                 ##Clipping upper extremes, mostly off southern
                 ##Tas coast, ~1.7std
                 "BO2_chlomin_ss" = c(0, 0.25),
                 ##Log, clipping upper extremes, all coastal ~2.6std
                 "BO2_ironmin_ss" = c(-Inf, -5.75),
                 ##Upper extreme in Bight, clipped ~1.4std, lower
                 ##extremes on tropical coasts ~2std
                 "BO2_salinitymin_ss" = c(32.15, 35.72),
                 ##Log, lower extreme in bight and other southern
                 ##coasts, ~3std. No upper extremes
                 "BO2_curvelmin_ss" = c(-3.32, Inf),
                 ##no clipping
                 "BO2_tempmax_ss" = c(8, 32),
                 ##Log, no upper clipping due to lack of extremes,
                 ##no lower clipping as almost all low values are
                 ##deep water and don't look like outliers. Log transform
                 ##brings out tropical patterns, without log, the SO
                 ##gradient is prominent.
                 "BO2_nitratemax_ss" = c(-13, 4),
                 ##Log, no lower extremes, upper extremes are
                 ##tropical coastal ~2std
                 "BO2_silicatemax_ss" = c(0, 3.16),
                 ## very long upper tail, mostly along south australian
                 ##shelf, but not coastal ~2std
                 "BO2_chlomax_ss" = c(0, 1.12),
                 ##Log, no lower extremes, some upper extremes around
                 ##PNG ~2.5std
                 "BO2_ironmax_ss" = c(-11, -5.2),
                 ##Upper and lower extremes along coasts ~2.5std
                 "BO2_salinitymax_ss" = c(34.2, 36.6),
                 ##Log, no upper extremes, lower extremes are randomly
                 ##placed ~2.5std
                 "BO2_curvelmax_ss" = c(-2.5, 0.5),
                 ##No clipping
                 "BO2_tempmean_ss" = c(6, 30),
                 ##Log, no clipping, extremes are in deep waters in
                 ##GoC but are part of the long tail
                 "BO2_nitratemean_ss" = c(-14, 3.1),
                 ##Log, no lower extremes just gradient into SO,
                 ##upper extremes on tropical coasts
                 "BO2_silicatemean_ss" = c(0.3, 2.8),
                 ##Very long upper tail, some in tropical coasts,
                 ##most along southern shelf edge and Tas ~2std
                 "BO2_chlomean_ss" = c(0, 0.48),
                 ##Log, no lower extremes, upper extremes are coastal,
                 ##mostly in GoC and Bight
                 "BO2_ironmean_ss" = c(-12, -5.6),
                 ##extremes are all coastal
                 "BO2_salinitymean_ss" = c(33.7, 36.1),
                 ##Log, lower extremes are randomly placed, no upper extremes,
                 "BO2_curvelmean_ss" = c(-4.7, 0),
                 ##no lower extremes, upper extremes are coastal
                 "BO2_temprange_ss" = c(0, 9.5),
                 ##Log, no clipping, extremes are deep water in GoC
                 "BO2_nitraterange_ss" = c(-14, 3),
                 ##Log, no lower extremes, upper extremes are tropical
                 ##coastal ~2std
                 "BO2_silicaterange_ss" = c(-1.2, 2.65),
                 ##very long upper tail, around Indonesia, south shelf
                 ##edge of Australia and Tas ~2std
                 "BO2_chlorange_ss" = c(0, 0.96),
                 ##Log, lower extreme in SO, upper extreme in GoC ~2std
                 "BO2_ironrange_ss" = c(-10.7, -6),
                 ##no lower extreme, upper extremes are all tropical coastal
                 ##~2std
                 "BO2_salinityrange_ss" = c(0, 2.9),
                 ##Log, no upper extreme, lower extremes are random ~2std
                 "BO2_curvelrange_ss" = c(-4.6, 0.4),
                 ##lower extremes in deep trences. no upper extremes
                 "MS_bathy_5m" = c(-6000, 0)
               )



pl <- drake::drake_plan(
               marine_map = target(sf::st_read(file_in(!!mapfile_location),
                                               layer = mapLayer),
                                   hpc = FALSE), #Workers can't see the same TMPDIR
               ## ausEEZ = marine_map[marine_map$Country == "Australia", ],
               env_complete = target(env_aus_eez(bio_oracle_cache = file_in(!!biooracle_folder),
                                          env_vars = env_vars,
                                          env_modes = env_modes,
                                          env_extent = env_extent,
                                          max_depth = max_depth,
                                          regrid_res = regrid_resolution,
                                          bio_oracle_str_template =
                                            "BO2_%s%s_ss"),
                                   hpc = FALSE), #Workers can't see the same TMPDIR
               ## env_logged = env_log_transform(env_data = env_complete,
               ##                                env_log = env_log),
               env_clipped = env_clip_extremes(env_data = env_complete,
                                               std_thres = env_limits_sd),
               env_final = env_name_spatial(env_data = env_clipped,
                                            spatial_vars = spatial_vars),
               env_round = env_round_label(env_data = env_final,
                                           spatial_vars = spatial_vars,
                                           env_res = regrid_resolution,
                                           env_offset = env_offset,
                                           env_id_col = env_id_col),
               env_names = get_env_names(env_round = env_round,
                                         spatial_vars = spatial_vars,
                                         env_id_col = env_id_col),
               ## here I have referred to a variable defined above,
               ##copepod_csv copepod_csv is just a string, which will
               ##be passed to read_csv. first, I wrap the string inside
               ##file_in, so that drake knows it is a filename, that
               ##I read from the file, and that the file should be tracked.
               ##files cannot be stored in variables, must be a string.
               combined_copepod = target(
                 readr::read_csv(
                          file_in(!!copepod_data),
                          na = c("(null)", "."),
                          col_types =
                            readr::cols(PROJECT_ID = col_character(),
                                        SAMPLE_DEPTH = col_number()),
                          ),
                 hpc = FALSE), #Workers can't see the same TMPDIR
               ##I had a lambda (unnamed) function here, but moved it to the
               ##custom funtion section
#
               ##drake_plan() forces you to put commas everywhere,
               ##this is not an R block.
#
               surv = target(split_surv(combined_copepod, matching),
                             ## supplying the initial splitting of the data
                             ##here How to use transform parameters the map()
                             ##function steps through "rows of a grid" the
                             ##nth target uses the nth entry of each object
                             ##in map() so the second target below uses names
                             ##= "cpr" and matching = "CPR" If you want all
                             ##combinations, see cross().
                             ##
                             ## Notice that variables passed to split_surv()
                             ##can come from all different places in
                             ##drake_plan().
                             ##eg. combined_copepod is another target.
                             ##
                             ##matching is provided by the tranform = map(...)
                             ##just below.
                             ##within map(), .id is provided by another
                             ##parameter from map(). In the surv_epi target
                             ##map(), .id is using the names object from
                             ##the surv target map(). So most map() params
                             ##can be hard coded or targets, but .id must
                             ##be assigned another map() param
                       transform = map(
                         surv_names = c("nrs",
                                   "cpr",
                                   "mkinnon",
                                   "goc",
                                   "nyan",
                                   "anita"),
                         .id = surv_names,
                         matching = list(nrs = "NRS",
                                         cpr = "CPR",
                                         mckinnon =
                                           as.character(
                                             c(4, 5, 7, 9, 12, 15, 16, 24)
                                           ), #McKinnon surveys
                                         goc = "1", #Gulf of Capentaria
                                         nyan = "21", #SE Tasmania
                                         anita = "18") #Tasmania data
                         )
                       ),
#
         #From now on, every call to surv should give me one survey at a time.
         surv_epi = target(
           remove_meso(surv, depth = epi_depth),
           transform = map(
             surv,
             .id = surv_names
           )
         ),
#
         ##Extract species names
         sp_names = target(
           clean_sp_names(surv_epi),
           transform = map(
             surv_epi,
             .id = surv_names
           )
         ),
         ##Convert to wide format
         surv_wide = target(
           surv_to_wide(surv_epi),
           transform = map(
             surv_epi,
             .id = surv_names
           )
         ),
#
         ##Align env and samples
         surv_env = target(
           align_env_samp(surv_wide,
                          spatial_vars = spatial_vars,
                          env_res = regrid_resolution,
                          env_offset = env_offset,
                          env_round = env_round),
           transform = map(
             surv_wide,
             .id = surv_names
           )
         ),
         ##Filter by Frequency of occurrence and coefficient of variance
         surv_sp_keep = target(
           foc_cov_filter(surv_env = surv_env,
                          sp_names = sp_names,
                          freq_range = freq_range,
                          cov_min = cov_min,
                          min_occurrence = min_occurrence
           ),
           transform = map(
             surv_env,
             sp_names,
             .id = surv_names
           )
         ),
         surv_env_filter = target(
           filter_surv_env(surv_env = surv_env,
                           surv_sp_names = surv_sp_keep,
                           env_id_col = env_id_col,
                           spatial_vars = spatial_vars,
                           env_vars = env_names
           ),
           transform = map(
             surv_env,
             surv_sp_keep,
             .id = surv_names
           )
         ),
         ##Fit GF models
         surv_gf = target(
           gradientForest::gradientForest(
                             data = as.data.frame(surv_env_filter),
                             predictor.vars = env_names,
                             response.vars = surv_sp_keep,
                             ntree = gf_trees,
                             compact = T,
                             nbin = gf_bins,
                             transform = NULL,
                             corr.threshold = gf_corr_thres,
                             maxLevel = floor(log2(length(surv_sp_keep) * 0.368 / 2)),
                             trace = TRUE
                           ),
           transform = map(
             surv_env_filter,
             surv_sp_keep,
             .id = surv_names
           )
         ),
         ##combined GF for copepods
         copepod_combined_gf = target(
           gradientForest::combinedGradientForest(surv_gf,
                                            nbin = gf_bins),
           transform = combine(surv_gf)
         ),


         ##Transform the environment. No need for target(), I am not mapping or combining
         env_trans = predict(object = copepod_combined_gf,
                                          newdata = env_round[, env_names],
                                          extrap = extrap),

         env_trans_spatial = cbind(env_round[, spatial_vars], env_trans)
         ## env_trans_spatial = env_merge_spatial(env_trans, env_round, spatial_vars),
         ## env_trans_wide = env_wide_list(env_trans_spatial),

         ## ## CASTeR variant of clustering

         ## ##Fit GF models
         ## surv_gf = target(
         ##   gfbootstrap::bootstrapGradientForest(
         ##                     as.data.frame(surv_env_filter),
         ##                     predictor.vars = env_names,
         ##                     response.vars = surv_sp_keep,
         ##                     nbootstrap = gf_trees,
         ##                     compact = T,
         ##                     nbin = gf_bins,
         ##                     transform = NULL,
         ##                     corr.threshold = gf_corr_thres,
         ##                     maxLevel = floor(log2(length(surv_sp_keep) * 0.368 / 2)),
         ##                     trace = TRUE
         ##                   ),
         ##   transform = map(
         ##     surv_env_filter,
         ##     surv_sp_keep,
         ##     .id = surv_names
         ##   )
         ## ),

         ## ##combined GF for copepods
         ## copepod_combined_gf = target(
         ##   gfbootstrap::combinedBootstrapGF(surv_gf,
         ##                                    nbin = gf_bins,
         ##                                    n_samp = gf_trees),
         ##   transform = combine(surv_gf)
         ## ),

         ## env_trans = predict(object = copepod_combined_gf,
         ##                                  newdata = env_round[, env_names],
         ##                                  type = c("mean", "variance", "points"),
         ##                                  extrap = extrap,
         ##                                  extrap_pow = extrap_pow),
         ## ##Hotellings p-value similiarity matrix, using diagonal covariance
         ## p_mat_diag_cov = rmethods:::hotellings_bulk(
         ##                      means = env_trans_wide$mean[, env_names],
         ##                      res_sq = env_trans_wide$variance[, env_names]
         ##                    ),

         ## ##cluster, using CAST
         ## cast_sweep = target(cast_compact_stats(aff_thres = aff_sweep,
         ##                                        sim_mat = p_mat_diag_cov),
         ##   transform = map(.id = aff_sweep,
         ##                   aff_sweep = !!(seq(0.05, 0.95, 0.05)))
         ## ),



         ## cast_sweep_list = target(list(cast_sweep),
         ##                      transform = combine(cast_sweep,
         ##                                          .id = aff_sweep)),

         ## ##Cluster the combined copepods
         ## ##choos the peak Hubert Gama
         ## cast_stats_split = target(cast_sweep[["stats"]],
         ##                      transform = map(cast_sweep,
         ##                                          .id = aff_sweep)),

         ## cast_stats = target(dplyr::bind_rows(cast_stats_split),
         ##                      transform = combine(cast_stats_split,
         ##                                          .id = aff_sweep)),

         ## max_h_ind = which.max(cast_stats$h),
         ## max_aff = cast_stats$aff[max_h_ind],

         ## ##plot the cast clustering
         ## save_copepod_aff_h = target(ggsave_wrapper(filename = file_out(!!pl_copepod_aff_h_file),
         ##                                     plot = ggplot(cast_stats[cast_stats$norm_z & cast_stats$type == "compact",], aes(x = aff, y = h, group = type, colour = as.factor(type))) +
         ##                                       geom_line()
         ##                                    ),
         ##                             hpc = FALSE),
         ## save_copepod_pmat_diag = target(ggsave_wrapper(filename = file_out(!!pl_copepod_p_mat_diag_file),
         ##                                         gg_sim_mat(sim_mat = p_mat_diag_cov,
         ##                                                    cast_ob = cast_sweep_list[[max_h_ind]]$cast_compact,
         ##                                                    highlight = TRUE,
         ##                                                    sort_between = TRUE,
         ##                                                    sort_within = TRUE)),
         ##                             hpc = FALSE),

         ## cluster_order = sort_between(sim_mat = p_mat_diag_cov,
         ##                              cast_ob = cast_sweep_list[[max_h_ind]]$cast_compact),

         ## cast_spatial = get_cast_spatial(cast_sweep_list[[max_h_ind]]$cast_compact[cluster_order], env_trans_spatial, spatial_vars),

         ## save_copepod_clust_map = target(
         ##   ggsave_wrapper(filename = file_out(!!pl_copepod_clust_map_file),
         ##                  plot =
         ##                    ggplot2::ggplot(
         ##                               data = cast_spatial,
         ##                               mapping =
         ##                                 ggplot2::aes_string(x = spatial_vars[1],
         ##                                              y = spatial_vars[2],
         ##                                              fill = as.factor(cast_spatial$clust))
         ##                             ) +
         ##                    ggplot2::geom_raster(),
         ## ),
         ##                             hpc = FALSE),


         ## ##TODO hotellings with point clouds
         ## ##create pair list
         ## ##map over pair list
         ## ##for each pair, extract the point cloud and calculate the hotellings T^2
         ## ##for each pair, return an ij dist and ji dist (symmetric)
         ## ##return the long form of the pairs
         ## ##add in the ii 1 casses, sort by i, then j,
         ## ##assign to a matrix by feeding the dist column into a matrix()
         ## pairs = all_pairs_diag(n=nrow(env_round)),

         ## dist_long = target(
         ##   pair_dist(pairs, env_trans_wide$points, env_names),
         ##   transform = split(pairs,
         ##                     slices = !!jobs
         ##                     )
         ## ),

         ## p_mat_full_cov_long = target(
         ##   dplyr::bind_rows(dist_long),
         ##   transform = combine(dist_long)
         ## ),

         ## p_mat_full_cov = matrix(p_mat_full_cov_long[
         ##   order(p_mat_full_cov_long$i, p_mat_full_cov_long$j), "dist" ],
         ##   nrow = nrow(env_round), ncol = nrow(env_round)),
         ## ##cluster, using CAST
         ## cast_sweep_full = target(cast_compact_stats(aff_thres = aff_sweep,
         ##                                        sim_mat = p_mat_full_cov),
         ##   transform = map(.id = aff_sweep,
         ##                   aff_sweep)
         ## ),

         ## cast_sweep_full_list = target(list(cast_sweep_full),
         ##                      transform = combine(cast_sweep_full,
         ##                                          .id = aff_sweep)),

         ## cast_stats_full_split = target(cast_sweep_full[["stats"]],
         ##                      transform = map(cast_sweep_full,
         ##                                          .id = aff_sweep)),

         ## cast_stats_full = target(dplyr::bind_rows(cast_stats_full_split),
         ##                      transform = combine(cast_stats_full_split,
         ##                                          .id = aff_sweep)),

         ## ##Cluster the combined copepods
         ## ##choos the peak Hubert Gama

         ## max_h_ind_full = which.max(cast_stats_full$h),
         ## max_aff_full = cast_stats_full$aff[max_h_ind_full],

         ## ##plot the cast clustering
         ## save_copepod_aff_h_full = target(ggsave_wrapper(filename = file_out(!!pl_copepod_aff_h_full_file),
         ##                                          plot = ggplot(cast_stats_full[cast_stats_full$norm_z & cast_stats_full$type == "compact",], aes(x = aff, y = h, group = type, colour = as.factor(type))) +
         ##                                            geom_line()
         ##                                                        ),
         ##                             hpc = FALSE),
         ## save_copepod_pmat_diag_full = target(ggsave_wrapper(filename = file_out(!!pl_copepod_p_mat_full_file),
         ##                                         gg_sim_mat(sim_mat = p_mat_full_cov,
         ##                                                    cast_ob = cast_sweep_full_list[[max_h_ind_full]]$cast_compact,
         ##                                                    highlight = TRUE,
         ##                                                    sort_between = TRUE,
         ##                                                    sort_within = TRUE)),
         ##                             hpc = FALSE),

         ## cluster_order_full = sort_between(sim_mat = p_mat_diag_cov,
         ##                              cast_ob = cast_sweep_list[[max_h_ind]]$cast_compact),

         ## cast_spatial_full = get_cast_spatial(cast_sweep_full_list[[max_h_ind_full]]$cast_compact[cluster_order_full], env_trans_spatial, spatial_vars),

         ## save_copepod_clust_full_map = target(
         ##   ggsave_wrapper(filename = file_out(!!pl_copepod_clust_map_full_file),
         ##                  plot =
         ##                    ggplot2::ggplot(
         ##                               data = cast_spatial_full,
         ##                               mapping =
         ##                                 ggplot2::aes_string(
         ##                                            x = spatial_vars[1],
         ##                                            y = spatial_vars[2],
         ##                                            fill = as.factor(cast_spatial_full$clust)
         ##                                            )
         ##                             ) +
         ##                    ggplot2::geom_raster(),
         ## ),
         ##                             hpc = FALSE),
         ## #Keep going, but get some outputs eventually
         ## save_copepod_gfboot_cumimp = target(ggsave_wrapper(filename =  file_out(!!pl_gfboot_cumimp_file),
         ##                                   plot = gg_combined_bootstrapGF(copepod_combined_gf,
         ##                                                                  n_curves = 30,
         ##                                                                  debug = FALSE)
         ##                               ),
         ##                             hpc = FALSE),

         ## plot_range = target(gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
         ##                              plot_type = "Predictor.Ranges",
         ##                              vars = 1:9,
         ##                              out_file = file_out(!!pl_gfboot_range_file)),
         ##                             hpc = FALSE),
         ## plot_density = target(gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
         ##                              plot_type = "Predictor.Density",
         ##                              vars = 1:9,
         ##                              out_file = file_out(!!pl_gfboot_density_file)),
         ##                             hpc = FALSE),
         ## plot_cumimp = target(gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
         ##                              plot_type = "Cumulative.Importance",
         ##                              vars = 1:9,
         ##                              out_file = file_out(!!pl_gfboot_cumimp_file)),
         ##                             hpc = FALSE),
         ## plot_perf = target(gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
         ##                              plot_type = "Performance",
         ##                              vars = 1:9,
         ##                              out_file = file_out(!!pl_gfboot_perf_file)),
         ##                             hpc = FALSE)
#
#
         #plotting a bit
         ext_pl = target(plot_extents(marine_map,
                               env_extent,
                               file_out(!!ext_pl_map_file)
                               ),
                                     hpc = FALSE),
#
         ## ext_pl_biooracle = target(plot_temp(env_final,
         ##                              spatial_vars,
         ##                              marine_map,
         ##                              env_extent,
         ##                              file_out(!!ext_pl_temp_file)
         ##                              ),
         ##                             hpc = FALSE),
         ext_pl_biooracle = target(plot_temp(env_spatial,
                                      spatial_vars,
                                      marine_map,
                                      env_extent,
                                      file_out(!!ext_pl_temp_file)
                                      ),
                                     hpc = FALSE),
         track_state = target(state_rds(file_out(!!state_rds_file),
                                 file_out(!!state_yaml_file)),
                                     hpc = FALSE),

         plot_range = target(gf_plot_wrapper(gf_model = copepod_combined_gf,
                                      plot_type = "Predictor.Ranges",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_range_file)),
                                     hpc = FALSE),
         plot_density = target(gf_plot_wrapper(gf_model = copepod_combined_gf,
                                      plot_type = "Predictor.Density",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_density_file)),
                                     hpc = FALSE),
         plot_cumimp = target(gf_plot_wrapper(gf_model = copepod_combined_gf,
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_cumimp_file)),
                                     hpc = FALSE),
         plot_perf = target(gf_plot_wrapper(gf_model = copepod_combined_gf,
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_perf_file)),
                                     hpc = FALSE)


         plot_


         )

##Set seed
r_seed <- 20200219

##Set cache
cache_dir <- here::here("drake_cache")
cache_ob <- drake::drake_cache(cache_dir)
if(is.null(cache_ob)){
  drake::new_cache(cache_dir)
  cache_ob <- drake::drake_cache(cache_dir)
}

drake::vis_drake_graph(plan = pl, cache = cache_ob, seed = r_seed,
  file = here::here("outputs", "drake_graph_pre.html"),
  selfcontained = TRUE,
  hover = TRUE
)
drake::sankey_drake_graph(plan = pl, cache = cache_ob, seed = r_seed,
                          file = here::here("outputs", "drake_graph_pre_sankey.html"),
  selfcontained = TRUE
)
ggsave(
  filename = here::here("outputs", "drake_ggplot_pre.png"),
  drake::drake_ggraph(plan = pl, cache = cache_ob, seed = r_seed)
)
#' Make
if (!interactive()) {
  if(system2("qstat") == 0 ){
    ##We are in a system where qstat is present and working
    options(
      clustermq.scheduler = "PBS",
      ## Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
      clustermq.template = here::here("code", "pbs_clustermq.tmpl")
    )
    parallelism <- "clustermq"
  } else {
    ##no access to qstat/qsub, run multicore/serial
    jobs <- future::availableCores(methods = c("PBS"), default = 1) - 1 ## number of cores, leave one for master
    parallelism <- "clustermq"
    options(
      clustermq.scheduler = "MULTICORE"
    )
    if (jobs <= 0) {
      ## not in a PBS job
      jobs <- future::availableCores(methods = c("mc.cores"))
    }
    if (jobs <= 1) {
      jobs <- 1
      parallelism <- "loop"
    }

  }
  print(parallelism)
print(getOption("clustermq.template", "PBS"))

  drake::make(pl, seed = r_seed,
              parallelism = parallelism,
              jobs = jobs, ## 6 jobs, for 6 surveys
              log_make = here::here("outputs", "drake_log.log"),
              template = list(log_file = here::here("outputs", "drake_worker_log.txt"),
                              memory = 16000,
                              cores = 4,
                              walltime = "20:00:00"),
              verbose = 4,
              cache = cache_ob,
              caching = "worker",
              garbage_collection = TRUE,
              prework = quote(future::plan(future.callr::callr, workers = future::availableCores(which = "max")))
              )

  drake::vis_drake_graph(plan = pl, cache = cache_ob, seed = r_seed,
                         file = here::here("outputs", "drake_graph.html"),
                       selfcontained = TRUE,
                       hover = TRUE)
drake::sankey_drake_graph(plan = pl, cache = cache_ob, seed = r_seed,
                          file = here::here("outputs", "drake_graph_sankey.html"),
                          selfcontained = TRUE)
ggsave(filename = here::here("outputs", "drake_ggplot.png"),
       drake::drake_ggraph(plan = pl, cache = cache_ob, seed = r_seed)
       )

}
