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
    dplyr::select(sp_names) %>%
      dplyr::summarise_all(
               function(x) {
                 sum(x != 0) / length(x)
               }
             ) %>%
        tidyr::gather(key = "species", value = "freq")

  surv_sp_occ <- surv_env %>%
    dplyr::select(sp_names) %>%
    dplyr::summarise_all(
             function(x) {
               sum(x != 0)
             }
           ) %>%
    tidyr::gather(key = "species", value = "occ")

  surv_sp_cov <- surv_env %>%
    dplyr::select(sp_names) %>%
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

env_clip_extremes <- function(env_data, env_limits) {

  ## Clipping extremes, post logging
  purrr::walk(names(env_limits), ~{
    min_x <- min(env_limits[[.x]])
    max_x <- max(env_limits[[.x]])
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

  env_spatial_x_row <- cbind(x_row = seq.int(1, nrow(env_data)), env_spatial)
  return(merge(env_predicted, env_spatial_x_row[, c("x_row", spatial_vars)]))
}

env_wide_list <- function(env_data){
  assertthat::assert_that("predict.combinedBootstrapGF" %in% class(env_data))
  type_names <- unique(env_data$type)
  env_wide <- lapply(type_names, function(ty, env_data) {
    tmp <- tidyr::pivot_wider(env_data[env_data$type == ty, c("var", "x_row", "y") ], names_from = "var", values_from = "y")
    tmp <- tmp[order(tmp$x_row), ]
    return(tmp)
  }, env_data = env_data)
  names(env_wide) <- type_names
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

pl_gfboot_cumimp_file <- here::here("outputs", "gfboot_cumimp.png")


pl <- drake::drake_plan(
               ##parameters
               epi_depth = 200,
               freq_range = c(0.05, 1),
               min_occurrence = 6,
               cov_min = 1.0,
               mapfile =  file_in(!!mapfile_location),
               mapLayer =  "World_EEZ_v8_2014_HR",
               biooracle_folder = here::here(
                                          "..", "..", "..",
                                          "Q1215", "bioORACLE"
                                        ),
               pred = list(),
               env_vars = c("depth",
                            "temp",
                            "nitrate",
                            "silicate",
                            "chlo",
                            "iron",
                            "salinity",
                            "curvel"),
               env_modes = c("min", "max", "mean", "range"),
               spatial_vars =  c("lon", "lat"),
               env_id_col = "env_id",
               bio_oracle_str_template = "BO2_%s%s_ss",
               env_offset = 0,
               max_depth = 1500,
               ##in lat lon degrees, use 1/integer fraction
               ##for proper rastering later,
               ##currently 1/12 to allign with BioORACLE
               regrid_resolution = 1 / 12,
               ##Extent chosen to match the largest extents of
               ##the Aus EEZ polygon and the FRDC benthic data
               ##FRDC is not being used, but previous effort
               ##has used this extent and the full sampling of the GoC is useful
               env_extent = list(x = c(109 + 1 / 24, 163 + 23 / 24),
                                 y = c(-47 - 23 / 24, -8 - 1 / 24)),
               gf_trees = 50,
               gf_bins = 201,
               gf_corr_thres = 0.5,
               extrap = TRUE,
               extrap_pow = 1/4,
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
               ),
               ##For each predictor, I have specified limits.
               ##Not all variables hit the limits, shwon in comment
               ##The limits are generally around 3 standard deviations
               ##from the mean, unless specified in comments
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
               ),
               marine_map = sf::st_read(mapfile, layer = mapLayer),
               ausEEZ = marine_map[marine_map$Country == "Australia", ],
               env_complete = env_aus_eez(bio_oracle_cache = biooracle_folder,
                                          env_vars = env_vars,
                                          env_modes = env_modes,
                                          env_extent = env_extent,
                                          max_depth = max_depth,
                                          regrid_res = regrid_resolution,
                                          bio_oracle_str_template =
                                            "BO2_%s%s_ss"),
               env_logged = env_log_transform(env_data = env_complete,
                                              env_log = env_log),
               env_clipped = env_clip_extremes(env_data = env_logged,
                                               env_limits = env_limits),
               env_final = env_name_spatial(env_data = env_clipped,
                                            spatial_vars = spatial_vars),
               env_round = env_round_label(env_data = env_final,
                                           spatial_vars = spatial_vars,
                                           env_res = regrid_resolution,
                                           env_offset = env_offset,
                                           env_id_col = env_id_col),
               ## here I have referred to a variable defined above,
               ##copepod_csv copepod_csv is just a string, which will
               ##be passed to read_csv. first, I wrap the string inside
               ##file_in, so that drake knows it is a filename, that
               ##I read from the file, and that the file should be tracked.
               ##files cannot be stored in variables, must be a string.
               combined_copepod =
                 readr::read_csv(
                          file_in(!!copepod_data),
                          na = c("(null)", "."),
                          col_types =
                            readr::cols(PROJECT_ID = col_character(),
                                        SAMPLE_DEPTH = col_number()),
                          ),
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
                           env_vars = base::names(env_round)
           ),
           transform = map(
             surv_env,
             surv_sp_keep,
             .id = surv_names
           )
         ),
         ##Fit GF models
         surv_gf = target(
           gfbootstrap::bootstrapGradientForest(
                             as.data.frame(surv_env_filter),
                             predictor.vars = base::names(env_round),
                             response.vars = surv_sp_keep,
                             nbootstrap = gf_trees,
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
           gfbootstrap::combinedBootstrapGF(surv_gf,
                                            nbin = gf_bins,
                                            n_samp = gf_trees),
           transform = combine(surv_gf)
         ),


         ##Transform the environment. No need for target(), I am not mapping or combining
         env_trans = predict.combinedGradientForest(object = copepod_combined_gf,
                                          newdata = env_round,
                                          type = c("mean", "variance", "points"),
                                          extrap = extrap,
                                          extrap_pow = extrap_pow),


         ##Hotellings p-value similiarity matrix, using diagonal covariance
         env_trans_wide = env_wide_list(env_trans),

         p_mat_diag_cov = rmethods:::hotellings_bulk(
                              means = env_trans_wide$mean[, env_vars],
                              res_sq = env_trans_wide$variance[, env_vars]
                            ),

         ##cluster, using CAST
         aff_sweep = seq(0.05, 0.95, 0.25),
         cast_sweep = target(cast_compact_stats(aff_thres = aff_sweep,
                                                sim_mat = p_mat_diag_cov),
           transform = map(.id = aff_sweep,
                           aff_sweep)
         ),

         ##add lat and lon to env_trans_wide
         ## env_trans_spatial = env_merge_spatial(env_trans, env_round, spatial_vars),


         ##TODO hotellings with point clouds


         ##Cluster the combined copepods
         
         

         #Keep going, but get some outputs eventually
#
#
         #plotting a bit
         ext_pl = plot_extents(marine_map,
                               env_extent,
                               file_out(!!ext_pl_map_file)
                               ),
#
         ext_pl_biooracle = plot_temp(env_final,
                                      spatial_vars,
                                      marine_map,
                                      env_extent,
                                      file_out(!!ext_pl_temp_file)
                                      ),
         track_state = state_rds(file_out(!!state_rds_file),
                                 file_out(!!state_yaml_file)),
         save_copepod_gfboot_cumimp = ggsave_wrapper(filename =  file_out(!!pl_gfboot_cumimp_file),
                                           plot = gg_combined_bootstrapGF(copepod_combined_gf,
                                                                          n_curves = 30,
                                                                          debug = FALSE)
                                       ),
         plot_range = gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
                                      plot_type = "Predictor.Ranges",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_range_file)),
         plot_density = gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
                                      plot_type = "Predictor.Density",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_density_file)),
         plot_cumimp = gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_cumimp_file)),
         plot_perf = gf_plot_wrapper(gf_model = copepod_combined_gf$gf_list[[1]],
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_gf_perf_file))
         )


r_seed <- 20200219
#' Make
if (!interactive()) {
  
  options(
    clustermq.scheduler = "PBS",
                                        # Created by drake_hpc_template_file("pbs_clustermq.tmpl") and modified:
    clustermq.template = here::here("code", "pbs_clustermq.tmpl")
)
   drake::make(pl, seed = r_seed,
               parallelism = "clustermq",
               jobs = 6, ## 6 jobs, for 6 surveys
               console_log_file = here::here("outputs", "drake_log.log")
               )
}

drake::vis_drake_graph(drake_config(pl, seed = r_seed),
                       file = here::here("outputs", "drake_graph.html"),
                       selfcontained = TRUE,
                       hover = TRUE)
drake::sankey_drake_graph(drake_config(pl, seed = r_seed),
                          file = here::here("outputs", "drake_graph_sankey.html"),
                          selfcontained = TRUE)
ggsave(filename = here::here("outputs", "drake_ggplot.png"),
       drake::drake_ggraph(drake_config(pl, seed = r_seed)))
