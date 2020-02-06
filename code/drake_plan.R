#' Libraries
##Drake
library(drake)
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
##Data
library(sdmpredictors)
##Plots
library(ggplot2)
library(ggthemes)
##Support
library(here)

#' Custom Functions
split_surv <- function(combined_copepod,
                       matching){
  return(dplyr::filter(combined_copepod, PROJECT_ID %in% matching))
}

remove_meso <- function(surv,
                        depth){
  dplyr::filter(surv, SAMPLE_DEPTH < depth | is.na(SAMPLE_DEPTH))
}

env_aus_eez <- function(bio_oracle_cache,
                        env_vars,
                        env_modes,
                        env_extent,
                        max_depth,
                        regrid_res,
                        spatial_vars,
                        bio_oracle_str_template = "BO2_%s%s_ss"
                        ){


  env_pairs <- data.table::as.data.table(merge.data.frame(env_vars, env_modes, all=TRUE))

  env_bio_oracle_names <- apply(env_pairs[x != "depth"], 1,  function(x){sprintf(bio_oracle_str_template, x[1], x[2])})
  ## Add bathymetry separately
  env_bio_oracle_names <- c(env_bio_oracle_names, "MS_bathy_5m")


  env_raster <- sdmpredictors::load_layers(env_bio_oracle_names, datadir = bio_oracle_cache, rasterstack = FALSE)

  aeez_target_grid <- raster::raster(x = raster::extent(env_extent), resolution = regrid_res, crs = "+proj=longlat +datum=WGS84")

  raster_crop <- raster::brick(lapply(env_raster, function(r){raster::crop(r, env_extent)}))
  raster_rescale <- raster::resample(raster_crop, aeez_target_grid, method = "bilinear")

  env <- raster::rasterToPoints(raster_rescale)
  env_complete <- as.data.frame(env[complete.cases(env),])
  return(env_complete)
}

env_log_transform <- function(env_data, env_log){
  ## Transformations by log10
  cv_min <- min(env_complete$BO2_curvelrange_ss)
  cv_min_offset <- cv_min + cv_min/10

  env_data$BO2_curvelrange_ss <- env_data$BO2_curvelrange_ss - cv_min_offset
  purrr::walk(env_log, ~{
    env_data[[.x]] <- log10(env_data[[.x]])
  })
  return(env_data)
}

env_clip_extremes <- function(env_data, env_limits){

  ## Clipping extremes, post logging
  purrr::walk(names(env_limits), ~{
    min_x <- min(env_limits[[.x]])
    max_x <- max(env_limits[[.x]])
    env_data[[.x]] <- pmax(min_x, pmin(env_data[[.x]], max_x))
  })
  return(env_data)
}
env_name_spatial <- function(env_data, spatial_vars){
  names(env_data)[1:2] <- spatial_vars
  return(env_data)
}

plot_extents <- function(marine_map, env_extent){
  ggplot(marine_map, aes(x = x, y = y)) +
    geom_sf(data = marine_map,inherit.aes = FALSE, color = "black", fill= NA) +
                                        #labs(fill = envVar) +
    coord_sf(xlim = env_extent$x, ylim = env_extent$y)+
    theme_tufte()

}

plot_temp <- function(env_data, spatial_vars, marine_map, env_extent){
  ggplot(env_data[,c(spatial_vars, "BO2_tempmean_ss")], aes(x = lon, y = lat, fill = BO2_tempmean_ss)) +
    geom_raster() +
    geom_sf(data = marine_map,inherit.aes = FALSE, color = "black", fill= NA) +
    labs(fill = "Mean ss Temp") +
    coord_sf(xlim = env_extent$x, ylim = env_extent$y)+
    theme_tufte()
}

## state<-rutilities::track_all_states()
## seed <- 20190703
## set.seed(seed)

## I am taking my code from here:
## [[file:/vmshare/phd/projects/aus_bioregions_paper/experiments/2019-06-28-1618_gf_models_kmeans/method_copepod.Rmd][file:/vmshare/phd/projects/aus_bioregions_paper/experiments/2019-06-28-1618_gf_models_kmeans/method_copepod.Rmd]]
#' Plan

## Set up variables


#This should really be a drake plan too!
## env_data <- archivist::areadLocal("eca58d95fc8d82de9750864ad2c82adf", "../2019-09-05-0908_clip_env/archivist")

pl <- drake::drake_plan(
               ##parameters
               epi_depth = 200,
               freq_range = c(0.05, 1),
               min_occurrence = 6,
               cov_min = 1.0,
               mapfile =  file_in(here::here("..", "..", "..", "Q1215", "ShapeFiles", "World_EEZ_v8")),
               mapLayer =  "World_EEZ_v8_2014_HR",
               biooracle_folder = here::here("..", "..", "..", "Q1215", "bioORACLE"),
               pred = list(),
               env_vars = c("depth", "temp", "nitrate", "silicate", "chlo", "iron",  "salinity", "curvel"),
               env_modes = c("min", "max", "mean", "range"),
               spatial_vars =  c("lat", "lon"),
               env_id_col = "env_id",
               bio_oracle_str_template = "BO2_%s%s_ss",
               env_res = 1/12,
               env_offset = 0,
               max_depth = 1500,
               regrid_resolution = 1/12, #in lat lon degrees, use 1/integer fraction for proper rastering later,
                                        #currently 1/12 to allign with BioORACLE
                                        #Chosen to match the largest extents of the Aus EEZ polygon and the FRDC benthic data
                                        #FRDC is not being used, but previous effort has used this extent and the full sampling of the GoC is useful
               env_extent = list(x = c( 109+1/24, 163+23/24), y = c(-47-23/24, -8-1/24 )),
               gf_trees = 500,
               gf_bins = 201,
               gf_corr_thres = 0.5,
                                        #The following variables work better in log scale.
                                        #If a variable is log transformed, clipping will take place on the log scale
               env_log = c( 
                 "BO2_nitratemin_ss" ,            
                 "BO2_silicatemin_ss",
                 "BO2_ironmin_ss" ,
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
                                        #For each predictor, I have specified limits. Not all variables hit the limits, shwon in comment
                                        #The limits are generally around 3 standard deviations from the mean, unless specified in comments
               env_limits = list(
                 "BO2_tempmin_ss" = c( 0, 30), #no clipping
                 "BO2_nitratemin_ss" = c( -16, 3), #Log, within limits
                 "BO2_silicatemin_ss" = c( -Inf, 2.5), #Log, upper limits in SO and in a few coastal sites, lower limits off east coast Tas, ~2std. Setting scale to avoid clipping lower limits 
                 "BO2_chlomin_ss" = c( 0, 0.25), #Clipping upper extremes, mostly off southern Tas coast, ~1.7std
                 "BO2_ironmin_ss" = c( -Inf , -5.75), #Log, clipping upper extremes, all coastal ~2.6std
                 "BO2_salinitymin_ss" = c( 32.15, 35.72), #Upper extreme in Bight, clipped ~1.4std, lower extremes on tropical coasts ~2std
                 "BO2_curvelmin_ss" = c( -3.32, Inf ), #Log, lower extreme in bight and other southern coasts, ~3std. No upper extremes
                 "BO2_tempmax_ss" = c( 8, 32), #no clipping
                 "BO2_nitratemax_ss" = c( -13, 4), #Log, no upper clipping due to lack of extremes, no lower clipping as almost all low values are deep water and don't look like outliers. Log transform brings out tropical patterns, without log, the SO gradient is prominent.
                 "BO2_silicatemax_ss" = c( 0, 3.16), #Log, no lower extremes, upper extremes are tropical coastal ~2std
                 "BO2_chlomax_ss" = c( 0, 1.12), # very long upper tail, mostly along south australian shelf, but not coastal ~2std
                 "BO2_ironmax_ss" = c( -11, -5.2), #Log, no lower extremes, some upper extremes around PNG ~2.5std
                 "BO2_salinitymax_ss" = c( 34.2, 36.6), #Upper and lower extremes along coasts ~2.5std
                 "BO2_curvelmax_ss" = c( -2.5, 0.5), #Log, no upper extremes, lower extremes are randomly placed ~2.5std
                 "BO2_tempmean_ss" = c( 6, 30), #No clipping
                 "BO2_nitratemean_ss" = c( -14, 3.1), #Log, no clipping, extremes are in deep waters in GoC but are part of the long tail
                 "BO2_silicatemean_ss" = c( 0.3, 2.8), #Log, no lower extremes just gradient into SO, upper extremes on tropical coasts
                 "BO2_chlomean_ss" = c( 0, 0.48), #Very long upper tail, some in tropical coasts, most along southern shelf edge and Tas ~2std
                 "BO2_ironmean_ss" = c( -12, -5.6), #Log, no lower extremes, upper extremes are coastal, mostly in GoC and Bight
                 "BO2_salinitymean_ss" = c( 33.7, 36.1), #extremes are all coastal
                 "BO2_curvelmean_ss" = c( -4.7, 0), #Log, lower extremes are randomly placed, no upper extremes, 
                 "BO2_temprange_ss" = c( 0, 9.5), #no lower extremes, upper extremes are coastal
                 "BO2_nitraterange_ss" = c( -14, 3), #Log, no clipping, extremes are deep water in GoC
                 "BO2_silicaterange_ss" = c( -1.2, 2.65),  #Log, no lower extremes, upper extremes are tropical coastal ~2std
                 "BO2_chlorange_ss" = c( 0, 0.96), #very long upper tail, around Indonesia, south shelf edge of Australia and Tas ~2std
                 "BO2_ironrange_ss" = c( -10.7, -6), #Log, lower extreme in SO, upper extreme in GoC ~2std
                 "BO2_salinityrange_ss" = c( 0, 2.9), #no lower extreme, upper extremes are all tropical coastal ~2std
                 "BO2_curvelrange_ss" = c( -4.6, 0.4), #Log, no upper extreme, lower extremes are random ~2std
                 "MS_bathy_5m" = c( -6000, 0) #lower extremes in deep trences. no upper extremes
               ),
               marine_map = sf::st_read(mapfile, layer = mapLayer),
               ausEEZ = marine_map[marine_map$Country == "Australia",],
               env_complete = env_aus_eez(bio_oracle_cache = biooracle_folder,
                                          env_vars = env_vars,
                                          env_modes = env_modes,
                                          env_extent = env_extent,
                                          max_depth = max_depth,
                                          regrid_res = regrid_resolution,
                                          bio_oracle_str_template = "BO2_%s%s_ss"),
               env_logged = env_log_transform(env_data = env_complete, env_log = env_log),
               env_clipped = env_clip_extremes(env_data = env_logged, env_limits = env_limits),
               env_final = env_name_spatial(env_data = env_clipped, spatial_vars = spatial_vars),
         ##here I have referred to a variable defined above, copepod_csv
         ##copepod_csv is just a string, which will be passed to read_csv.
         ##first, I wrap the string inside file_in, so that drake knows it is a filename,
         ##that I read from the file, and that the file should be tracked.
         ##files cannot be stored in variables, must be a string.
               combined_copepod = readr::read_csv(file_in(here::here("..", "..", "..", "Q1215", "AusCPR", "combined_copeped_jul19.csv")),
                                             na = c("(null)", "."),
                                             col_types = readr::cols(PROJECT_ID = col_character(),
                                                                     SAMPLE_DEPTH = col_number()),
                                             ),
         ##I had a lambda (unnamed) function here, but moved it to the
         ##custom funtion section
#
         ##drake_plan() forces you to put commas everywhere, this is not an R block.
#
         surv = target( split_surv(combined_copepod, matching),         ##supplying the initial splitting of the data here
                       ## How to use transform parameters
                       ## the map() function steps through "rows of a grid"
                       ## the nth target uses the nth entry of each object in map()
                       ## so the second target below uses names = "cpr" and matching = "CPR"
                       ## If you want all combinations, see cross().
                       ##
                       ## Notice that variables passed to split_surv() can come from all
                       ## different places in drake_plan().
                       ## combined_copepod is another target
                       ## matching is provided by the tranform = map(...) just below.
                       ## within map(), .id is provided by another parameter from map()
                       ## In the surv_epi target map(), .id is using the names object from
                       ## the surv target map().
                       ## So most map() params can be hard coded or targets, but .id must be
                       ##assigned another map() param
                       transform = map(
                         names = c("nrs", "cpr", "mkinnon", "goc", "nyan", "anita"),
                         .id = names,
                         matching = list(nrs = "NRS",
                                         cpr = "CPR",
                                         mckinnon = as.character(c(4, 5, 7, 9, 12, 15, 16, 24)), #McKinnon surveys
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
             .id = names
           )
         ),
#
         #Keep going, but get some outputs eventually
#
#
         #plotting a bit
         ext_pl = plot_extents(marine_map, env_extent),
         saved_ext_pl = ggsave(file_out(here::here("outputs", "extents.png")),
                               plot = ext_pl,
                               units = "cm",
                               width = 8,
                               height = 6,
                               dpi = "screen"
                               ),
#
         ext_pl_biooracle = plot_temp(env_final, spatial_vars, marine_map, env_extent),
         saved_ext_pl_biooracle = ggsave(file_out(here::here("outputs", "temps.png")),
                               plot = ext_pl_biooracle,
                               units = "cm",
                               width = 8,
                               height = 6,
                               dpi = "screen"
                               )
#
         )

#' Make
drake::make(pl)

drake::vis_drake_graph(drake_config(pl), file = "../outputs/drake_graph.html", selfcontained = TRUE, hover = TRUE)
drake::sankey_drake_graph(drake_config(pl), file = "../outputs/drake_graph_sankey.html", selfcontained = TRUE)
ggsave(filename = "../outputs/drake_ggplot.png", drake::drake_ggraph(drake_config(pl)))

