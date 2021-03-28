#' Libraries
## Tidyverse
library(purrr)
##Parallel
library(future)
library(future.callr)
library(furrr)
##Analysis
library(cluster)
library(clustsig)
library(readr)
library(tidyverse)
library(sf)
library(raster)
library(gradientForest)
library(gfbootstrap)
library(castcluster)
library(data.table)
library(lutz)
library(glue)
library(lubridate)
library(rfishbase)
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
split_surv <- function(surv_all,
                       matching,
                       spatial_vars) {
    dplyr::filter(surv_all, ProjectNumber %in% matching) %>%
           dplyr::mutate(TaxonName = clean_sp_names(TaxonName))
}

remove_meso <- function(surv,
                        depth) {
  dplyr::filter(surv, SAMPLE_DEPTH < depth | is.na(SAMPLE_DEPTH))
}

clean_sp_names <- function(x) {
length_pre <- length(unique(x))
x %>%
    stringr::str_replace_all("\\(.*\\)", "") %>%
    stringr::str_replace_all(" ", "_") %>%
    stringr::str_replace_all("-", "_") %>%
    stringr::str_replace_all("/", "") %>%
    stringr::str_replace_all("__", "_") -> y
length_post <- length(unique(y))
assertthat::assert_that(length_pre == length_post)
return(y)
}

get_uncertain_sp_names <- function(surv, uncertain_val, abund_col) {
  surv %>%
    filter(across({{abund_col}}) == uncertain_val) %>%
    dplyr::select(TaxonName) %>%
    dplyr::distinct() %>%
    dplyr::pull(TaxonName)
 }
surv_to_wide <- function(surv, abund_col) {
  surv %>%
    dplyr::group_by(across(-{{abund_col}})) %>%
    dplyr::summarise(across({{abund_col}}, sum)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = TaxonName, value = {{abund_col}}, fill = 0) %>%
    ##May need a column rename here
    dplyr::mutate(No_taxa = NULL,
                  TaxonGroup = NULL,
                  ProjectNumber = NULL
                  )
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
    rphildyerphd::align_sp(spatial_cols = spatial_vars,
                           res = env_res, grid_offset = env_offset,
                           fun = mean) %>%
    merge(env_round, by = spatial_vars) -> surv_env
  return(surv_env)
}

foc_cov_filter_microbe <- function(microbe_samples,
                                   n_grids,
                                   max_otu,
                           freq_range,
                           cov_min,
                           min_occurrence) {
  assertthat::assert_that(all(class(microbe_samples) == c("data.table", "data.frame")))

  microbe_freq_cov <- microbe_samples[ , .(
    occ = .N,
    freq = .N/n_grids,
    cov = sd(OTU.Count) / mean(OTU.Count)),
    by = OTU]


  keep <- microbe_freq_cov[occ >= min_occurrence &
                           cov >= cov_min &
                           freq >= min(freq_range) &
                           freq <= max(freq_range),]

  keep_capped <- head(keep[order(-cov)], n = max_otu)   

  otu_short <- data.table(OTU = keep_capped$OTU,
                          OTU_short = paste0("R16s.", seq.int(1, nrow(keep_capped))),
                          key = "OTU"
                          )

  ret <- merge(microbe_samples, otu_short, by = "OTU")

  return(ret)

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
env_restrict_to_region <- function(bio_oracle_cache,
                        env_vars,
                        env_modes,
                        env_poly,
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

  target_grid <- raster::raster(x = env_poly,
                                     resolution = regrid_res,
                                     crs = "+proj=longlat +datum=WGS84")

  raster_crop <- raster::brick(lapply(env_raster,
                                      function(r) {
                                        raster::crop(r, raster::extent(target_grid))
                                      }))
  raster_rescale <- raster::resample(raster_crop,
                                     target_grid,
                                     method = "bilinear")
  ##mask is much quicker than st_intersection
  raster_masked <- raster::mask(raster_rescale, env_poly)
  crs(raster_masked) <-"+proj=longlat +datum=WGS84"
  env_points <- tibble::as_tibble(rasterToPoints(raster_masked))

  names(env_points)[1:2] <- spatial_vars

  env_region <- env_points[complete.cases(env_points), ]
  return(env_region)
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
                         env_poly,
                         out_file,
                         units = "cm",
                         width = 16,
                         height = 9,
                         dpi = 300
                         ) {

  env_bbox <-  sf::st_bbox(env_poly)
  ext_pl <- ggplot(marine_map, aes(x = x, y = y)) +
    geom_sf(data = marine_map,
            inherit.aes = FALSE,
            color = "black",
            fill = NA) +
   ggplot2::coord_sf(xlim = c(env_bbox$xmin, env_bbox$xmax), ylim = c(env_bbox$ymin, env_bbox$ymax))+
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
                      env_poly,
                      out_file,
                      units = "cm",
                      width = 16,
                      height = 9,
                      dpi = 300
                      ) {

  env_bbox <-  sf::st_bbox(env_poly)
  ext_pl_biooracle <- ggplot(env_data[, c(spatial_vars, "BO2_tempmean_ss")],
         aes(x = lon, y = lat, fill = BO2_tempmean_ss)) +
    geom_raster() +
    geom_sf(data = marine_map, inherit.aes = FALSE,
            color = "black", fill = NA) +
    labs(fill = "Mean ss Temp") +
   ggplot2::coord_sf(xlim = c(env_bbox$xmin, env_bbox$xmax), ylim = c(env_bbox$ymin, env_bbox$ymax))+
    theme_tufte()

  ggsave(out_file,
    plot = ext_pl_biooracle,
    units = units,
    width = width, #widescreen
    height = height,
    dpi = dpi
  )

}

plot_clust <- function(sites, clustering, spatial_vars, marine_map, env_poly, samples = NULL, grids = NULL, clip_samples = TRUE){

  if (clip_samples | is.null(samples)){
    env_bbox <-  sf::st_bbox(env_poly)
  } else {
    env_bbox <- sf::st_bbox(c(xmin = min(samples[, spatial_vars[1]]),
                                 xmax = max(samples[, spatial_vars[1]]),
                                 ymin = min(samples[, spatial_vars[2]]),
                                 ymax = max(samples[, spatial_vars[2]])
                                 )
                            )
  }

  pl <- ggplot2::ggplot(data= data.frame(sites, clust = as.factor(clustering)),
                            ggplot2::aes_string(x = spatial_vars[1], y = spatial_vars[2], fill = "clust")) +
  ggplot2::geom_raster()  +
  ggplot2::scale_fill_manual(values = rainbow(max(clustering))) +
  ggplot2::labs(fill = "cluster") +
  ggplot2::geom_sf(data = marine_map, inherit.aes = FALSE, color = "black", fill= NA)+
   ggplot2::coord_sf(xlim = c(env_bbox$xmin, env_bbox$xmax), ylim = c(env_bbox$ymin, env_bbox$ymax))+

  ggthemes::theme_tufte()

  if(!is.null(samples)){
    if(class(samples)[1] == "list" & length(samples) == 1){
      samples <- samples[[1]]
     }
    if(clip_samples){
      keep <- samples[, spatial_vars[1]]  >= env_bbox$xmin &
        samples[, spatial_vars[1]]  <= env_bbox$xmax &
        samples[, spatial_vars[2]]  >= env_bbox$ymin &
        samples[, spatial_vars[2]]  <= env_bbox$ymax
      samples <- samples[keep,]
    }
    pl <- pl +
  ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), shape = ".", colour = "dimgray", data = samples[,spatial_vars], inherit.aes = FALSE)
  }
  if(!is.null(grids)){
    if(class(grids)[1] == "list" & length(grids) == 1){
      grids <- grids[[1]]
     }
    pl <- pl +
  ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), data = grids[,spatial_vars], shape = "o", colour = "dimgray", inherit.aes = FALSE)
  }

  return(pl)

}

plot_clust_poly <- function(sites, clustering, spatial_vars, marine_map, env_poly, regrid_res, labels = TRUE, samples = NULL, grids = NULL, clip_samples = TRUE){

  if (clip_samples | is.null(samples)){
    env_bbox <-  sf::st_bbox(env_poly)
  } else {
    env_bbox <- sf::st_bbox(c(xmin = min(samples[, spatial_vars[1]]),
                                 xmax = max(samples[, spatial_vars[1]]),
                                 ymin = min(samples[, spatial_vars[2]]),
                                 ymax = max(samples[, spatial_vars[2]])
                                 )
                            )
  }

  target_grid <- raster::raster(x = env_poly,
                                     resolution = regrid_res,
                                     crs = "+proj=longlat +datum=WGS84")

  clust_raster <- raster::rasterize(
            x = sites[, spatial_vars],
            y = target_grid,
            field = as.numeric(clustering)
            )
  names(clust_raster) <- c("clustering")
  clust_multipoly <- sf::st_as_sf(raster::rasterToPolygons(clust_raster, dissolve = TRUE),
                         crs = "+proj=longlat +datum=WGS84")

  sf::st_crs(clust_multipoly) <- "+proj=longlat +datum=WGS84"
  pl <- ggplot2::ggplot(clust_multipoly, mapping = ggplot2::aes(fill = as.factor(clustering))) +
    ggplot2::geom_sf() +
  ggplot2::scale_fill_manual(values = rainbow(max(clustering))) +
  ggplot2::labs(fill = "cluster") +
  ggplot2::geom_sf(data = marine_map, inherit.aes = FALSE, color = "black", fill= NA) +
   ggplot2::coord_sf(xlim = c(env_bbox$xmin, env_bbox$xmax), ylim = c(env_bbox$ymin, env_bbox$ymax)) +
  ggthemes::theme_tufte()

  if(labels) {
    pl <- pl +
      ggplot2::geom_sf_label(ggplot2::aes(label= clustering), fill = "white")
  }
  if(!is.null(samples)){
    if(class(samples)[1] == "list" & length(samples) == 1){
      samples <- samples[[1]]
     }
    if(clip_samples){
      keep <- samples[, spatial_vars[1]]  >= env_bbox$xmin &
        samples[, spatial_vars[1]]  <= env_bbox$xmax &
        samples[, spatial_vars[2]]  >= env_bbox$ymin &
        samples[, spatial_vars[2]]  <= env_bbox$ymax
      samples <- samples[keep,]
    }
    pl <- pl +
  ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), shape = ".", colour = "dimgray", data = samples[,spatial_vars], inherit.aes = FALSE)
  }
  if(!is.null(grids)){
    if(class(grids)[1] == "list" & length(grids) == 1){
      grids <- grids[[1]]
     }
    pl <- pl +
  ggplot2::geom_point(mapping = ggplot2::aes(x = lon, y = lat), data = grids[,spatial_vars], shape = "o", colour = "dimgray", inherit.aes = FALSE)
  }

  return(pl)

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

simprof_plot_wrapper <- function(simprof_model,
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
  simprof.plot(simprof_model)
  dev.off()
}

state_rds <- function(rds_path, yaml_path) {
  total_state <- rutilities::track_all_states()
  saveRDS(file = rds_path, object = total_state)
  cat(yaml::as.yaml(total_state, line.sep = "\n", column.major = FALSE), file = yaml_path)
}

cluster_capture <- function(dname, data, k, nrep, ...) {
#' fit k-medoids to dataset, and return relevant 1% rule stats

  n_sites <- base::nrow(data)
  clust <- cluster::clara(x = data, k = k, ...)
  min_clust <- min(clust$clusinfo[, "size"])
  min_clust_ratio <- min_clust / n_sites


  ret <- tibble_row(dataname = dname, k = k, nrep = nrep, min_clust = min_clust, min_clust_ratio = min_clust_ratio, sil_avg = clust$silinfo$avg.width, clust = list(clust))
  return(ret)
}
cluster_capture_simprof <- function(dname, data, env_names, subset_rounding) {

  ##Rotate to help make clusters more compact

  if(!is.null(subset_rounding)){
    env_smaller <- data$lon %% subset_rounding == 0 & data$lat %% subset_rounding == 0
    data_pca <- prcomp(data[env_smaller, env_names])
  } else {
    data_pca <- prcomp(data[, env_names])
  }


  clust_simprof <- stats::setNames(list(
                            clustsig::simprof(data_pca$x,
                                              method.cluster = "complete",
                                              alpha = 0.05,
                                              )
                          ),
                          nm = c(dname)
                          )

  }


string_strip_counter <- function(x) {
  assertthat::is.string(x)
  spl <- sub("_\\d*$", "", x)
  return(spl)
}

string_get_tail <- function(x, split = "_") {

  assertthat::is.string(x)
  spl <- strsplit(x, split)[[1]]
  return(tail(spl, 1))

}
name_list <- function(li, na){
  x <- li
  names(x) <- na
  return(x)
}


##Microbes come in a 10GB long form CSV file, with a second CSV file holding the site data.
##From memory, the coversion from long to wide broke tidyverse.
##I used sqlite to get frequencies of occurrence, and only pulled rows with common otu
##then tidyr::spread actually worked
##The ultimate goal is to get a site by OTU
## Running some tests now.

samples_in_region <- function(
                              sites,
                              env_poly,
                              spatial_vars,
                              sites_crs = sf::st_crs(env_poly)
                                   ){

  x <- sf::st_as_sf(sites, coords = spatial_vars, remove = FALSE, crs = sites_crs)
  y <- sf::st_intersects(x, env_poly, sparse = FALSE)
  y <- apply(y, 1, any)

  return(y)
}
microbe_load_hack <- function(...) {
  dt_time <- microbenchmark::microbenchmark({
    ## micro_dt <- data.table::fread(microbe_bacteria_csv, sep = ",", select =1:3, check.names = TRUE, key = c("Sample.ID", "OTU" ), data.table = TRUE, stringsAsFactors = FALSE)
    micro_dt <- data.table::fread(...)
  }, times= 1
  )
  return(micro_dt)
}
test_microbe_extract <- function(){
stop("Not for actual use")

library(microbenchmark)
library(pryr)
library(data.table)
microbe_bacteria_csv <- here::here(
                        "..", "..", "..",
                        "Q1215", "aus_microbiome", "marine_bacteria", "Bacteria.csv")
microbe_bacteria_data <- here::here(
                        "..", "..", "..",
                        "Q1215", "aus_microbiome", "marine_bacteria", "bacteria.db")
microbe_bacteria_fst <- here::here(
                        "..", "..", "..",
                        "Q1215", "aus_microbiome", "marine_bacteria", "bacteria.fst")
microbe_bacteria_context <- here::here(
                        "..", "..", "..",
                        "Q1215", "aus_microbiome", "marine_bacteria", "contextual.csv")
##data needed:
## Bacteria.csv - Sample ID, OTU, OTU Count
## contextual.csv - Sample ID (for joining), Latitude, Longitude
## May also want date of sample, depth, microbial abundance

##There are a few approaches
## 1. careful use of data.table::fread to only get the subset of data I need, then hope data.table gets me there
## 2. Convert to SQL database, and use SQL to filter, then pull in and spread with tidyverse/data.table
## 3. Figure out disk.frame, which works on chunks of the data and stores the rest on disk. Author of disk.frame suggests SQL is slower

dt_time <- microbenchmark::microbenchmark({
    ## micro_dt <- data.table::fread(microbe_bacteria_csv, sep = ",", select =1:3, check.names = TRUE, key = c("Sample.ID", "OTU" ), data.table = TRUE, stringsAsFactors = FALSE)
    micro_dt <- data.table::fread(microbe_bacteria_csv, sep = ",", select =1:3, check.names = TRUE, key = c("Sample.ID"), data.table = TRUE, stringsAsFactors = FALSE)
  }, times= 1
) 
#only 300 seconds
dt_size <- pryr::object_size(micro_dt)
#only 500mb, stringsAsFactors may have helped
#Never mind, just use data.table then.

sites <- data.table::fread(microbe_bacteria_context, , sep = ",", select =c(
                                                           "Sample ID",
                                                           "Depth [m]",
                                                           "Microbial Abundance [cells per ml]",
                                                           "Date Sampled",
                                                           "Latitude [decimal degrees]",
                                                           "Longitude [decimal degrees]"),
                                                           check.names = TRUE,
                                                           key = c("Sample.ID"),
                                                           data.table = TRUE, stringsAsFactors = FALSE)

dt_cont_time <- microbenchmark::microbenchmark({
micro_sites <- data.table::fread(microbe_bacteria_context, , sep = ",", select =c(
                                                           "Sample ID",
                                                           "Depth [m]",
                                                           "Microbial Abundance [cells per ml]",
                                                           "Date Sampled",
                                                           "Latitude [decimal degrees]",
                                                           "Longitude [decimal degrees]"),
                                                          check.names = TRUE, key = c("Sample.ID"), data.table = TRUE, stringsAsFactors = FALSE)
  }, times= 1
) 
#only 300 seconds
dt_cont_size <- pryr::object_size(micro_sites)




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
plankton_data_root <- here::here(
                        "..", "..", "..",
                        "Q1215", "AusCPR"
                      )

source(glue::glue("{plankton_data_root}/PhytoDataPhilFeb21.R"))


source(glue::glue("{plankton_data_root}/zooDataPhilMar21.R"))



microbe_bacteria_csv <- here::here(
                        "..", "..", "..",
                        "Q1215", "aus_microbiome", "marine_bacteria", "Bacteria.csv")
microbe_bacteria_context <- here::here(
                        "..", "..", "..",
                        "Q1215", "aus_microbiome", "marine_bacteria", "contextual.csv")

fish_taxon_file  <- here::here(
                            "..", "..", "..",
                            "Q1215", "Watson_Fisheries_Catch_Data", "Version5", "Output", "TaxonomicData.rds"
                          )

fish_data_folder <- here::here(
                            "..", "..", "..",
                            "Q1215", "Watson_Fisheries_Catch_Data", "Version5", "Output", "Annual_TotalCatchSpecies"
                          )
##Using a closure here, fish_data_folder becomes a constant within the function
fish_file_fn <- function(yr){paste0(fish_data_folder, "/Watson_", yr, "_TotalCatch_bySpecies.csv")}


pl_microbe_freq_file <-  here::here("outputs", "microbe_freq_range.png")
pl_microbe_freq_cov_file <-  here::here("outputs", "microbe_freq_cov.png")
pl_microbe_filtered_freq_file <-  here::here("outputs", "microbe_filtered_freq_range.png")


ext_pl_temp_file <- here::here("outputs", "temps.png")
ext_pl_map_file <- here::here("outputs", "extents.png")
state_rds_file <- here::here("outputs", "state.rds")
state_yaml_file <- here::here("outputs", "state.yaml")

pl_zooplank_gf_range_file <- here::here("outputs", "zooplank_gf_range.png")
pl_zooplank_gf_density_file <- here::here("outputs", "zooplank_gf_density.png")
pl_zooplank_gf_cumimp_file <- here::here("outputs", "zooplank_gf_cumimp.png")
pl_zooplank_gf_perf_file <- here::here("outputs", "zooplank_gf_perf.png")

pl_phytoplank_gf_range_file <- here::here("outputs", "phytoplank_gf_range.png")
pl_phytoplank_gf_density_file <- here::here("outputs", "phytoplank_gf_density.png")
pl_phytoplank_gf_cumimp_file <- here::here("outputs", "phytoplank_gf_cumimp.png")
pl_phytoplank_gf_perf_file <- here::here("outputs", "phytoplank_gf_perf.png")

pl_alltroph_gf_range_file <- here::here("outputs", "alltroph_gf_range.png")
pl_alltroph_gf_density_file <- here::here("outputs", "alltroph_gf_density.png")
pl_alltroph_gf_cumimp_file <- here::here("outputs", "alltroph_gf_cumimp.png")
pl_alltroph_gf_perf_file <- here::here("outputs", "alltroph_gf_perf.png")

pl_zooplank_kmed_perf <- here::here("outputs", "zooplank_kmed_perf.png")
pl_zooplank_kmed_perf_sil <- here::here("outputs", "zooplank_kmed_perf_sil.png")
pl_phytoplank_kmed_perf <- here::here("outputs", "phytoplank_kmed_perf.png")
pl_phytoplank_kmed_perf_sil <- here::here("outputs", "phytoplank_kmed_perf_sil.png")

pl_alltroph_kmed_perf <- here::here("outputs", "alltroph_kmed_perf.png")
pl_alltroph_kmed_perf_sil <- here::here("outputs", "alltroph_kmed_perf_sil.png")

pl_test_cluster_rand_file <- here::here("outputs", "test_cluster_rand.png")

pl_gfboot_range_file <- here::here("outputs", "gfboot_range.png")
pl_gfboot_density_file <- here::here("outputs", "gfboot_density.png")
pl_gfboot_cumimp_file <- here::here("outputs", "gfboot_cumimp.png")
pl_gfboot_perf_file <- here::here("outputs", "gfboot_perf.png")

pl_gfboot_cumimp_file <- here::here("outputs", "gfboot_cumimp.png")

pl_zooplank_aff_h_file <- here::here("outputs", "zooplank_aff_h.png")
pl_zooplank_clust_map_file <- here::here("outputs", "zooplank_clust_map.png")
pl_zooplank_p_mat_diag_file <- here::here("outputs", "zooplank_p_mat_diag.png")

pl_zooplank_aff_h_full_file <- here::here("outputs", "zooplank_aff_h_full.png")
pl_zooplank_clust_map_full_file <- here::here("outputs", "zooplank_clust_map_full.png")
pl_zooplank_p_mat_full_file <- here::here("outputs", "zooplank_p_mat_full.png")

pl_zooplank_clust_map_all_file <- tibble( file = c(
                                           here::here("outputs", "zooplank_clust_map_anita.png"),
                                           here::here("outputs", "zooplank_clust_map_combined.png"),
                                           here::here("outputs", "zooplank_clust_map_cpr.png"),
                                           here::here("outputs", "zooplank_clust_map_goc.png"),
                                           here::here("outputs", "zooplank_clust_map_mckinnon.png"),
                                           here::here("outputs", "zooplank_clust_map_nrs.png")
                                           #here::here("outputs", "zooplank_clust_map_nyan.png")
                                         )
                                        )
pl_fish_gf_range_file <- here::here("outputs", "fish_gf_range.png")
pl_fish_gf_density_file <- here::here("outputs", "fish_gf_density.png")
pl_fish_gf_cumimp_file <- here::here("outputs", "fish_gf_cumimp.png")
pl_fish_gf_perf_file <- here::here("outputs", "fish_gf_perf.png")

pl_fish_kmed_perf <- here::here("outputs", "fish_kmed_perf.png")
pl_fish_kmed_perf_sil <- here::here("outputs", "fish_kmed_perf_sil.png")

jobs <- 5

## parameters

phytoplank_names = c(
  "nrs",
                                   "cpr",
                                   "brett",
                                    ## "other_1050",
                                    ## "other_1051",
                                    ## "other_1054",
                                    ## "other_1056",
                                    ## "other_1057",
                                    ## "other_1058",
                                    ## "other_1059",
                                    ## "other_1066",
                                    ## "other_479",
                                    ## "other_1068",
                                    ## "other_1067",
                                    ## "other_1069",
                                    ## "other_54",
                                    ## "other_591",
                                    ## "other_782",
                                    ## "other_786",
                                    ## "other_790",
                                    ## "other_796",
                                    ## "other_795",
                                    "other_806",
                                    ## "other_801",
                                    ## "other_805",
                                    ## "other_804",
                                    "other_807"
                                    ## "other_517",
                                    ## "other_519"
                                   )
                         phytoplank_matching = list(nrs = 599,
                                         cpr = 597,
                                         brett = 794,
                                        ## other_1050 = 1050,
                                        ## other_1051 = 1051,
                                        ## other_1054 = 1054,
                                        ## other_1056 = 1056,
                                        ## other_1057 = 1057,
                                        ## other_1058 = 1058,
                                        ## other_1059 = 1059,
                                        ## other_1066 = 1066,
                                        ## other_479 = 479,
                                        ## other_1068 = 1068,
                                        ## other_1067 = 1067,
                                        ## other_1069 = 1069,
                                        ## other_54 = 54,
                                        ## other_591 = 591,
                                        ## other_782 = 782,
                                        ## other_786 = 786,
                                        ## other_790 = 790,
                                        ## other_796 = 796,
                                        ## other_795 = 795,
                                        other_806 = 806,
                                        ## other_801 = 801,
                                        ## other_805 = 805,
                                        ## other_804 = 804,
                                        other_807 = 807
                                        ## other_517 = 517,
                                        ## other_519 = 519
                                         )
                         zooplank_names = c("nrs",
                                   "cpr",
                                   "mckinnon",
                                   "goc",
                                   ## "nyan",
                                   "anita")
                         zooplank_matching = list(nrs = 599,
                                         cpr = 597,
                                         mckinnon =
                                           c(4, 5, 7, 9, 12, 15, 16, 24), #McKinnon surveys
                                         goc = 1, #Gulf of Capentaria
                                         ## nyan = 21, #SE Tasmania
                                         anita = 18) #Tasmania data
depth_range = list(epi = c(0, 200),
                   meso = c(201, 1000 ),
                   bathy = c(1001, Inf))
depth_names <- c("epi",
                 "meso"
                # "bathy"
                 )
               freq_range = c(0.05, 1)
               min_occurrence = 6
               cov_min = 1.0
               max_otu = 2000

fish_years <- 2007:2017

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
               regrid_resolution = 1 / 2#TODO: 1 / 12,
               ##Extent chosen to match the largest extents of
               ##the Aus EEZ polygon and the FRDC benthic data
               ##FRDC is not being used, but previous effort
               ##has used this extent and the full sampling of the GoC is useful
               env_bounds = list(x = c(109 + 1 / 24, 163 + 23 / 24),
                                 y = c(-47 - 23 / 24, -8 - 1 / 24))
               gf_trees = 20
               gf_bins = 201
               gf_corr_thres = 0.5
               gf_compact = FALSE
               extrap = 1 / 4

               k_range = seq.int(2,20)
               cluster_reps = seq.int(1,3)
               cluster_reps_test = seq.int(1,10)

               clara_samples = 20
               clara_sampsize = 50
               clara_trace = 0
               clara_rngR = TRUE
               clara_pamLike = TRUE
               clara_correct.d = TRUE

               min_clust_thres = 0.01

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
                                   format = "qs",
                                   hpc = FALSE), #Workers can't see the same TMPDIR
               ## env_poly is a polygon that defines the study area.
               ## env_bbox is the bounding box of env_extent
               ## ausEEZ = marine_map[marine_map$Country == "Australia", ],
               ## env_poly = marine_map[marine_map$Country == "Australia", ],
               env_poly = sf::st_as_sf(as(raster::extent(env_bounds), "SpatialPolygons"),
                                       crs = sf::st_crs(marine_map)
                                       ),
               env_extent = raster::extent(env_poly),

               env_region = target(env_restrict_to_region(bio_oracle_cache = file_in(!!biooracle_folder),
                                          env_vars = env_vars,
                                          env_modes = env_modes,
                                          env_poly = env_poly,
                                          max_depth = max_depth,
                                          regrid_res = regrid_resolution,
                                          spatial_vars = spatial_vars,
                                          bio_oracle_str_template = bio_oracle_str_template
                                            ),
                                   format = "fst_tbl",
                                   hpc = FALSE), #Workers can't see the same TMPDIR
               ## env_logged = env_log_transform(env_data = env_region,
               ##                                env_log = env_log),
               env_clipped = env_clip_extremes(env_data = env_region,
                                               std_thres = env_limits_sd),
               env_round = target(env_round_label(env_data = env_clipped,
                                           spatial_vars = spatial_vars,
                                           env_res = regrid_resolution,
                                           env_offset = env_offset,
                                           env_id_col = env_id_col),
                                  format = "fst"
                                  ),
               env_names = get_env_names(env_round = env_round,
                                         spatial_vars = spatial_vars,
                                         env_id_col = env_id_col),
         #plotting a bit
         ext_pl = target(plot_extents(marine_map,
                               env_poly,
                               file_out(!!ext_pl_map_file)
                               ),
                                     hpc = FALSE),
#
         ext_pl_biooracle = target(plot_temp(env_clipped,
                                      spatial_vars,
                                      marine_map,
                                      env_extent,
                                      file_out(!!ext_pl_temp_file)
                                      ),
                                     hpc = FALSE),
               ## here I have referred to a variable defined above,
               ##zooplank_csv zooplank_csv is just a string, which will
               ##be passed to read_csv. first, I wrap the string inside
               ##file_in, so that drake knows it is a filename, that
               ##I read from the file, and that the file should be tracked.
               ##files cannot be stored in variables, must be a string.
         zooplank_all = target(
           load_zoo_data(plankton_data_root),
           hpc = FALSE,#Workers can't see the same TMPDIR
           format = "fst_tbl"
         ),
         zooplank_all_rename = target(
           dplyr::rename(zooplank_all, "{spatial_vars[1]}" := Longitude, "{spatial_vars[2]}" := Latitude, TaxonName = Species, Abund_m3 = ZAbund_m3),
           format = "fst_tbl"
         ),
               ##I had a lambda (unnamed) function here, but moved it to the
               ##custom funtion section
#
               ##drake_plan() forces you to put commas everywhere,
               ##this is not an R block.
#
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
                             ##parameter from map(). In the zooplank_epi target
                             ##map(), .id is using the names object from
                             ##the surv target map(). So most map() params
                             ##can be hard coded or targets, but .id must
                             ##be assigned another map() param
                             ##UPDATE: static mapping is the original branching in drake
                             ##but more recent versions added dynamic mapping.
                             ##Dynamic mapping works more like purrr, operating over lists or rows
                             ##
                             ##NOTES:
               ##dynamic elements must be variables in the environment OR existing targets
               ##For variables, vec_slice is applied:
               ## one row per subtarget for data.frame and friends, unless .by is specified, then one group per subtarget.
               ## one element per subtarget for list and friends
               ##the returned target will have the same type as the return value of the function,
               ##combined by whatever action vec_c() thinks is best. No need for recombining data.frame and friends
               ## rbind for data.frame and friends,
               ## c() for lists and vectors
               ## future dynamic calls over dynamic targets will get the subtargets
               ## .trace is useful metadata for debugging, and shows the input value
               ## of each subtarget without polluting the actual workflow. Use c() to select which inputs to store
               ##
               ##
               ## `format =` is not part of dynamic, but is very useful anyway. You can force drake to use fast
               ## storage backends like fst and qs. However, choosing fst for anything that isn't a data frame will coerce it
               ## to a data frame. Take care.
        zooplank_surv = target(split_surv(zooplank_all_rename, zooplank_matching[[zooplank_names]], spatial_vars),
                      dynamic = map(
                        .trace = c(zooplank_names),
                        zooplank_names
                      ),
                      format = "fst_tbl"
                      ),
#
        zooplank_surv_uncertain_sp_names = target(
          get_uncertain_sp_names(zooplank_surv, -999, "Abund_m3"),
          dynamic = map(zooplank_surv)
          ),

         ##Extract species names
         zooplank_sp_names = target(
           zooplank_surv %>%
           dplyr::select(TaxonName) %>%
           dplyr::distinct() %>%
           dplyr::filter(TaxonName != "No_taxa") %>%
           dplyr::pull(TaxonName) %>%
          base::setdiff(zooplank_surv_uncertain_sp_names),
           dynamic = map(
             .trace = zooplank_names,
             zooplank_names,
             zooplank_surv_uncertain_sp_names,
             zooplank_surv
           ),
           format = "qs"
         ),


         #From now on, every call to surv should give me one survey at a time.

         ##Convert to wide format
         zooplank_wide = target(
           surv_to_wide(zooplank_surv, "Abund_m3"),
           dynamic = map(
             .trace = zooplank_names,
             zooplank_names,
             zooplank_surv
           ),
           format = "fst_tbl"
         ),
#
         ##Align env and samples
         zooplank_env = target(
           align_env_samp(surv = zooplank_wide,
                          spatial_vars = spatial_vars,
                          env_res = regrid_resolution,
                          env_offset = env_offset,
                          env_round = env_round),
           dynamic = map(
             .trace = zooplank_names,
             zooplank_names,
             zooplank_wide
           ),
           format = "fst_tbl"
         ),
         ##Filter by Frequency of occurrence and coefficient of variance
         zooplank_sp_keep = target(
           foc_cov_filter(surv_env = zooplank_env,
                          sp_names = zooplank_sp_names,
                          freq_range = freq_range,
                          cov_min = cov_min,
                          min_occurrence = min_occurrence
           ),
           dynamic = map(
             .trace = zooplank_names,
             zooplank_names,
             zooplank_sp_names,
             zooplank_env
           ),
           format = "qs"
         ),
         zooplank_env_filter = target(
           filter_surv_env(surv_env = zooplank_env,
                           surv_sp_names = zooplank_sp_keep,
                           env_id_col = env_id_col,
                           spatial_vars = spatial_vars,
                           env_vars = env_names
           ),
           dynamic = map(
             .trace = zooplank_names,
             zooplank_env,
             zooplank_sp_keep,
             zooplank_names,
           ),
           format = "fst_tbl"
         ),
         ##Fit GF models
         zooplank_gf = target(
           stats::setNames(list(gradientForest::gradientForest(
                             data = as.data.frame(zooplank_env_filter),
                             predictor.vars = env_names,
                             response.vars = zooplank_sp_keep,
                             ntree = gf_trees,
                             compact = gf_compact,
                             nbin = gf_bins,
                             transform = NULL,
                             corr.threshold = gf_corr_thres,
                             maxLevel = floor(log2(length(zooplank_sp_keep) * 0.368 / 2)),
                             trace = TRUE
                             )),
                           nm = c(zooplank_names)),
           dynamic = map(
             .trace = zooplank_names,
             zooplank_env_filter,
             zooplank_sp_keep,
             zooplank_names,
           ),
           format = "qs"
         ),
        ##combined GF for zooplanks
        zooplank_combined_gf = target(
          do.call(gradientForest::combinedGradientForest,
                  c(zooplank_gf, nbin = gf_bins)
                  ),
          ##dynamic = combine(zooplank_gf),
          format = "qs"
        ),


        ## ##Transform the environment. No need for target(), I am not mapping or combining
        ## ##NOTE: don't try to hack the drake transforms, just run a parallel pipeline, use functions
        ## ## to reuse effort.
        ## Better reason to avoid combining: combined GF needs different processing steps
        ## gf_all = target(
        ##   vctrs::vec_c(zooplank_gf_named, list(zooplank_combined_gf = zooplank_combined_gf)),
        ##   ),

        ## gf_all_names = names(gf_all),



        env_trans_zooplank_combined = target(
          tibble::as_tibble(predict(object = zooplank_combined_gf,
                  newdata = env_round[, env_names],
                  extrap = extrap)),
          format = "fst_tbl"
          ),
        env_trans_zooplank_combined_spatial = target(
          cbind(env_round[, spatial_vars], env_trans_zooplank_combined),

          format = "fst_tbl"
        ),

        env_trans_zooplank = target(
          predict(object = zooplank_gf[[1]],
                  newdata = as.data.frame(env_round[, as.character(unique(zooplank_gf[[1]]$res$var))]),
                  extrap = extrap),
          dynamic = map(zooplank_gf,
                        zooplank_names,
                        .trace = zooplank_names
                        ),
          format = "fst_tbl"
        ),
        env_trans_zooplank_spatial = target(
          list(cbind(env_round[, spatial_vars], env_trans_zooplank)),
          dynamic = map(env_trans_zooplank),
          format = "qs"
        ),

        env_trans_zooplank_spatial_named = target(
          name_list(env_trans_zooplank_spatial, zooplank_names),
          format = "qs"
        ),


        env_trans_zooplank_all = target(
          vctrs::vec_c(env_trans_zooplank_spatial_named, list(zooplank_combined_gf = env_trans_zooplank_combined_spatial)),
          format = "qs"
        ),

        env_trans_zooplank_names = names(env_trans_zooplank_all),

         ## ext_pl_biooracle = target(plot_temp(env_trans_spatial,
         ##                              spatial_vars,
         ##                              marine_map,
         ##                              env_extent,
         ##                              file_out(!!ext_pl_temp_file)
         ##                              ),
         ##                             hpc = FALSE),
         track_state = target(state_rds(file_out(!!state_rds_file),
                                 file_out(!!state_yaml_file)),
                                     hpc = FALSE),

         plot_range = target(gf_plot_wrapper(gf_model = zooplank_combined_gf,
                                      plot_type = "Predictor.Ranges",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_zooplank_gf_range_file)),
                                     hpc = FALSE),
         plot_density = target(gf_plot_wrapper(gf_model = zooplank_combined_gf,
                                      plot_type = "Predictor.Density",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_zooplank_gf_density_file)),
                                     hpc = FALSE),
         plot_cumimp = target(gf_plot_wrapper(gf_model = zooplank_combined_gf,
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_zooplank_gf_cumimp_file)),
                                     hpc = FALSE),
         plot_perf = target(gf_plot_wrapper(gf_model = zooplank_combined_gf,
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_zooplank_gf_perf_file)),
                                     hpc = FALSE),

         ## Cluster, with k-means
         ## I will use the drake plan to parallelise, rather than relying on the clustering function.
         ## Does not transfer as well, but avoids having to tweak the parallel setup to accomodate a single function that wants multiple nodes.
         ## for each env_trans_all, sweep over k using clara. a cross() transform
         ## find the min cluster size for each env_trans_all x k combination
         ## for each env_trans_all, plot min size vs k
         ## for each env_trans_all, return best k and associated clustering
         ## plot best k and associated clustering

         cluster_zooplank = target(
           cluster_capture(env_trans_zooplank_names,
                           env_trans_zooplank_all[[env_trans_zooplank_names]][, names(env_trans_zooplank_all[[env_trans_zooplank_names]]) %in% env_names],
                           k_range,
                           cluster_reps,
                           samples = clara_samples,
                           sampsize = clara_sampsize,
                           trace = clara_trace,
                           rngR = clara_rngR,
                           pamLike = clara_pamLike,
                           correct.d = clara_correct.d),
           dynamic = cross(
             env_trans_zooplank_names,
             .trace = c(env_trans_zooplank_names, cluster_reps, k_range),
             cluster_reps,
             k_range,
             ),
           format = "qs"
         ),
         ## cluster_copepod_combined = target(
         ##   cluster_capture("copepod_combined", env_trans_copepod_combined,
         ##                   k, nrep,
         ##       samples = clara_samples,
         ##       sampsize = clara_sampsize,
         ##       trace = clara_trace,
         ##       rngR = clara_rngR,
         ##       pamLike = clara_pamLike,
         ##       correct.d = clara_correct.d),
         ##   transform = cross(
         ##     nrep = !!seq.int(1,cluster_reps),
         ##     k = !!k_range
         ##   )
         ## ),

         ## cluster_copepod_df = target(
         ##   rbind(cluster_copepod),
         ##   transform = combine(cluster_copepod)
         ## ),

         ## cluster_copepod_all_df = target(
         ##   rbind(cluster_copepod_df, cluster_copepod_combined),
         ##   transform = combine(cluster_copepod_combined)
         ## ),
         pl_zooplank_clust_perfs = ggsave_wrapper(
           filename = file_out(!!pl_zooplank_kmed_perf),
           plot = ggplot(
             data.frame(cluster_zooplank[, c("dataname", "k", "min_clust_ratio")],
                        pass = as.factor(cluster_zooplank$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = min_clust_ratio, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),
         pl_zooplank_clust_perfs_sil = ggsave_wrapper(
           filename = file_out(!!pl_zooplank_kmed_perf_sil),
           plot = ggplot(
             data.frame(cluster_zooplank[, c("dataname", "k", "sil_avg")],
                        pass = as.factor(cluster_zooplank$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = sil_avg, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),

         cluster_zooplank_best_df = cluster_zooplank %>%
           dplyr::group_by(dataname) %>%
           dplyr::filter(min_clust_ratio >= min_clust_thres) %>%
           dplyr::filter(k == max(k)) %>%
           dplyr::filter(min_clust_ratio == max(min_clust_ratio)) %>%
           dplyr::ungroup() %>%
           dplyr::arrange(dataname),

         ## plot best cluster for each group
        zooplank_env_filter_list = target(
          list(zooplank_env_filter),
          dynamic = map(zooplank_env_filter),
          format = "qs"
        ),
        zooplank_env_filter_list_all = target(
          vctrs::vec_c(name_list(zooplank_env_filter_list, zooplank_names), list(zooplank_combined_gf = zooplank_env_filter)),
          format = "qs"
        ),

         pl_zooplank_clusters = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("zooplank_clust_map_",
                               cluster_zooplank_best_df$dataname,
                               ".png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_zooplank_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = NULL,
               grids = zooplank_env_filter_list_all[[cluster_zooplank_best_df$dataname]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(cluster_zooplank_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

         pl_zooplank_clusters_samples = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("zooplank_clust_map_",
                               zooplank_names,
                               "_samples.png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_zooplank_best_df[cluster_zooplank_best_df$dataname == zooplank_names, ]$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = zooplank_wide,
               grids = zooplank_env_filter_list_all[[zooplank_names]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(zooplank_names,
                         zooplank_wide),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

         ## We want to see how much variability exists in the clustering algorithm due to random restarts
         ## I want to reuse exising runs as much as possible

         test_cluster_rand = target(
           cluster_capture("zooplank_combined", env_trans_zooplank_all$zooplank_combined_gf,
                           k_range, cluster_reps_test,
               samples = clara_samples,
               sampsize = clara_sampsize,
               trace = clara_trace,
               rngR = clara_rngR,
               pamLike = clara_pamLike,
               correct.d = clara_correct.d,
               keep.data = FALSE,
               medoids.x = FALSE),
           dynamic = cross(
             .trace = c(cluster_reps_test, k_range),
             cluster_reps_test,
             k_range
             ),
           format = "qs"
           ),


         test_cluster_stats = plyr::ddply(test_cluster_rand, c("dataname", "k"),
                                    summarise,
                                    N = length(min_clust_ratio),
                                    mean = mean(min_clust_ratio),
                                    sd = sd(min_clust_ratio),
                                    se = sd / sqrt(N),
                                    max = max(min_clust_ratio)),

         ## test_cluster_best = test_cluster_bind %>%
         ##   dplyr::group_by(k) %>%
         ##   dplyr::filter(min_clust_ratio == max(min_clust_ratio)) %>%
         ##   dplyr::first() %>%
         ##   dplyr::ungroup(),

         ## I really think error bars are the way to show it
         pl_test_cluster_rand = ggsave_wrapper(
           filename = file_out(!!pl_test_cluster_rand_file),
           plot = ggplot(
             test_cluster_stats[, c("dataname", "k", "max", "mean", "se")],
             mapping = aes(x = k, y = mean)) +
             geom_errorbar(aes(ymin = mean - se, ymax=mean+se), width = 0.2) +
             geom_point() +
             geom_point(data = test_cluster_stats[, c("dataname", "k", "max")], mapping = aes(x = k, y = max)) +
             facet_wrap(vars(dataname)),
         ),


        ## Alternative clustering approach

        ## Heirarchical clustering with SIMPROF


         cluster_zooplank_simprof = target(
           vctrs::vec_c(cluster_capture_simprof("zooplank_combined_gf",
                                   env_trans_zooplank_all$zooplank_combined_gf,
                                   env_names,
                                   subset_rounding = 4
                                   ),

                        cluster_capture_simprof("zooplank_combined_gf",
                                                ##Sorry, this is terrible, but I am only showing it doesn't
                                                env_trans_zooplank_all$zooplank_combined_gf[
                                                  155 <= env_trans_zooplank_all$zooplank_combined_gf$lon &
                                                  160 >= env_trans_zooplank_all$zooplank_combined_gf$lon &
                                                  -30 <= env_trans_zooplank_all$zooplank_combined_gf$lat &
                                                  -25 >= env_trans_zooplank_all$zooplank_combined_gf$lat ,
                                                 ],
                                   env_names,
                                   subset_rounding = NULL
                                   ),
                        ),
           format = "qs"
         ),

        plot_simprof_aus = target(
          simprof_plot_wrapper(cluster_zooplank_simprof[[1]],
                                                       out_file =
             here::here("outputs",
                        paste0("zooplank_clust_simprof_",
                               "aus",
                               ".png"))
             ),
             hpc = FALSE
          ),
        plot_simprof_gbr = target(
          simprof_plot_wrapper(cluster_zooplank_simprof[[2]],
                               out_file =
                                 here::here("outputs",
                                            paste0("zooplank_clust_simprof_",
                                                   "gbr",
                                                   ".png"))
             ),
             hpc = FALSE
          ),
         ##also use silhouette witdth with cluster

         ## env_trans_spatial = env_merge_spatial(env_trans, env_round, spatial_vars),
         ## env_trans_wide = env_wide_list(env_trans_spatial),


         ##Microbe data
         microbe_sites = target(
           data.table::fread(file_in(!!microbe_bacteria_context),
                             sep = ",",
                             select =c(
                               "Sample ID",
                               "Depth [m]",
                               "Microbial Abundance [cells per ml]",
                               "Date Sampled",
                               "Latitude [decimal degrees]",
                               "Longitude [decimal degrees]"),
                             col.names = c(
                               "Sample.ID",
                               "Depth",
                               "Microbial.Abundance",
                               "Date",
                               "lat",
                               "lon"),
                             check.names = TRUE,
                             key = c("Sample.ID"),
                             data.table = TRUE,
                             stringsAsFactors = FALSE),
           format = "fst_dt",
           ),
  microbe_sites_region = target(
   samples_in_region(microbe_sites, env_poly, spatial_vars),
   format = "qs"
   ),
         microbe_samples_all = target(
           microbe_load_hack(file_in(!!microbe_bacteria_csv),
                             sep = ",",
                             select =1:3,
                             check.names = TRUE,
                             key = c("Sample.ID", "OTU"),
                             data.table = TRUE,
                             stringsAsFactors = FALSE),
           format = "fst_dt",
           ),


 microbe_samples_region = target(
   data.table::merge.data.table(microbe_samples_all, microbe_sites[microbe_sites_region, ]),
   format = "fst_dt"
 ),

 ## By microbe_samples_region, we have all sample data in a single table.
 ##filter by depth
 microbe_samples_region_depth = target(
   microbe_samples_region[!is.na(Depth) & Depth >= min(depth_range[[depth_names]]) & Depth <= max(depth_range[[depth_names]]) ,], #only 79 samples in region are NA
                          dynamic = map(depth_names),
   format = "fst_dt"
 ),
 ## Group into grid cells, drop depth, date, abundance (which is NA in >99% of rows)
 ## For zooplankton, I created a site by species matrix, then aggregated that into a grid by species matrix, then
 ## filterd by species statistics.
 ## For the microbes, the site by species matrix is huge, too large.
 ## I will work in smaller steps
 ## first, grid all samples without aggregating, and get the grid cell count
 microbe_samples_region_depth_rounded = target(
   microbe_samples_region_depth[ ,
                                .(OTU, OTU.Count,
                                  lon = ((round(lon/regrid_resolution+env_offset) - env_offset) * regrid_resolution),
                                  lat = ((round(lat/regrid_resolution+env_offset) - env_offset) * regrid_resolution)
                                  )
                                ],
   dynamic = map(microbe_samples_region_depth),
   format = "fst_dt"
 ),

 microbe_samples_region_depth_n = target( data.table::uniqueN(microbe_samples_region_depth_rounded, by = c("lon", "lat")),
   dynamic = map( microbe_samples_region_depth_rounded),
   format = "qs"
 ),

 ## second, aggregate, by OTU into each grid cell but keep in long format
 microbe_samples_grid_depth = target(
   microbe_samples_region_depth_rounded[, .(OTU.Count = mean(OTU.Count)), by = c("lon", "lat", "OTU")],
   dynamic = map(microbe_samples_region_depth_rounded),
   format = "qs"
 ),

         microbe_grid_filtered = target(
           foc_cov_filter_microbe(microbe_samples_grid_depth,
                                  n_grids = microbe_samples_region_depth_n,
                                   max_otu,
                           freq_range,
                           cov_min,
                           min_occurrence),
           dynamic = map(microbe_samples_grid_depth,
                         microbe_samples_region_depth_n),
           format = "fst_dt"
           ),

 otu_lut = target(
   unique(microbe_grid_filtered[, .(OTU, OTU_short)]),
           dynamic = map(microbe_grid_filtered),
   format = "fst_dt"
   ),

         ##Size of matrix exceeds R limits without filtering, but is largely 0, about 0.7% are non-zero.
         ##Remove rare OTU's first.
         microbe_grid_wide = target(
           data.table::dcast(microbe_grid_filtered,
                             formula = lon+lat ~ OTU_short,
                             fill =  0,
                             value.var = "OTU.Count"),
           dynamic = map(microbe_grid_filtered,
                         depth_names,
                         .trace = depth_names),
           format = "fst_dt"),


         microbe_grid_env = target(
           merge(as.data.table(env_round), microbe_grid_wide, by = spatial_vars),
           dynamic = map(
             .trace = depth_names,
             depth_names,
             microbe_grid_wide
           ),
           format = "fst_dt"
         ),
         microbe_gf = target(
           stats::setNames(list(gradientForest::gradientForest(
                             data = as.data.frame(microbe_grid_env),
                             predictor.vars = env_names,
                             response.vars = otu_lut$OTU_short,
                             ntree = gf_trees,
                             compact = gf_compact,
                             nbin = gf_bins,
                             transform = NULL,
                             corr.threshold = gf_corr_thres,
                             maxLevel = floor(log2(length(otu_lut$OTU_short) * 0.368 / 2)),
                             trace = TRUE
                             )),
                           nm = c(depth_names)),
           dynamic = map(
             .trace = depth_names,
             microbe_grid_env,
             otu_lut,
             depth_names
           ),
           format = "qs"
         ),


        env_trans_microbe = target(
          stats::setNames(nm = c(depth_names),
                          object =
                            list(predict(object = microbe_gf[[depth_names]],
                                    newdata = as.data.frame(env_round[, as.character(unique(microbe_gf[[depth_names]]$res$var))]),
                                    extrap = extrap))
                          ),
          dynamic = map(
            depth_names,
            .trace = depth_names
          ),
          format = "qs"
        ),
        env_trans_microbe_spatial = target(
          stats::setNames(nm = c(depth_names),
                          object =
                            cbind(env_round[, spatial_vars], env_trans_microbe[[depth_names]])
                         ),
          dynamic = map(
            depth_names,
            .trace = depth_names
          ),
          format = "qs"
        ),


         cluster_microbe = target(
           cluster_capture(depth_names,
                           env_trans_microbe[[depth_names]][, names(env_trans_microbe[[depth_names]]) %in% env_names],
                           k_range,
                           cluster_reps,
                           samples = clara_samples,
                           sampsize = clara_sampsize,
                           trace = clara_trace,
                           rngR = clara_rngR,
                           pamLike = clara_pamLike,
                           correct.d = clara_correct.d),
           dynamic = cross(
             depth_names,
             .trace = c(depth_names, cluster_reps, k_range),
             cluster_reps,
             k_range,
             ),
           format = "qs"
         ),

         cluster_microbe_best_df = cluster_microbe %>%
           dplyr::group_by(dataname) %>%
           dplyr::filter(min_clust_ratio >= min_clust_thres) %>%
           dplyr::filter(k == max(k)) %>%
           dplyr::filter(min_clust_ratio == max(min_clust_ratio)) %>%
           dplyr::ungroup() %>%
           dplyr::arrange(dataname),

    microbe_samples_region_depth_list = target(
    stats::setNames(nm = depth_names,
                    object =
                        list(microbe_samples_region_depth)),
    dynamic = map( microbe_samples_region_depth,
                    depth_names),
    format = "qs"
    ),
    microbe_grid_env_list = target(
    stats::setNames(nm = depth_names,
                    object =
                        list(microbe_grid_env)),
    dynamic = map( microbe_grid_env,
                    depth_names),
    format = "qs"
    ),
         pl_microbe_clusters = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("microbe_clust_map_",
                               cluster_microbe_best_df$dataname,
                               ".png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_microbe_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = as.data.frame(unique(microbe_samples_region_depth_list[[cluster_microbe_best_df$dataname]][, c("lat", "lon")])),
               grids = as.data.frame(microbe_grid_env_list[[cluster_microbe_best_df$dataname]])[, spatial_vars],
               clip_samples = TRUE
             )
            ),
           dynamic = map(cluster_microbe_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

         pl_microbe_clusters_samples = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("microbe_clust_map_samples_",
                               cluster_microbe_best_df$dataname,
                               ".png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_microbe_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = as.data.frame(unique(microbe_samples_region_depth_list[[cluster_microbe_best_df$dataname]][, c("lat", "lon")])),
               grids = as.data.frame(microbe_grid_env_list[[cluster_microbe_best_df$dataname]])[, spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(cluster_microbe_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

 ##plot some microbe stuff

         plot_range_microbe = target(gf_plot_wrapper(gf_model = microbe_gf[[depth_names]],
                                      plot_type = "Overall.Importance",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("microbe_gf_varimp_", depth_names, ".png"))),
                                     dynamic = map(
             .trace = depth_names,
             depth_names,
             ),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),
         plot_density_microbe = target(gf_plot_wrapper(gf_model = microbe_gf[[depth_names]],
                                      plot_type = "Split.Density",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("microbe_gf_density_", depth_names, ".png"))),
                                     dynamic = map(
             .trace = depth_names,
             depth_names,
             microbe_gf
                                     ),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),
         plot_cumimp_microbe = target(gf_plot_wrapper(gf_model = microbe_gf[[depth_names]],
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("microbe_gf_cumimp_", depth_names, ".png"))),
                                     dynamic = map(
             .trace = depth_names,
             depth_names,
                                     ),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),
         plot_perf_microbe = target(gf_plot_wrapper(gf_model = microbe_gf[[depth_names]],
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("microbe_gf_perf_", depth_names, ".png"))),
                                     dynamic = map(
             .trace = depth_names,
             depth_names,
                                     ),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),

         pl_microbe_freq = ggsave_wrapper(
           filename = file_out(!!pl_microbe_freq_file),
           plot = ggplot(
 data.frame(x = seq.int(1, nrow(microbe_samples_region[ , .(.N), by = OTU])),  as.data.frame(microbe_samples_region[ , .(.N), by = OTU][order(-N), .(N)])),
             mapping = aes(x = x, y = N)) +
             geom_point()
           ),
         pl_microbe_freq_cov = target(ggsave_wrapper(
           filename =   here::here("outputs", paste0("microbe_freq_cov_", depth_names, ".png")),
           plot = ggplot(
  as.data.frame(microbe_samples_region[ , .(
    occ = .N,
    freq = .N/microbe_samples_region_depth_n,
    cov = sd(OTU.Count) / mean(OTU.Count)),
    by = OTU][!is.na(cov), .(freq, cov)]),
             mapping = aes(x = freq, y = cov)) +
             geom_point()
  ),
  dynamic = map(microbe_samples_region_depth_n,
                depth_names)
  ),

         pl_microbe_filtered_freq = target( ggsave_wrapper(
           filename =   here::here("outputs", paste0("microbe_filtered_freq_range_", depth_names, ".png")),
           plot = ggplot(
 data.frame(x = seq.int(1, nrow(microbe_grid_filtered[ , .(.N), by = OTU])),  as.data.frame(microbe_grid_filtered[ , .(.N), by = OTU][order(-N), .(N)])),
             mapping = aes(x = x, y = N)) +
             geom_point()
  ),
 dynamic = map(microbe_grid_filtered,
               depth_names)
           ),



 ##how to filter microbe sites?
 ##attach lat and lon to every OTU, using sample ID?
 ##then drop sample id, and
 ##I only want OTUs that appera in my study area.
 ##The approach for CPR was to make a site species matrix first, then
 ## filter, but that does not work for OTUs, there are too many.
 ## I could use sf to find sample IDs that are in the study area, then
 ## only keep OTUs with a "present" sample ID.
 ## How? filter over microbe_sites, then do a join over sample ID that only keeps rows with a match.
 ## Minimum memory: filter over microbe sites, filter sample IDs in OTU table, make OTU wide, then add env data.
 ##

 ## I have the samples restricted by region, I have OTUs filtered by frequency.
 ## I am missing samples merged into grid cells.
 ## I can round off lats and lons in both tables separately, then the lat-lon becomes the new merge key.

 ## CPR phytoplankton data

         phytoplank_all = target(
           load_phyto_data(plankton_data_root),
           hpc = FALSE), #Workers can't see the same TMPDIR
         phytoplank_all_rename = target(
           dplyr::rename(phytoplank_all, "{spatial_vars[1]}" := Longitude, "{spatial_vars[2]}" := Latitude) %>%
          dplyr::mutate(Abund_m3 = Cells_L*1000, Cells_L = NULL),
           format = "fst_tbl"
         ),
        phytoplank_surv = target(split_surv(phytoplank_all_rename, phytoplank_matching[[phytoplank_names]], spatial_vars),
                      dynamic = map(
                        .trace = c(phytoplank_names),
                        phytoplank_names
                      ),
                      format = "fst_tbl"
                      ),
#
#
         ##Extract species names
         phytoplank_sp_names = target(
           phytoplank_surv %>%
           dplyr::select(TaxonName) %>%
           dplyr::distinct() %>%
           dplyr::filter(TaxonName != "No_taxa") %>%
           dplyr::pull(TaxonName) %>%
          base::setdiff(phytoplank_surv_uncertain_sp_names),
           dynamic = map(
             .trace = phytoplank_names,
             phytoplank_names,
             phytoplank_surv_uncertain_sp_names,
             phytoplank_surv
           ),
           format = "qs"
         ),
        phytoplank_surv_uncertain_sp_names = target(
          get_uncertain_sp_names(phytoplank_surv, -999, "Abund_m3"),
          dynamic = map(phytoplank_surv)
          ),
#
         ##Convert to wide format
         phytoplank_wide = target(
           surv_to_wide(phytoplank_surv, "Abund_m3"),
           dynamic = map(
             .trace = phytoplank_names,
             phytoplank_names,
             phytoplank_surv
           ),
           format = "fst_tbl"
         ),
#
         ##Align env and samples
         phytoplank_env = target(
           align_env_samp(surv = phytoplank_wide,
                          spatial_vars = spatial_vars,
                          env_res = regrid_resolution,
                          env_offset = env_offset,
                          env_round = env_round),
           dynamic = map(
             .trace = phytoplank_names,
             phytoplank_names,
             phytoplank_wide
           ),
           format = "fst_tbl"
         ),
         ##Filter by Frequency of occurrence and coefficient of variance
         phytoplank_sp_keep = target(
           foc_cov_filter(surv_env = phytoplank_env,
                          sp_names = phytoplank_sp_names,
                          freq_range = freq_range,
                          cov_min = cov_min,
                          min_occurrence = min_occurrence
           ),
           dynamic = map(
             .trace = phytoplank_names,
             phytoplank_names,
             phytoplank_sp_names,
             phytoplank_env
           ),
           format = "qs"
         ),
         phytoplank_env_filter = target(
           filter_surv_env(surv_env = phytoplank_env,
                           surv_sp_names = phytoplank_sp_keep,
                           env_id_col = env_id_col,
                           spatial_vars = spatial_vars,
                           env_vars = env_names
           ),
           dynamic = map(
             .trace = phytoplank_names,
             phytoplank_env,
             phytoplank_sp_keep,
             phytoplank_names,
           ),
           format = "fst_tbl"
         ),
         ##Fit GF models
         phytoplank_gf = target(
           stats::setNames(list(gradientForest::gradientForest(
                             data = as.data.frame(phytoplank_env_filter),
                             predictor.vars = env_names,
                             response.vars = phytoplank_sp_keep,
                             ntree = gf_trees,
                             compact = gf_compact,
                             nbin = gf_bins,
                             transform = NULL,
                             corr.threshold = gf_corr_thres,
                             maxLevel = floor(log2(length(phytoplank_sp_keep) * 0.368 / 2)),
                             trace = TRUE
                             )),
                           nm = c(phytoplank_names)),
           dynamic = map(
             .trace = phytoplank_names,
             phytoplank_env_filter,
             phytoplank_sp_keep,
             phytoplank_names,
           ),
           format = "qs"
         ),
        ##combined GF for phytoplanks
        phytoplank_combined_gf = target(
          do.call(gradientForest::combinedGradientForest,
                  c(phytoplank_gf, nbin = gf_bins)
                  ),
          ##dynamic = combine(phytoplank_gf),
          format = "qs"
        ),


         plot_phytoplank_range = target(gf_plot_wrapper(gf_model = phytoplank_combined_gf,
                                      plot_type = "Predictor.Ranges",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_phytoplank_gf_range_file)),
                                     hpc = FALSE),
         plot_phytoplank_density = target(gf_plot_wrapper(gf_model = phytoplank_combined_gf,
                                      plot_type = "Predictor.Density",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_phytoplank_gf_density_file)),
                                     hpc = FALSE),
         plot_phytoplank_cumimp = target(gf_plot_wrapper(gf_model = phytoplank_combined_gf,
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_phytoplank_gf_cumimp_file)),
                                     hpc = FALSE),
         plot_phytoplank_perf = target(gf_plot_wrapper(gf_model = phytoplank_combined_gf,
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      out_file = file_out(!!pl_phytoplank_gf_perf_file)),
                                     hpc = FALSE),

        env_trans_phytoplank_combined = target(
          tibble::as_tibble(predict(object = phytoplank_combined_gf,
                  newdata = env_round[, env_names],
                  extrap = extrap)),
          format = "fst_tbl"
          ),
        env_trans_phytoplank_combined_spatial = target(
          cbind(env_round[, spatial_vars], env_trans_phytoplank_combined),

          format = "fst_tbl"
        ),

        env_trans_phytoplank = target(
          predict(object = phytoplank_gf[[1]],
                  newdata = as.data.frame(env_round[, as.character(unique(phytoplank_gf[[1]]$res$var))]),
                  extrap = extrap),
          dynamic = map(phytoplank_gf,
                        phytoplank_names,
                        .trace = phytoplank_names
                        ),
          format = "fst_tbl"
        ),
        env_trans_phytoplank_spatial = target(
          stats::setNames(
                   list(cbind(env_round[, spatial_vars], env_trans_phytoplank)),
                   nm = phytoplank_names),
          dynamic = map(env_trans_phytoplank,
                        phytoplank_names),
          format = "qs"
        ),

        env_trans_phytoplank_all = target(
          vctrs::vec_c(env_trans_phytoplank_spatial, list(phytoplank_combined_gf = env_trans_phytoplank_combined_spatial)),
          format = "qs"
        ),

        env_trans_phytoplank_names = names(env_trans_phytoplank_all),

         cluster_phytoplank = target(
           cluster_capture(env_trans_phytoplank_names,
                           env_trans_phytoplank_all[[env_trans_phytoplank_names]][, names(env_trans_phytoplank_all[[env_trans_phytoplank_names]]) %in% env_names],
                           k_range,
                           cluster_reps,
                           samples = clara_samples,
                           sampsize = clara_sampsize,
                           trace = clara_trace,
                           rngR = clara_rngR,
                           pamLike = clara_pamLike,
                           correct.d = clara_correct.d),
           dynamic = cross(
             env_trans_phytoplank_names,
             .trace = c(env_trans_phytoplank_names, cluster_reps, k_range),
             cluster_reps,
             k_range,
             ),
           format = "qs"
         ),

         pl_phytoplank_clust_perfs = ggsave_wrapper(
           filename = file_out(!!pl_phytoplank_kmed_perf),
           plot = ggplot(
             data.frame(cluster_phytoplank[, c("dataname", "k", "min_clust_ratio")],
                        pass = as.factor(cluster_phytoplank$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = min_clust_ratio, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),
         pl_phytoplank_clust_perfs_sil = ggsave_wrapper(
           filename = file_out(!!pl_phytoplank_kmed_perf_sil),
           plot = ggplot(
             data.frame(cluster_phytoplank[, c("dataname", "k", "sil_avg")],
                        pass = as.factor(cluster_phytoplank$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = sil_avg, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),

         cluster_phytoplank_best_df = cluster_phytoplank %>%
           dplyr::group_by(dataname) %>%
           dplyr::filter(min_clust_ratio >= min_clust_thres) %>%
           dplyr::filter(k == max(k)) %>%
           dplyr::filter(min_clust_ratio == max(min_clust_ratio)) %>%
           dplyr::ungroup() %>%
           dplyr::arrange(dataname),

         ## plot best cluster for each group
        phytoplank_env_filter_list = target(
          list(phytoplank_env_filter),
          dynamic = map(phytoplank_env_filter),
          format = "qs"
        ),
        phytoplank_env_filter_list_all = target(
          vctrs::vec_c(name_list(phytoplank_env_filter_list, phytoplank_names), list(phytoplank_combined_gf = phytoplank_env_filter)),
          format = "qs"
        ),

         pl_phytoplank_clusters = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("phytoplank_clust_map_",
                               cluster_phytoplank_best_df$dataname,
                               ".png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_phytoplank_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = NULL,
               grids = phytoplank_env_filter_list_all[[cluster_phytoplank_best_df$dataname]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(cluster_phytoplank_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

         pl_phytoplank_clusters_samples = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("phytoplank_clust_map_",
                               phytoplank_names,
                               "_samples.png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_phytoplank_best_df[cluster_phytoplank_best_df$dataname == phytoplank_names, ]$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = phytoplank_wide,
               grids = phytoplank_env_filter_list_all[[phytoplank_names]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(phytoplank_names,
                         phytoplank_wide),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),


 ## Reg Watson's global fish catch data
 ## Species catches are at /QRISdata/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/Annual_TotalCatchSpecies
 ## In a CSV, lat, lon, speciesID, totalCatch (plus a few others, like ocean area),
 ## species ID is propably in /QRISdata/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/TaxonomicData.rds
 ## To map species names to depths, Isaac provided:
 ## https://github.com/IsaakBM/VoCC_Prioritizr_global/blob/master/yscripts/R_scripts/VoCCPrioritizr_00d_CostLayerDepthSpecies.R
 ## Which also needs
 ##  /QRISdata/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/Cost_Layers/Global/Cost_RasterStack_bySpecies.rds
 ##  or maybe
 ##  /QRISdata/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/Cost_Layers/Global/Cost_SpeciesList.rds

 fish_taxon = target(
   {x <- data.table::as.data.table(readRDS(file_in(!!fish_taxon_file )))
   data.table::setkey(x, "TaxonKey")
   },
   format = "qs"
   ),

 ## TaxLevel 6 is resolved to species and subspecies. Some (8) have Rank == NA,and it isn't clear why.
 ## I will keep subspecies, but may need to merge them later

## fish_taxon %>% filter(TaxonKey %in% missing_taxa)

 fish_catch_all = target(
   purrr::map_dfr(fish_years,
                  ~ {cbind(data.table::fread(fish_file_fn(.x), select = c("TaxonKey", "LatCentre", "LonCentre", "TotalCatch"),
                                                     col.names = c("TaxonKey", "lat","lon", "TotalCatch"), key = "TaxonKey"), Year = .x)}
                  ),
   format = "qs"
 ),

 fish_samples = target(
   {
 fish_taxon %>%
 ##Species/Subspecies level, but exclude some enstries (~8) with NA in Rank.
   dplyr::filter(TaxLevel == 6 & !is.na(Rank)) %>%
##not all taxa are found in the range fish_years, which is not a problem, but absent species don't need further processing
  dplyr::filter(!(TaxonKey %in% setdiff(unique(fish_taxon$TaxonKey), unique(fish_catch_all$TaxonKey)))) -> fish_taxon_sp

fish_sp_depth <- rfishbase::species(fish_taxon_sp$TaxonName, fields = c("Species", "DepthRangeShallow", "DepthRangeDeep"), server = "fishbase")
missing_fishbase <- fish_taxon_sp %>% filter(is.na(fish_sp_depth$Species))


 fish_sp_depth_sealife <- rfishbase::species(missing_fishbase$TaxonName, fields = c("Species", "DepthRangeShallow", "DepthRangeDeep"), server = "sealifebase")
 missing_all_base <- missing_fishbase %>% filter(is.na(fish_sp_depth_sealife$Species))

 fish_taxon_depth <- merge(fish_taxon_sp, rbind(fish_sp_depth, fish_sp_depth_sealife), by.x = "TaxonName", by.y = "Species") %>%
   dplyr::select(TaxonKey, TaxonName, DepthRangeShallow, DepthRangeDeep)

  fish_taxon_depth_full <-  fish_taxon_depth %>%
    ##dplyr::filter(complete.cases(fish_taxon_depth[, c("DepthRangeShallow", "DepthRangeDeep")])) %>%
    ##
    dplyr::mutate(is.epi = DepthRangeShallow >= min(depth_range$epi) & DepthRangeShallow < max(depth_range$epi) |
                    DepthRangeDeep >= min(depth_range$epi) & DepthRangeDeep < max(depth_range$epi),
                  is.meso = DepthRangeShallow >= min(depth_range$meso) & DepthRangeShallow < max(depth_range$meso) |
                    DepthRangeDeep >= min(depth_range$meso) & DepthRangeDeep < max(depth_range$meso)
                  )

  fish_taxon_depth_full[is.na(fish_taxon_depth_full$is.epi), "is.epi"] <- FALSE
  fish_taxon_depth_full[is.na(fish_taxon_depth_full$is.meso), "is.meso"] <- FALSE

 ##Keep only the epipelagic taxa
 fish_taxon_epi <- fish_taxon_depth_full[is.epi == TRUE]

 ##Now use the epipelagic taxa to get site by species matrix
 fish_catch_mean <- fish_catch_all[, .(TotalCatch = mean(TotalCatch)), by = .(TaxonKey, lat, lon)]

 data.table::setkey(fish_catch_mean, "TaxonKey")
 data.table::setkey(fish_taxon_epi, "TaxonKey")
 fish_samples <- fish_catch_mean[fish_taxon_epi, .(TaxonKey, TaxonName, lat, lon, TotalCatch)]
 fish_samples[, TaxonName := clean_sp_names(TaxonName)]

   },
format = "qs"
),

 ##filter to environment
fish_samples_region = target(
  fish_samples[samples_in_region(fish_samples, env_poly, spatial_vars), ],
  format = "qs"
  ),

fish_samples_wide = target(
  surv_to_wide(fish_samples_region[ , .(lat, lon, TaxonName, TotalCatch)], "TotalCatch"),
  format = "qs"
  ),


##Fish data falls in an inconvenient spot for aligning, exactly every 0.25 and 0.75, which throws out rounding.
## Add then remove a small offset.
fish_samples_env = target(
  fish_samples_wide %>%
  rphildyerphd::align_sp(spatial_cols = spatial_vars,
                         res = regrid_resolution, grid_offset = env_offset+1e-5,
                         fun = mean) %>%
  dplyr::mutate(lat = lat + 5e-6, lon = lon + 5e-6) %>%
  merge(env_round, by = spatial_vars),
  format = "qs"
  ),


         fish_sp_keep = target(
           foc_cov_filter(surv_env = fish_samples_env,
                          sp_names = names(fish_samples_env)[!names(fish_samples_env) %in% c(env_id_col,spatial_vars)],
                          freq_range = freq_range,
                          cov_min = cov_min,
                          min_occurrence = min_occurrence
           ),
           format = "qs"
         ),
         fish_env_filter = target(
           filter_surv_env(surv_env = fish_samples_env,
                           surv_sp_names = fish_sp_keep,
                           env_id_col = env_id_col,
                           spatial_vars = spatial_vars,
                           env_vars = env_names
           ),
           format = "qs"
         ),
         ##Fit GF models
         fish_gf = target(
           gradientForest::gradientForest(
                             data = as.data.frame(fish_env_filter),
                             predictor.vars = env_names,
                             response.vars = fish_sp_keep,
                             ntree = gf_trees,
                             compact = gf_compact,
                             nbin = gf_bins,
                             transform = NULL,
                             corr.threshold = gf_corr_thres,
                             maxLevel = floor(log2(length(fish_sp_keep) * 0.368 / 2)),
                             trace = TRUE
                           ),
           format = "qs"
         ),

         plot_range_fish = target(gf_plot_wrapper(gf_model = fish_gf,
                                      plot_type = "Overall.Importance",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("fish_gf_varimp",  ".png"))),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),
         plot_density_fish = target(gf_plot_wrapper(gf_model = fish_gf,
                                      plot_type = "Split.Density",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("fish_gf_density",  ".png"))),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),
         plot_cumimp_fish = target(gf_plot_wrapper(gf_model = fish_gf,
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("fish_gf_cumimp",  ".png"))),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),
         plot_perf_fish = target(gf_plot_wrapper(gf_model = fish_gf,
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      out_file = here::here("outputs", paste0("fish_gf_perf",  ".png"))),
             trigger = trigger(condition = TRUE),
                                     hpc = FALSE),

        env_trans_fish = target(
          tibble::as_tibble(predict(object = fish_gf,
                  newdata = env_round[, env_names],
                  extrap = extrap)),
          format = "fst_tbl"
          ),
        env_trans_fish_spatial = target(
          cbind(env_round[, spatial_vars], env_trans_fish),
          format = "fst_tbl"
        ),
         cluster_fish = target(
           cluster_capture("fish",
                           env_trans_fish[, names(env_trans_fish) %in% env_names],
                           k_range,
                           cluster_reps,
                           samples = clara_samples,
                           sampsize = clara_sampsize,
                           trace = clara_trace,
                           rngR = clara_rngR,
                           pamLike = clara_pamLike,
                           correct.d = clara_correct.d),
           dynamic = cross(
             .trace = c(cluster_reps, k_range),
             cluster_reps,
             k_range,
             ),
           format = "qs"
         ),

         pl_fish_clust_perfs = ggsave_wrapper(
           filename = file_out(!!pl_fish_kmed_perf),
           plot = ggplot(
             data.frame(cluster_fish[, c("dataname", "k", "min_clust_ratio")],
                        pass = as.factor(cluster_fish$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = min_clust_ratio, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),
         pl_fish_clust_perfs_sil = ggsave_wrapper(
           filename = file_out(!!pl_fish_kmed_perf_sil),
           plot = ggplot(
             data.frame(cluster_fish[, c("dataname", "k", "sil_avg")],
                        pass = as.factor(cluster_fish$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = sil_avg, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),

         cluster_fish_best_df = cluster_fish %>%
           dplyr::group_by(dataname) %>%
           dplyr::filter(min_clust_ratio >= min_clust_thres) %>%
           dplyr::filter(k == max(k)) %>%
           dplyr::filter(min_clust_ratio == max(min_clust_ratio)) %>%
           dplyr::ungroup() %>%
           dplyr::arrange(dataname),

        fish_env_filter_list_all = target(
          list(fish_gf = fish_env_filter),
          format = "qs"
        ),

         pl_fish_clusters = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("fish_clust_map_",
                               cluster_fish_best_df$dataname,
                               ".png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_fish_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = NULL,
               grids = fish_env_filter_list_all[[cluster_fish_best_df$dataname]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(cluster_fish_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

         pl_fish_clusters_samples = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("fish_clust_map_",
                               "samples.png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_fish_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = fish_samples_wide,
               grids = fish_env_filter_list_all[[cluster_fish_best_df$dataname]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(cluster_fish_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

## Combine all trophic levels together

alltroph_combined_gf = target(
  do.call(gradientForest::combinedGradientForest,
          c(nbin = gf_bins,
            c(list(fish = fish_gf),
            stats::setNames(phytoplank_gf, nm = paste("phytoplank", names(phytoplank_gf), sep = ".")),
            stats::setNames(zooplank_gf, nm = paste("zooplank", names(zooplank_gf), sep = ".")),
            stats::setNames(microbe_gf, nm = paste("microbe", names(microbe_gf), sep = "."))
            )
            )
          ),
  format = "qs"
),

         plot_alltroph_range = target(gf_plot_wrapper(gf_model = alltroph_combined_gf,
                                      plot_type = "Predictor.Ranges",
                                      vars = 1:9,
                                      dpi = 1200,
                                      out_file = file_out(!!pl_alltroph_gf_range_file)),
                                     hpc = FALSE),
         plot_alltroph_density = target(gf_plot_wrapper(gf_model = alltroph_combined_gf,
                                      plot_type = "Predictor.Density",
                                      vars = 1:9,
                                      dpi = 1200,
                                      out_file = file_out(!!pl_alltroph_gf_density_file)),
                                     hpc = FALSE),
         plot_alltroph_cumimp = target(gf_plot_wrapper(gf_model = alltroph_combined_gf,
                                      plot_type = "Cumulative.Importance",
                                      vars = 1:9,
                                      dpi = 1200,
                                      out_file = file_out(!!pl_alltroph_gf_cumimp_file)),
                                     hpc = FALSE),
         plot_alltroph_perf = target(gf_plot_wrapper(gf_model = alltroph_combined_gf,
                                      plot_type = "Performance",
                                      vars = 1:9,
                                      dpi = 1200,
                                      out_file = file_out(!!pl_alltroph_gf_perf_file)),
                                     hpc = FALSE),

        env_trans_alltroph = target(
          tibble::as_tibble(predict(object = alltroph_combined_gf,
                  newdata = env_round[, env_names],
                  extrap = extrap)),
          format = "fst_tbl"
          ),
        env_trans_alltroph_spatial = target(
          cbind(env_round[, spatial_vars], env_trans_alltroph),
          format = "fst_tbl"
        ),
         cluster_alltroph = target(
           cluster_capture("alltroph",
                           env_trans_alltroph[, names(env_trans_alltroph) %in% env_names],
                           k_range,
                           cluster_reps,
                           samples = clara_samples,
                           sampsize = clara_sampsize,
                           trace = clara_trace,
                           rngR = clara_rngR,
                           pamLike = clara_pamLike,
                           correct.d = clara_correct.d),
           dynamic = cross(
             .trace = c(cluster_reps, k_range),
             cluster_reps,
             k_range,
             ),
           format = "qs"
         ),

         pl_alltroph_clust_perfs = ggsave_wrapper(
           filename = file_out(!!pl_alltroph_kmed_perf),
           plot = ggplot(
             data.frame(cluster_alltroph[, c("dataname", "k", "min_clust_ratio")],
                        pass = as.factor(cluster_alltroph$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = min_clust_ratio, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),
         pl_alltroph_clust_perfs_sil = ggsave_wrapper(
           filename = file_out(!!pl_alltroph_kmed_perf_sil),
           plot = ggplot(
             data.frame(cluster_alltroph[, c("dataname", "k", "sil_avg")],
                        pass = as.factor(cluster_alltroph$min_clust_ratio >= min_clust_thres)),
             mapping = aes(x = k, y = sil_avg, colour = pass)) +
             geom_point() +
             facet_wrap(vars(dataname)),
         ),

         cluster_alltroph_best_df = cluster_alltroph %>%
           dplyr::group_by(dataname) %>%
           dplyr::filter(min_clust_ratio >= min_clust_thres) %>%
           dplyr::filter(k == max(k)) %>%
           dplyr::filter(min_clust_ratio == max(min_clust_ratio)) %>%
           dplyr::ungroup() %>%
           dplyr::arrange(dataname),

        ## alltroph_env_filter_list_all = target(
        ##   list(alltroph_combined_gf = alltroph_env_filter),
        ##   format = "qs"
        ## ),

         pl_alltroph_clusters = target(
           ggsave_wrapper(
             here::here("outputs",
                        paste0("alltroph_clust_map_",
                               cluster_alltroph_best_df$dataname,
                               ".png")),
             plot_clust_poly(
               env_round[, spatial_vars],
               cluster_alltroph_best_df$clust[[1]]$clustering,
               spatial_vars,
               marine_map,
               env_poly,
               regrid_resolution,
               samples = NULL,
               ## grids = alltroph_env_filter_list_all[[cluster_alltroph_best_df$dataname]][,spatial_vars],
               clip_samples = FALSE
             )
            ),
           dynamic = map(cluster_alltroph_best_df),
           trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
           ),

         ## pl_alltroph_clusters_samples = target(
         ##   ggsave_wrapper(
         ##     here::here("outputs",
         ##                paste0("alltroph_clust_map_",
         ##                       "samples.png")),
         ##     plot_clust_poly(
         ##       env_round[, spatial_vars],
         ##       cluster_alltroph_best_df$clust[[1]]$clustering,
         ##       spatial_vars,
         ##       marine_map,
         ##       env_poly,
         ##       regrid_resolution,
         ##       samples = alltroph_samples_wide,
         ##       ## grids = alltroph_env_filter_list_all[[cluster_alltroph_best_df$dataname]][,spatial_vars],
         ##       clip_samples = FALSE
         ##     )
         ##    ),
         ##   dynamic = map(cluster_alltroph_best_df),
         ##   trigger = trigger(condition = TRUE) #Always replot the figures, dynamic variables cannot be used here
         ##   ),



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
      clustermq.scheduler = "multiprocess"
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

#future::plan(future.callr::callr)
  drake::make(pl, seed = r_seed,
              parallelism = parallelism,
              jobs = jobs, ## 6 jobs, for 6 surveys
              log_make = here::here("outputs", "drake_log.log"),
              template = list(log_file = here::here("outputs", "drake_worker_log.txt"),
                              memory = 16000,
                              cores = 4,
                              walltime = "20:00:00"),
              verbose = 1,
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
