##essential for _targets.R
library(targets)

## Load functions and globals
## Load all files in the functions folder
source_files <- list.files("./functions", full.names = TRUE)
sapply(source_files, source)

## Load global objects
## Usually something living inside data()
source("./functions/params.R") ##might be redundant, probably sourced above

## Set global R options, mostly clustermq
future_plan <- configure_parallel(default_clustermq = TRUE,
                   future_plan = future.callr::callr)

## Set targets options
tar_option_set(
  ##qs format is smaller and reads/writes faster than RDS. needs qs package.
  format = "qs",
  ## By default, don't load anything into the master
  ## The workers have access to the same FS, and
  ## can read from the cache just as easily as
  ## master, but then we don't have to store
  ## all the info in memory twice.
  storage = "worker",
  retrieval = "worker",
  deployment =  "worker",
  garbage_collection = TRUE,
  memory = "transient",
  error =  "continue",
  ## Load libraries here. Faster than library(..) at top of this file.
  packages = c(
    "gfbootstrap",
    "gradientForest",
    "castcluster",
    "apcluster",
    "cluster",
    "NbClust",
    "planktonr",
    "qs",
    "sf",
    "terra",
    "raster",
    "stars",
    "fpc",
    "semver",
    "processx",
    "wdpar",
    "RCurl",
    "tmap",
    "ggplot2",
    "ggthemes",
    "jsonlite",
    "glue",
    "ncdf4",
    # "ClimateOperators",
    "sdmpredictors",
    "data.table",
    "tidyverse",
    "purrr",
    "lutz", # needed for loading phy and zoo
    "lubridate", # needed for loading phy and zoo
    "rfishbase",
    "indicspecies",
		"float",
		"torch"
  ),
  ## Debug breaking functions. Otherwise, comment
  ## debug = "zoo_long",
  workspace_on_error = TRUE,
  ## Track these packages, rebuild targets if package changes. "packages" option will not rebuild targets.
  imports = c(
    "gfbootstrap",
    "rphildyerphd"
  )


)

## List of targets

list(

## Lists within lists is ok too, if we want to encapsulate
## eg setup_list_targets(), a function that returns a list of tar_targets.

  ## Steps
  #
  # For every survey, we
  # 1. Load in data <- unique
  # 2. clean and shape into long form <- unique
  # 3. merge with env, applying filters <- common
  # 4. fit a gradientForest object <- common
  # 5. Predict and cluster using the gradient forest object <- common
  #
  # For each trophic level, we fit a combined GF. then predict and cluster it
  #
  # For all gf objects, we fit a combined GF, then predict and cluster.
  #
  # We also need to set up the env data and polygons, which is common to all surveys
  #
  # Last of all, plotting

  ## Load in all the input targets
  file_inputs(), #returns a list of targets
  ##
  ## Load in environmental data and domain
  domain_extent_targets(
    mapfile_location,
    map_layer,
    env_bounds
  ),
  
  tar_target(
      clust_methods_target,
      clust_methods,
      iteration = "vector"
  ),

  env_domain_targets(
      biooracle_folder,
      env_biooracle_names,
      env_poly,
      max_depth,
      regrid_resolution,
      spatial_vars,
      env_limits_sd,
      env_offset,
      env_id_col
  ),

  ## Load in biological data and convert to
  ## long form
  tar_target(
    zoo_long,
    load_zoo_long(zoo_load_script,
                  zoo_data_dir,
                  zoo_matching,
                  zoo_names,
                  spatial_vars,
                  depth_names,
                  depth_range
                  )
  ),

  tar_target(
    phy_long,
    load_phy_long(
      phy_load_script,
      phy_data_dir,
      phy_matching,
      phy_names,
      spatial_vars,
      depth_names,
      depth_range
    )
  ),

  tar_target(
    bac_long,
    load_bac_long(
      bac_otu_csv,
      bac_site_csv,
      spatial_vars,
      depth_names,
      depth_range
    )
  ),

  tar_target(
    fish_long,
    load_fish_long(
      fish_taxon_file,
      fish_data_dir,
      fish_years,
      description_to_survey,
      spatial_vars,
      depth_names,
      depth_range,
      regrid_resolution$grid_res_gf,
      env_offset,
      biooracle_folder
    )
  ),

  ## Combine all biological data together
  ## Each row is a survey, the unit GF models are fit over
  ## Downstream targets can iterate over rows
  tar_target(
    all_bio_long,
    merge_all_bio(
      list(
        zoo_long,
        phy_long,
        bac_long,
        fish_long
      ),
      regrid_res = regrid_resolution$grid_res_gf,
      env_offset = env_offset,
      agg_fun = mean,
      spatial_vars = spatial_vars
    ),
    iteration = "vector"
  ),
  ##
  ## Combine biological and environmental data
  ## At this step we also
  ## filter and merge the samples
  ##  - Clean species names
  ##  - remove rare or uninformative species
  ##  - remove species with inconsistent sampling over time
  ##  The output has env row keys, ready to push into wide form for GF
  ##  and merge with environmental data

  tar_target(
    all_bio_env,
    merge_bio_env(
      env_domain_fit,
      all_bio_long,
      spatial_vars,
      regrid_res = regrid_resolution$grid_res_gf,
      env_offset,
      agg_fun,
      env_id_col,
      freq_range,
      cov_min,
      min_occurrence,
      max_taxa
    ),
    pattern = cross(env_domain_fit, all_bio_long),
    iteration = "vector"
  ),

  tar_target(
    gfbootstrap_survey,
    fit_gfbootstrap(
      all_bio_env,
      env_biooracle_names,
      gf_trees,
      gf_compact,
      gf_bins,
      gf_corr_thres
    ),
    pattern = map(all_bio_env)
  ),

  ## Create combined GF models
  ## One per trophic level
  ## one for "all" within an env_domain
  ## Env domains should not be crossed
  ## depth and trophic can be crossed
  ## eg. all depths at trophic X, all trophic at depth X
  tar_target(
    gfbootstrap_combined_tmp,
    combine_gfbootstrap_p1(
        gfbootstrap_survey,
        custom_combinations
    )
    ## Do NOT map over gfbootstrap_survey
  ),
  tar_target(
    gfbootstrap_combined,
    combine_gfbootstrap_p2(
      gfbootstrap_combined_tmp,
      gf_bins,
      gf_bootstrap_combinations
      ),
    pattern = map(gfbootstrap_combined_tmp)
  ),

  tar_target(
    gfbootstrap_diagnostics,
    gfbootstrap_diagnostic_stats(gfbootstrap_combined,
                                 gfbootstrap_predicted,
                                 env_domain_cluster,
                                 gfbootstrap_cluster,
                                 env_biooracle_names,
                                 pred_importance_top),
    pattern = map(gfbootstrap_combined, gfbootstrap_predicted)
  ),
  tar_target(
      gfbootstrap_diagnostics_plots,
      gfbootstrap_diagnostic_plots(gfbootstrap_combined,
                                   gfbootstrap_diagnostics,
                                   env_domain_plot,
                                   env_domain_cluster,
                                   env_poly,
                                   marine_map,
                                   env_id_col,
                                   env_biooracle_names,
                                   pred_importance_top,
                                   plot_description = "gfdiagnostics",
                                         output_folder),
      pattern = map(gfbootstrap_combined, gfbootstrap_diagnostics),
      format = "file"
  ),
  tar_target(
    gf_survey,
    fit_gf(
      all_bio_env,
      env_biooracle_names,
      gf_trees,
      gf_compact,
      gf_bins,
      gf_corr_thres
      ),
    pattern = map(all_bio_env)
  ),

  tar_target(
      gf_combined_tmp,
      combine_gf_p1(
          gf_survey,
          custom_combinations
      )
      ## Do NOT map over gfbootstrap_survey
  ),
  tar_target(
      gf_combined,
      combine_gf_p2(
          gf_combined_tmp,
          gf_bins
      ),
      pattern = map(gf_combined_tmp)
  ),

  tar_target(
      gf_predicted,
      predict_gf(
          gf_combined,
          env_domain_plot, ## this target knows it's own env_domain provenance, and loads in the appropriate env_domain branch.
          env_biooracle_names,
          extrap,
          pred_importance_top,
          env_id_col,
          depth_range
      ),
      pattern = map(gf_combined)
  ),

  tar_target(
    gf_cluster_kmedoids,
    cluster_gf_kmedoids(
        gf_predicted,
        env_domain_plot,
        cluster_min_k,
        cluster_max_k,
        cluster_fixed_k,
        k_range,
        clara_samples,
        clara_sampsize,
        clara_trace,
        clara_rngR,
        clara_pamLike,
        clara_correct.d
    ),
    pattern = map(gf_predicted)
  ),


  tar_target(
    nbclust_plots,
    plot_nbclust_rank(),
    pattern = map(gf_predicted)
  ),

  ## Adjust gf_kmedoids to be compatible with this function
  ## tar_target(
  ##   gf_kmedoid_polygons,
  ##   cluster_raster_to_polygons(
  ##     gfbootstrap_cluster,
  ##     spatial_vars
  ##   ),
  ##   pattern = map(gf_cluster_kmedoids)
  ## ),



  ## tar_target(
  ##   kmedoid_plots,
  ##   plot_kmedoids(gf_kmedoid_polygons,
  ##                 plot_description),
  ##   pattern = map(gf_kmedoid_polygons)
  ## ),

  ## plot rank plot of clusters with hline at 41
  ## plot maps of 41 clusters
  ##



  tar_target(
    gfbootstrap_predicted,
    predict_gfbootstrap(
      gfbootstrap_combined,
      env_domain_cluster, ## this target knows it's own env_domain provenance, and loads in the appropriate env_domain branch.
      env_biooracle_names,
      extrap,
      pred_importance_top,
      env_id_col,
      depth_range
    ),
    pattern = map(gfbootstrap_combined)
  ),

  ##Setting up branching over clustering methods
  tar_target(
    gfbootstrap_cluster,
    cluster_gfbootstrap(
      clust_methods_target,
      gfbootstrap_predicted,
      env_domain_cluster, ## this target knows it's own env_domain provenance, and loads in the appropriate env_domain branch.
      env_id_col,
      spatial_vars,
      m = clust_m,
      min_range = min_range_tol,
      min_tol = min_gamma_tol,
      keep_all_clusts
          ),
    pattern = cross(gfbootstrap_predicted, clust_methods_target)
  ),
  

  tar_target(
    gfbootstrap_plotted,
    plot_gfbootstrap(
      gfbootstrap_cluster,
      gfbootstrap_polygons,
      gfbootstrap_predicted,
      all_bio_env,
      all_bio_long,
      env_poly,
      spatial_vars,
      regrid_resolution$grid_res_cluster,
      marine_map,
      plot_clust_labels,
      plot_description = "clustering_resolution",
      plot_sim_mat = TRUE,
      output_folder
    ),
    pattern = map(gfbootstrap_cluster,
                  gfbootstrap_polygons, cross(gfbootstrap_predicted, clust_methods_target)),
    format = "file"
  ),

  tar_target(
    gfbootstrap_polygons,
    cluster_raster_to_polygons(
      gfbootstrap_cluster,
      spatial_vars
    ),
    pattern = map(gfbootstrap_cluster)
  ),


  tar_target(
    iucn_cat_target,
    iucn_categories,
    iteration = "vector"
  ),

  tar_target(
    mpa_polygons,
    get_mpa_polys(
      country_code,
      iucn_cat_target,
      marine_categories,
      mpa_folder
    ),
    pattern = map(iucn_cat_target)
  ),

  tar_target(
      gfbootstrap_mpa_plots,
      plot_gfbootstrap_mpa(
          gfbootstrap_cluster,
          extrap_polygons_present,
          gfbootstrap_predicted,
          env_poly,
          mpa_polygons,
          spatial_vars,
          regrid_resolution$grid_res_cluster,
          marine_map,
          plot_clust_labels,
          plot_description = "clustering_mpas",
          output_folder
      ),
      pattern = cross(mpa_polygons, map(gfbootstrap_cluster,
                    extrap_polygons_present, cross(gfbootstrap_predicted, clust_methods_target))),
      format = "file"
  ),

  tar_target(
    gfbootstrap_coverage_plots,
    plot_gfbootstrap_coverage(
      gfbootstrap_polygons,
      mpa_polygons,
      plot_description = "cluster_mpa_coverage",
      output_folder
    ),
    pattern =  cross(gfbootstrap_polygons, mpa_polygons),
    format = "file"
  ),

  tar_target(
      cluster_env_extrapolate_present,
      extrapolate_to_env(
          gfbootstrap_combined,
          gfbootstrap_predicted,
          env_domain_plot,
          env_biooracle_names,
          extrap,
          pred_importance_top,
          env_id_col,
          depth_range
      ),
      pattern = map(gfbootstrap_predicted,
                             gfbootstrap_combined)
      ),

  tar_target(
      cluster_env_assign_cluster_present,
      assign_new_sites_to_cluster(
          cluster_env_extrapolate_present,
          gfbootstrap_cluster,
          env_domain_plot,
          env_id_col,
          spatial_vars
      ),
      pattern = map(
					cross(cluster_env_extrapolate_present, clust_methods_target),
					gfbootstrap_cluster)
  ),

  tar_target(
      extrap_polygons_present,
      cluster_raster_to_polygons(
          cluster_env_assign_cluster_present,
          spatial_vars
      ),
      pattern = map(cluster_env_assign_cluster_present)
  ),

  tar_target(
      extrap_plotted_present,
      plot_gfbootstrap(
          gfbootstrap_cluster,
          extrap_polygons_present,
          gfbootstrap_predicted,
          all_bio_env,
          all_bio_long,
          env_poly,
          spatial_vars,
          regrid_resolution$grid_res_plot,
          marine_map,
          plot_clust_labels,
          plot_description = "extrap_present",
          plot_sim_mat = FALSE,
          output_folder
      ),
      pattern = map(gfbootstrap_cluster,
                         extrap_polygons_present, cross(gfbootstrap_predicted, clust_methods_target)),
      format = "file"
  ),

  tar_target(
    extrap_confidence_plot_present,
    plot_confidence(cluster_env_assign_cluster_present,
                           env_domain_plot,
                           env_poly,
                           marine_map,
                           max_clust_prob_plot,
                           env_id_col,
                           spatial_vars,
                           plot_description = "cluster_confidence",
                           output_folder),
    pattern = map(cluster_env_assign_cluster_present),
    format = "file"
  ),

  tar_target(
      extrap_overlay_polygons_present,
      plot_polygon_overlay(
          extrap_polygons_present,
          env_poly,
          plot_description = "extrap_polygon_overlay",
          output_folder
      ),
      pattern = map(env_poly),
      format = "file"
  ),
  tar_target(
    indicator_species,
    find_indicator_species(
        extrap_polygons_present,
        all_bio_long,
        spatial_vars
    ),
    pattern =
        cross(all_bio_long, extrap_polygons_present)
  )

  ## tar_target(
  ##   comparable_clusters,
  ##   compare_clusters(surveys_for_cluster_compare,
  ##                    cluster_compare_methods,
  ##                    gfbootstrap_combined)
  ## )

  ## tar_target(
  ##   gfbootstrap_coverage,
  ##   calc_coverage(
  ##     gfbootstrap_cluster,
  ##     gfbootstrap_predicted,
  ##     spatial_vars,
  ##     output_folder
  ##   ),
  ##   pattern = map(gfbootstrap_cluster, gfbootstrap_predicted),
  ##   format = "file"
  ## )


)
