# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only

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
## future_plan <- configure_parallel(default_clustermq = TRUE,
## future_plan = future.callr::callr)

## Set targets options
tar_option_set(
  ##qs format is smaller and reads/writes faster than RDS. needs qs package.
  format = "qs",
  controller = ccg,
  resources = tar_resources(
    crew = tar_resources_crew(controller = "small")
  ),
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
    "ggrepel",
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
    res_env_target,
    res_env,
    iteration = "vector"
  ),

  tar_target(
    res_gf_target,
    res_gf,
    iteration = "vector"
  ),

  tar_target(
    res_clust_target,
    res_clust,
    iteration = "vector"
  ),

  tar_target(
    res_unique_target,
    unique(c(res_env, res_gf, res_clust)),
    iteration = "vector"
  ),

  tar_target(
      clust_methods_target,
      clust_methods,
      iteration = "vector"
  ),

  tar_target(
    env_biooracle_names,
    make_biooracle_names(
      env_vars,
      env_modes,
      env_year,
      env_pathway,
      env_bathy,
      env_present_only
    )
  ),

  tar_target(
    biooracle_layers,
    unique(purrr::flatten_chr(env_biooracle_names$env_biooracle_names))
  ),

  tar_target(
    biooracle_files,
    fetch_biooracle(biooracle_layers,
                    biooracle_folder),
    pattern = map(biooracle_layers),
    format = "file",
  ),

  
  tar_target(
    env_domain,
    load_env_domain(
      biooracle_folder,
      env_biooracle_names,
      env_poly,
      max_depth,
      res_unique_target,
      spatial_vars,
      env_limits_sd,
      env_offset,
      env_id_col,
      biooracle_files ## not used, here as a dependency
    ),
    pattern = cross(env_poly, res_unique_target, env_biooracle_names),
    iteration = "vector"
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

  ## tar_target(
  ##   fish_long,
  ##   load_fish_long(
  ##     fish_taxon_file,
  ##     fish_data_dir,
  ##     fish_years,
  ##     description_to_survey,
  ##     spatial_vars,
  ##     depth_names,
  ##     depth_range,
  ##     env_offset,
  ##     biooracle_folder
  ##   )
  ## ),

  ## Combine all biological data together
  ## Each row is a survey, the unit GF models are fit over
  ## Downstream targets can iterate over rows
  tar_target(
    all_bio_long,
    data.table::rbindlist(
      list(
        zoo_long,
        phy_long,
        bac_long
        ## fish_long
      ),
      use.names = TRUE
    )
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
    ),
    pattern = cross(env_poly, res_gf_target, all_bio_long),
    iteration = "vector"
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
    resources = tar_resources(
      crew = tar_resources_crew(controller = "small")
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
      {
        future::plan(future.callr::callr,
                     workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                     "1")))
        combine_gf_p2(
          gf_combined_tmp,
          gf_bins
        )
      },
      resources = tar_resources(
        crew = tar_resources_crew(controller = "ram")
      ),
      pattern = map(gf_combined_tmp)
  ),

  tar_target(
    gf_no_env_predicted,
    predict_gf(
      gf_combined,
      res_clust_target,
      env_domain,
      env_biooracle_names,
      extrap,
      pred_importance_top,
      env_id_col,
      depth_range
    ),
    pattern = cross(gf_combined, res_clust_target)
  ),

  tar_target(
    ## Matches gf_predicted, except the predictions are the original env vars, maybe PCA adjusted if Sayre or mcquaid did
    env_predicted,
    predict_env(
        res_clust_target,
        res_gf_target,
        env_domain,
        env_biooracle_names,
        extrap,
        pred_importance_top,
        env_id_col,
        depth_range
    )
  ),

  tar_target(
    gf_predicted,
    rbind(gf_no_env_predicted,
          env_predicted)
  ),

  tar_target(
    plot_gf_cont,
    plot_gf_continuous(
        gf_predicted,
        env_domain,
        env_biooracle_names,
        pred_importance_top,
        marine_map,
        env_poly,
        pca_n_vars,
        pca_scale,
        plot_description = "compositional_turnover",
        output_folder
    ),
    pattern = map(gf_predicted)
  ),

  tar_target(
    nbclust_branch_table,
    nbclust_generate_branches(
      gf_predicted,
      k_range,
      nbclust_index_metadata,
      nbclust_dist,
      nbclust_method,
      nbclust_max_runtime,
      nbclust_include_graphical
    )
    ## no mapping or crossing
  ),

  tar_target(
    nbclust_branch_fitted,
    {
      if (Sys.getenv("OMP_NUM_THREADS") == "" && Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
        Sys.setenv(OMP_NUM_THREADS = Sys.getenv("SLURM_CPUS_PER_TASK"))
      }
      if (Sys.getenv("MKL_NUM_THREADS") == "" && Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
        Sys.setenv(MKL_NUM_THREADS = Sys.getenv("SLURM_CPUS_PER_TASK"))
      }
      if (Sys.getenv("BLIS_NUM_THREADS") == "" && Sys.getenv("SLURM_CPUS_PER_TASK") != "") {
        Sys.setenv(BLIS_NUM_THREADS = Sys.getenv("SLURM_CPUS_PER_TASK"))
      }
      nbclust_fit_branches(
        gf_predicted,
        nbclust_index_metadata,
        nbclust_branch_table,
        env_biooracle_names
      )
    },
    resources = tar_resources(
        crew = tar_resources_crew(controller = "smallram")
    ),
    pattern = map(nbclust_branch_table)
  ),

  tar_target(
    gf_cluster_nbclust,
    nbclust_merge_branches(
      nbclust_branch_fitted,
      gf_predicted,
      nbclust_index_metadata
    )
    ## No mapping or crossing
  ),


  tar_target(
    nbclust_plots,
    plot_nbclust_rank(
        gf_cluster_nbclust,
        plot_description = "nbclust_rank",
        output_folder
    ),
    pattern = map(gf_cluster_nbclust)
  ),

  tar_target(
    gf_cluster_kmedoids,
    cluster_gf_kmedoids(
      gf_predicted,
      cluster_fixed_k,
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
    gf_kmedoid_polygons,
    cluster_raster_to_polygons(
      gf_cluster_kmedoids,
      spatial_vars
    ),
    pattern = map(gf_cluster_kmedoids)
  ),



  tar_target(
    kmedoid_plots,
    plot_gf_kmedoids(gf_cluster_kmedoids,
                     gf_kmedoid_polygons,
                     gf_predicted,
                     cluster_fixed_k,
                     all_bio_env,
                     all_bio_long,
                     env_poly,
                     spatial_vars,
                     marine_map,
                     plot_description = "kmedoid_bioregions",
                     output_folder),
    pattern = map(gf_kmedoid_polygons, gf_cluster_kmedoids, gf_predicted)
  ),

  ## plot rank plot of clusters with hline at 41
  ## plot maps of 41 clusters
  ##

  tar_target(
    gfbootstrap_survey,
    {
      future::plan(future.callr::callr,
                   workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                   "1")))
      fit_gfbootstrap(
      all_bio_env,
      env_biooracle_names,
      gf_trees,
      gf_bootstrap_iters,
      gf_compact,
      gf_bins,
      gf_corr_thres
      )
    },
    resources = tar_resources(
      crew = tar_resources_crew(controller = "multicore")
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
    {
      future::plan(future.callr::callr,
                   workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                   "1")))
      combine_gfbootstrap_p1(
        gfbootstrap_survey,
        custom_combinations
      )
    },
    resources = tar_resources(
      crew = tar_resources_crew(controller = "ram")
    )
    ## Do NOT map over gfbootstrap_survey
  ),
  tar_target(
    gfbootstrap_combined,
    {
      future::plan(future.callr::callr,
                   workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                   "1")))
      combine_gfbootstrap_p2(
        gfbootstrap_combined_tmp,
        gf_bins,
        cgf_bootstrap_combinations
      )
    },
    resources = tar_resources(
      crew = tar_resources_crew(controller = "multicore")
    ),
    pattern = map(gfbootstrap_combined_tmp)
  ),


  tar_target(
    gfbootstrap_predicted,
    {
      future::plan(future.callr::callr,
                   workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                   "1")))
      predict_gfbootstrap(
        gfbootstrap_combined,
        res_clust_target,
        env_domain,
        env_biooracle_names,
        extrap,
        pred_importance_top,
        env_id_col,
        depth_range
      )
    },
    resources = tar_resources(
      crew = tar_resources_crew(controller = "gpu")
    ),
    pattern = cross(gfbootstrap_combined, res_clust_target)
  ),


  tar_target(
    gfbootstrap_diagnostics,
    {
      future::plan(future.callr::callr,
                   workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                   "1")))
      gfbootstrap_diagnostic_stats(gfbootstrap_cluster,
                                   gfbootstrap_combined,
                                   gfbootstrap_predicted,
                                   env_domain,
                                   env_biooracle_names,
                                   pred_importance_top)
    },
    resources = tar_resources(
      crew = tar_resources_crew(controller = "ram")
    ),
    pattern = map(gfbootstrap_cluster)
  ),

  tar_target(
      gfbootstrap_diagnostics_plots,
      gfbootstrap_diagnostic_plots(gfbootstrap_cluster,
                                   gfbootstrap_diagnostics,
                                   gfbootstrap_combined,
                                   env_domain,
                                   env_poly,
                                   marine_map,
                                   env_id_col,
                                   env_biooracle_names,
                                   pred_importance_top,
                                   plot_description = "gfdiagnostics",
                                   output_folder),
      resources = tar_resources(
          crew = tar_resources_crew(controller = "ram")
      ),
      pattern = map(gfbootstrap_cluster, gfbootstrap_diagnostics),
      format = "file"
  ),


  ##Setting up branching over clustering methods
  tar_target(
    gfbootstrap_cluster,
    cluster_gfbootstrap(
      clust_methods_target,
      gfbootstrap_predicted,
      env_domain,
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
      marine_map,
      plot_clust_labels,
      plot_description = "clustering_resolution",
      plot_sim_mat = TRUE,
      output_folder
    ),
    pattern = map(gfbootstrap_cluster,
                  gfbootstrap_polygons, cross(gfbootstrap_predicted, clust_methods_target)),
    resources = tar_resources(
          crew = tar_resources_crew(controller = "ram")
    ),
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
          extrap_polygons,
          gfbootstrap_predicted,
          env_poly,
          mpa_polygons,
          spatial_vars,
          marine_map,
          plot_clust_labels,
          plot_description = "clustering_mpas",
          output_folder
      ),
      pattern = cross(mpa_polygons,
                      map(extrap_polygons,
                          cross(env_biooracle_names,
                                map(gfbootstrap_cluster,
                                    cross(gfbootstrap_predicted, clust_methods_target))))),
      resources = tar_resources(
        crew = tar_resources_crew(controller = "ram")
      ),
      format = "file"
  ),

  tar_target(
    gfbootstrap_coverage_plots,
    plot_gfbootstrap_coverage(
      extrap_polygons,
      env_biooracle_names,
      mpa_polygons,
      plot_description = "cluster_mpa_coverage",
      output_folder
    ),
    pattern =  cross(extrap_polygons, mpa_polygons),
    format = "file"
  ),

  tar_target(
      cluster_env_extrapolate,
      {
        future::plan(future.callr::callr,
                     workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                     "1")))
        extrapolate_to_env(
          gfbootstrap_combined,
          gfbootstrap_predicted,
          env_domain,
          env_biooracle_names,
          extrap,
          pred_importance_top,
          env_id_col,
          depth_range
        )
      },
      resources = tar_resources(
        crew = tar_resources_crew(controller = "gpu")
      ),
      pattern = cross(
        env_biooracle_names,
        map(
          gfbootstrap_predicted,
          cross(gfbootstrap_combined, res_clust_target)
        )
      ),
  ),



  tar_target(
      cluster_env_assign_cluster,
      {
        future::plan(future.callr::callr,
                     workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                     "1")))
        assign_new_sites_to_cluster(
          cluster_env_extrapolate,
          gfbootstrap_cluster,
          env_biooracle_names,
          env_domain,
          env_id_col,
          spatial_vars
        )
      },
      resources = tar_resources(
        crew = tar_resources_crew(controller = "ram")
      ),
      pattern = map(
        cross(env_biooracle_names, gfbootstrap_cluster),
        cross(cluster_env_extrapolate, clust_methods_target)
      )
  ),

  tar_target(
      extrap_polygons,
      cluster_raster_to_polygons(
          cluster_env_assign_cluster,
          spatial_vars
      ),
      pattern = map(cluster_env_assign_cluster)
  ),

  tar_target(
      extrap_plotted,
      {
        future::plan(future.callr::callr,
                     workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                                     "1")))
        plot_gfbootstrap(
          gfbootstrap_cluster,
          extrap_polygons,
          gfbootstrap_predicted,
          all_bio_env,
          all_bio_long,
          env_poly,
          spatial_vars,
          marine_map,
          plot_clust_labels,
          plot_description = "extrap",
          plot_sim_mat = FALSE,
          output_folder
        )
      },
      resources = tar_resources(
        crew = tar_resources_crew(controller = "ram")
      ),
      pattern = map(extrap_polygons,
                    cross(env_biooracle_names,
                          map(gfbootstrap_cluster,
                      cross(gfbootstrap_predicted, clust_methods_target)))),
      format = "file"
  ),

  tar_target(
    extrap_confidence_plot,
    plot_confidence(cluster_env_assign_cluster,
                           env_domain,
                           env_poly,
                           marine_map,
                           max_clust_prob_plot,
                           env_id_col,
                           spatial_vars,
                           plot_description = "cluster_confidence",
                           output_folder),
    pattern = map(cluster_env_assign_cluster),
    format = "file"
  ),

  tar_target(
      extrap_overlay_polygons,
      plot_polygon_overlay(
          extrap_polygons,
          env_poly,
          plot_description = "extrap_polygon_overlay",
          output_folder
      ),
      pattern = map(env_poly),
      format = "file"
  )

  ## tar_target(
  ##   indicator_species,
  ##   {
  ##     future::plan(future.callr::callr,
  ##                  workers = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
  ##                                                  "1")))
  ##     find_indicator_species(
  ##       extrap_polygons,
  ##       all_bio_long,
  ##       spatial_vars
  ##     )
  ##   },
  ##   resources = tar_resources(
  ##     crew = tar_resources_crew(controller = "ram")
  ##   ),
  ##   pattern =
  ##       cross(all_bio_long, extrap_polygons)
  ## )

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
