##essential for _targets.R
library(targets)

## Load functions and globals
## Load all files in the functions folder
source_files = list.files("./functions", full.names = TRUE )
sapply(source_files, source)

## Load global objects
## Usually something living inside data()
source("./functions/params.R") ##might be redundant, probably sourced above

## Set global R options, mostly clustermq
configure_parallel(default_clustermq = TRUE,
                   future_plan = future.callr::callr)
# options()

## Set targets options
tar_option_set(
  ##qs format is smaller and reads/writes faster than RDS. needs qs package.
  format = "qs",
  ## Load libraries here. Faster than library(..) at top of this file.
  packages = c(
    "gfbootstrap",
    "gradientForest",
    "qs",
    "sf",
    "terra",
    "raster",
    "stars",
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
    "ClimateOperators",
    "sdmpredictors",
    "data.table",
    "tidyverse",
    "purrr",
    "lutz", # needed for loading phy and zoo
    "lubridate", # needed for loading phy and zoo
    "rfishbase"
  ),
  ## Debug breaking functions. Otherwise, comment
  ## debug = "zoo_long",
  error = "workspace",
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
  track_inputs(), #returns a list of targets
  ##
  ## Load in environmental data and domain
  domain_extent_targets(
    mapfile_location,
    map_layer,
    env_bounds
    ),

  tar_target(
    env_domain,
    load_env_domain(
      biooracle_folder,
      env_biooracle_names,
      env_poly,
      max_depth,
      regrid_resolution,
      spatial_vars,
      bio_oracle_str_template,
      env_limits_sd,
      env_offset,
      env_id_col
    ),
    pattern = map(env_poly), # maps over box polygon and
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
    ),
    deployment = "worker",
    storage = "worker",
    retrieval = "worker",
    garbage_collection = TRUE,
    memory = "transient"
  ),

  tar_target(
    fish_long,
    load_fish_long(
      fish_taxon_file,
      fish_data_dir,
      fish_years,
      spatial_vars,
      depth_names,
      depth_range,
      regrid_resolution,
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
      regrid_res = regrid_resolution,
      env_offset = env_offset,
      agg_fun = mean,
      spatial_vars = spatial_vars
    ),
    deployment = "worker",
    storage = "worker",
    retrieval = "worker",
    garbage_collection = TRUE,
    memory = "transient",
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
      ),
    pattern = cross(env_domain, all_bio_long),
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
    )
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
    )
)
