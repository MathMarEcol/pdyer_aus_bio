##essential for _targets.R
library(targets)

## Load functions and globals
## Load all files in the functions folder
source_files = list.files("./functions", full.names = TRUE )
sapply(source_files, source)

## Load global objects
## Usually something living inside data()
source("./functions/params.R")

## Set global R options, mostly clustermq
configure_parallel(default_clustermq = TRUE,
                   future_plan = future.callr::callr)
# options()
print(options())

## Set targets options
tar_option_set(
  ##qs format is smaller and reads/writes faster than RDS. needs qs package.
  format = "qs",
  ## Load libraries here. Faster than library(..) at top of this file.
  packages = c(
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

  track_inputs(), #returns a list of targets

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
      spatial_vars,
      depth_names,
      depth_range
    )
  ),


  tar_target(
    all_bio_long,
    data.table::rbindlist(
                  list(
                 zoo_long,
                 phy_long,
                 bac_long,
                 fish_long
                 ),
                 use.names = TRUE
                )[,
                  tar_group := .GRP,
                  by = c("survey",  "trophic")] %>%
      data.table::setkeyv(spatial_vars),
    iteration = "group"
  ),

  domain_extent_targets(
    mapfile_location,
    map_layer,
    env_bounds
    ),

  tar_target(
    env_domain,
    load_env_domain(
      biooracle_folder,
      env_vars,
      env_modes,
      env_poly[[1]],
      max_depth,
      regrid_res,
      spatial_vars,
      bio_oracle_str_template,
      env_limits_sd,
      env_offset,
      env_id_col
    ),
    pattern = map(env_poly), # maps over box polygon and
    iteration = "list"
  )
)
