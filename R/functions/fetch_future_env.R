#' Fetch future environmental conditions
#'
#' Returns deltas for 2050 and 2100 under each
#' SSP, ready for adding to baseline levels
#' in 2005-2015.
#'
#' The user provides a lot of information
#' then this function fetches a subset,
#' reprojects it onto a rectangular lat lon
#' grid, and calculates summaries to match
#' bioORACLE.
#' Flow
#' 1. Get metadata for datasets (A dataset is a CMIP6 model run)
#' 2. Clean and filter metadata
#'  - drop models without all the scenarios (experiment_id's)
#'  - pick out one variant_label per model.
#'    - If the model creates a new variant for each experiment, just take
#'      a random variant for each experiment
#' 3. Fetch and clean metadata for files (a dataset can split it's output
#'    across multiple files)
#'  - Extract out date ranges for each file
#'  - Add logical cols to show which files are needed for fetching the desired
#'    date range
#' 4. Round out env_bounds to keep coordinates neat
#' 5. Extract data from files
#'    - Estimate subset of grid cells needed to cover bounding box
#'    - Use nco and cdo to download and regrid just the needed parts of
#'      each file
#'      and merge any split datasets into a singe file
#' 6. Calculate statistics
#'  - for each unique combination of CMIP6 model, time point, env variable and
#'    scenario
#'   - long term average
#'   - average annual max
#'   - average annual min
#'   - average annual range
#' 7. Calculate ensemble means
#'  - within each combination of time point, env variable and scenario
#' 8. Calculate deltas from ensembles
#'
#' ncks (from ndo) subsets both time and space over opendap, while cdo subsets
#' one or the other, then does the second subset on a local copy, resulting in
#' a larger download.
#'
#' benchmarks, cdo sellatlonbox first, ~180 seconds
#' cdo seltimestep first, ~30 seconds
#' ncks combined, ~3 seconds
#'
#' cdo makes regridding simpler, so using cdo on the local dataset.
#' nco (and cdo) can use date ranges, and calculate the appropriate slices
#' from dates.
#' It is just as fast (~3 seconds to ~4 seconds, could be internet issues),
#' safer and clearer to specify date ranges rather than calculating date slices
#' myself.
#' Another reason to use dates directly is that ncremap will use dates
#' to merge relevant parts of multiple files.
#'
#' These params should be set for each call:
#'   - out_dir
#'   - env_future_vars
#'   - env_future_scenarios
#'   - env_bounds
#'   - env_offset
#'   - regrid_resolution
#'   - target_hist_start
#'   - target_hist_end
#'   - target_2050_start
#'   - target_2050_end
#'   - target_2100_start
#'   - target_2100_end
#'
#' For other params, use the defaults.
#'
#' @param out_dir Character string of path to save files. For safety, dir is not
#'   created if it does not exist and fetch_future_env will fail. out_dir
#'   should either be an absolute path or start with ./ (.\ on windows), any
#'   other setup may not work
#' @param env_future_vars Character vector of ESGF CMIP6 variable_id's. These
#'   variables will be fetched from ESGF to create deltas.
#' @param env_future_scenarios Character vector of ESGF CMIP6 experiment_id's. A
#'   delta for each of 2050 and 2100 will be created for each future scenario.
#' @param env_bounds List, gives lat lon bounding box for the region of
#'   interest. See default for the expected list structure.
#' @param env_offset Numeric, offsets added to lat and lon when regridding to a
#'   standard rectangular grid. Currently the same offset is applied to both
#'   directions.
#' @param regrid_resolution Numeric, size of grid cells in degrees. I recommend
#'   using 1/N format, to reduce issues with numeric precision elsewhere.
#' @param target_hist_start in format YYYYmm, indicates the start date for the
#'   historical time range.
#' @param target_hist_end in format YYYYmm, indicates the end date for the
#'   historical time range.
#' @param target_2050_start Character in format YYYYmm, indicates the start
#'   dates for the 2050 time range.
#' @param target_2050_end in format YYYYmm, indicates the end date for the 2050
#'   time range.
#' @param target_2100_start Character in format YYYYmm, indicates the start date
#'   for the 2100 time range.
#' @param target_2100_end Character in format YYYYmm, indicates the end date for
#'   the 2100 time range.
#'
#'
#' Use the defaults for these variables
#' @param grid_des_file (USE DEFAULT) Character, name of automatically generated
#'   grid description file
#' @param env_hist_scenarios Character vector, ESGF CMIP6 experiment_id's, for
#'   historical baseline data.  Code currently assumes "historical" in places
#'   though, other values will fail
#' @param node_thredds_blacklist Character vector, some ESGF nodes have not
#'   enabled OpenDAP. Nodes listed here are exclued from the set of download
#'   nodes.
#' @param lat_options Character vector, set of names used for longitude and
#'   latitude in CMIP6 netcdf files. Found by trial and error
#' @param lon_options Character vector, set of names used for longitude and
#'   latitude in CMIP6 netcdf files. Found by trial and error
#' @param realm_l Character vector, ESGF CMIP6 realms to use.  Code makes
#'   assumptions that each dataset only has one value of realm
#' @param freq_l Character vector, ESGF CMIP6 time frequencies to use. Code
#'   makes assumptions that each dataset only has one value of frequency
#' @param grid_l Character vector, ESGF CMIP6 grid resolution to use. Code makes
#'   assumptions that each dataset only has one value of grid
#' @param use_facets Character vector, ESGF CMIP6 facets to search over. The
#'   default facets somehow allow access to other data nodes. Other values seem
#'   to fail to produce reliable results, "facets=*" does not, and the set of
#'   facets in use_fields also does not
#' @param use_fields Character vector, ESGF CMIP6 facets to return from search.
#'   Code assumes certain fileds are present in the return value, changing this
#'   may fail.
#' @param shards Character vector. ESGF is a federation of nodes. These nodes
#'   are queried for datasets and file metadata. Other nodes may be used to
#'   download data. See node_thredds_blacklist




## Quick access to the needed packages
## Will not load in scripts or automated pipelines
if (interactive) {
  library(lubridate)
  library(data.table)
  library(purrr)
  library(ncdf4)
  library(ClimateOperators)
  library(glue)
  library(jsonlite)
  library(RCurl)
  library(stars)
  library(tmap)
} else {
  import::here(data.table, ":=")
}
fetch_future_env <- function(
                              out_dir = "./outputs",
                              env_future_vars = c("tos"),
                              env_future_scenarios =
                                c("ssp126", "ssp245", "ssp585"),
                              env_bounds = list(
                                x = c(109 + 1 / 24, 163 + 23 / 24),
                                y = c(-47 - 23 / 24, -8 - 1 / 24)
                              ),
                              env_offset = 0,
                              regrid_resolution = 1 / 2,
                              target_hist_start = "200501",
                              target_hist_end = "201412",
                              target_2050_start = "204001",
                              target_2050_end = "205001",
                              target_2100_start = "209001",
                              target_2100_end = "209912",
                              grid_des_file = "grid_des.txt",
                              env_hist_scenarios = c("historical"),
                              node_thredds_blacklist =
                                c("esgf3.dkrz.de",
                                  "aims3.llnl.gov",
                                  "esgf.rcec.sinica.edu.tw"),
                              lat_options = c("nav_lat", "lat", "latitude"),
                              lon_options = c("nav_lon", "lon", "longitude"),
                              realm_l = c("ocean", "ocnBgChem", "ocnBgchem"),
                              freq_l = c("mon", "month"),
                              grid_l = "gn",
                              use_facets = c(
                                "mip_era",
                                "activity_id",
                                "model_cohort",
                                "product",
                                "source_id",
                                "institution_id",
                                "source_type",
                                "nominal_resolution",
                                "experiment_id",
                                "sub_experiment_id",
                                "variant_label",
                                "grid_label",
                                "table_id",
                                "frequency",
                                "realm",
                                "variable_id",
                                "cf_standard_name",
                                "data_node"
                              ),
                              use_fields = c(
                                "url",
                                "experiment_id",
                                "realm",
                                "id",
                                "source_id",
                                "datetime_start",
                                "datetime_stop",
                                "experiment_id",
                                "frequency",
                                "grid_label",
                                "variable_id",
                                "variant_label",

                                "data_node",
                                "project",
                                "institution_id",
                                "activity_id",
                                "table_id",
                                "version",
                                "datetime_start",
                                "datetime_stop"
                              ),
                              shards = c(
                                "esgf-node.llnl.gov",
                                "esgf.nci.org.au",
                                "esgf-data.dkrz.de",
                                "esgf-node.ipsl.upmc.fr",
                                "esg-dn1.nsc.liu.se"
                              )
                              ) {

  stopifnot(file.exists(out_dir))
  ## Code Flow
  ## 1. Get metadata for datasets (A dataset is a CMIP6 model run)
  metadata <- fetch_future_env_fetch_metadata(
    env_future_vars = env_future_vars,
    env_future_scenarios = env_future_scenarios,
    env_hist_scenarios = env_hist_scenarios,
    realm_l = realm_l,
    freq_l = freq_l,
    grid_l = grid_l,
    use_facets = use_facets,
    use_fields = use_fields
  )
  ## TODO check if any models (source_id) have been eliminated

  ## 2. Clean and filter metadata
  ##  - drop models without all the scenarios (experiment_id's)
  ##  - pick out one variant_label per model.
  ##    - If the model creates a new variant for each experiment, just take a
  ##      random variant for each experiment
  metadata_full <- fetch_future_env_dataset_metadata(
    metadata = metadata,
    env_hist_scenarios = env_hist_scenarios,
    env_future_scenarios = env_future_scenarios
  )
  ## 3. Fetch and clean metadata for files (a dataset can split it's output
  ##    across multiple files)
  ##  - Extract out date ranges for each file
  ##  - Add logical cols to show which files are needed for fetching the desired
  ##    date range
  file_data <- fetch_future_env_file_data(
    metadata_full = metadata_full,
    shards = shards,
    node_thredds_blacklist = node_thredds_blacklist,
    target_hist_start = target_hist_start,
    target_hist_end = target_hist_end,
    target_2050_start = target_2050_start,
    target_2050_end = target_2050_end,
    target_2100_start = target_2100_start,
    target_2100_end = target_2100_end
  )

  ## 4. Round out env_bounds to keep coordinates neat
  env_bounds_rounded <- list(
    x = c(
      floor(((env_bounds$x[1]) * regrid_resolution) /
            regrid_resolution + env_offset),
      ceiling(((env_bounds$x[2]) * regrid_resolution) /
              regrid_resolution + env_offset)
    ),
    y = c(
      floor(((env_bounds$y[1]) * regrid_resolution) /
            regrid_resolution + env_offset),
      ceiling(((env_bounds$y[2]) * regrid_resolution) /
              regrid_resolution + env_offset)
    )
  )
  fetch_future_env_grid_des(
    env_bounds_rounded = env_bounds_rounded,
    out_dir = out_dir,
    grid_des_file = grid_des_file
  )



  ## 5. Extract data from files
  ##    - Estimate subset of grid cells needed to cover bounding box
  ##    - Use nco and cdo to download and
  ##       regrid just the needed parts of each file
  ##      and merge any split datasets into a singe file
  ## 6. Calculate statistics
  ##  - for each unique combination of CMIP6 model, time point,
  ##      env variable and scenario
  ##   - long term average
  ##   - average annual max
  ##   - average annual min
  ##   - average annual range
  ##   fetch_future_env_get_nc_files() will both extract
  ##     the subset of data needed, and
  ## generate statistics for each dataset.
  fetch_future_env_get_nc_files(
    out_dir = out_dir,
    metadata_full = metadata_full,
    file_data = file_data,
    env_hist_scenarios = env_hist_scenarios,
    env_future_scenarios = env_future_scenarios,
    lat_options = lat_options,
    lon_options = lon_options
  )


  ## 7. Calculate ensemble means
  ##  - within each combination of time point, env variable and scenario
  pairs <- purrr::cross(list(
    variable_id = unique(metadata_full$variable_id),
    experiment_id = unique(metadata_full$experiment_id),
    target_name = names(target_dates)
  ))
  purrr::walk(pairs, ~ {
    if (any(
      .x$experiment_id %in% env_future_scenarios & .x$target_name %in%
        c("f_2050", "f_2100"),
      .x$experiment_id %in% env_hist_scenarios & .x$target_name %in%
        c("baseline")
    )) {

      ## long term mean, min and max
      ClimateOperators::cdo(glue::glue(
        "-O -ensmean ",
        "'{out_dir}{.Platform$file.sep}{.x$variable_id}_*_",
        "{.x$experiment_id}_{.x$target_name}_long_mean.nc' ",
        "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
        "{.x$experiment_id}_{.x$target_name}_long_mean.nc"
      ))
      ## long term average min max
      ClimateOperators::cdo(glue::glue(
        "-O -ensmean ",
        "'{out_dir}{.Platform$file.sep}{.x$variable_id}_*",
        "_{.x$experiment_id}_{.x$target_name}_avg_yr_min.nc' ",
        "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
        "{.x$experiment_id}_{.x$target_name}_avg_yr_min.nc"
      ))
      ClimateOperators::cdo(glue::glue(
        "-O -ensmean ",
        "'{out_dir}{.Platform$file.sep}{.x$variable_id}_*_",
        "{.x$experiment_id}_{.x$target_name}_avg_yr_max.nc' ",
        "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
        "{.x$experiment_id}_{.x$target_name}_avg_yr_max.nc"
      ))
      ## average range
      ClimateOperators::cdo(glue::glue(
        "-O -ensmean ",
        "'{out_dir}{.Platform$file.sep}{.x$variable_id}_*_",
        "{.x$experiment_id}_{.x$target_name}_avg_yr_range.nc' ",
        "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
        "{.x$experiment_id}_{.x$target_name}_avg_yr_range.nc"
      ))
    }
  })


  ## 8. Calculate deltas from ensembles


  ## walk over all variables and create deltas from baseline to each future year
  ## for each scenario
  ## Assuming for now that all historical experiments use baseline
  pairs <- purrr::cross(list(
                            variable_id = unique(metadata_full$variable_id),
                            experiment_id = env_future_scenarios,
                    historical_exp = env_hist_scenarios,
                    target_name = c("f_2050", "f_2100")
                  ))
  purrr::walk(pairs, ~{
      ## long term mean, min and max
     ClimateOperators::cdo(glue::glue(
       "-O sub {out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$experiment_id}_{.x$target_name}_long_mean.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$historical_exp}_baseline_long_mean.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_delta_{.x$target_name}_",
       "{.x$experiment_id}_{.x$historical_exp}_baseline_long_mean.nc"
     ))
      ## long term average min max
     ClimateOperators::cdo(glue::glue(
       "-O sub {out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$experiment_id}_{.x$target_name}_avg_yr_min.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$historical_exp}_baseline_avg_yr_min.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_delta_{.x$target_name}_",
       "{.x$experiment_id}_{.x$historical_exp}_baseline_avg_yr_min.nc"))
     ClimateOperators::cdo(glue::glue(
       "-O sub {out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$experiment_id}_{.x$target_name}_avg_yr_max.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$historical_exp}_baseline_avg_yr_max.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_delta_{.x$target_name}_",
       "{.x$experiment_id}_{.x$historical_exp}_baseline_avg_yr_max.nc"
     ))
      ## average range
     ClimateOperators::cdo(glue::glue(
       "-O sub {out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$experiment_id}_{.x$target_name}_avg_yr_range.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_ensemble_",
       "{.x$historical_exp}_baseline_avg_yr_range.nc ",
       "{out_dir}{.Platform$file.sep}{.x$variable_id}_delta_{.x$target_name}_",
       "{.x$experiment_id}_{.x$historical_exp}_baseline_avg_yr_range.nc"
     ))
      })






}

## Useful for quickly plotting deltas for a variable to check
test_fetch_future_env <- function(variable_id = "tos") {
  files_plot <-
    list.files(path = ".", pattern = glue::glue("{variable_id}_delta_*"))
  purrr::walk(files_plot, ~ {
    pl <- stars::read_stars(.x)
    qin <- tmap::qtm(pl)
    tmap::tmap_save(qin, file.path(
      out_dir,
      glue::glue("{.x}.png")
    ))
  })
}





fetch_future_env_fetch_metadata <- function(env_future_vars,
                                            env_future_scenarios,
                                            env_hist_scenarios,
                                            realm_l,
                                            freq_l,
                                            grid_l,
                                            use_facets,
                                            use_fields) {
  search_vars <- paste(env_future_vars, collapse = "%2C")
  search_scenarios <- paste(c(env_future_scenarios, env_hist_scenarios),
    collapse = "%2C"
  )
  search_realm <- paste(realm_l, collapse = "%2C")
  search_freq <- paste(freq_l, collapse = "%2C")
  search_grid <- paste(grid_l, collapse = "%2C")
  return_facets <- paste(use_facets,
    collapse = "%2C"
  )
  return_fields <- paste(use_fields,
    collapse = "%2C"
  )
  query_str <- glue::glue(
    "https://{shards}/esg-search/search/?offset=0&limit=900&",
    "type=Dataset&latest=true&variable_id={search_vars}&distrib=true&",
    "realm={search_realm}&project=CMIP6&frequency={search_freq}&",
    "experiment_id={search_scenarios}&grid_label={search_grid}&",
    "fields=*&facets={return_facets}&",
    "format=application%2Fsolr%2Bjson"
  )
  metadata <- data.table::rbindlist(lapply(query_str, function(qr) {
    tryCatch({
        ESGF_dataset_query <- jsonlite::fromJSON(qr)
        return(data.table::as.data.table(ESGF_dataset_query$response$docs))
      },
      error = function(e) {
        warning(qr)
        return(NULL)
      }
    )
  }), fill = TRUE)


  drop_cols <- purrr::map(names(metadata), ~ {
    flat_col <- unlist(metadata[[.x]])
    if (length(flat_col) == nrow(metadata)) {
      data.table::set(metadata, i = NULL, .x, flat_col)
      return(NULL)
    } else {
      message(glue::glue(
                      "Dropping col {.x} from metadata, uneven vector produced"
                    ))
      return(.x)
    }
  })
  ## unlist will drop NULLs
  drop_cols <- unlist(drop_cols)
  metadata[, (drop_cols) := NULL]
  metadata <- unique(metadata)
  return(metadata)
}

fetch_future_env_dataset_metadata <- function(metadata,
                                              env_hist_scenarios,
                                              env_future_scenarios) {
  metadata_by_model_variant <- metadata[,
    .(
      all_scenarios =
        all(c(env_hist_scenarios, env_future_scenarios) %in%
          .SD[, experiment_id])
    ),
    by = list(source_id, variable_id, variant_label)
  ]

  metadata_by_model_variant[
    ,
    variant_label_numeric :=
      as.numeric(gsub("[^[:digit:]]", "", variant_label))
  ]
  first_full_variant <- metadata_by_model_variant[
    all_scenarios == TRUE,
    .I[which.min(.SD$variant_label_numeric)],
    by = c("source_id", "variable_id")
  ]$V1


  ## Need all the following variables
  metadata_cols <- c(
    ## "data_node", ## dont filter by data_node at the dataset level
    "source_id",
    "variant_label",
    "variable_id",
    "project",
    "activity_id",
    "institution_id",
    "experiment_id",
    "table_id",
    "grid_label",
    "version",
    "grid_label",
    ## "replica", ## only affects data_node, but adds duplicates
    "master_id",
    "instance_id"
    # "datetime_start", ## creating duplicates
    # "datetime_stop"
  )
  metadata_full_variants <- metadata[
    metadata_by_model_variant[first_full_variant],
    on = c("source_id", "variable_id", "variant_label"),
    nomatch = NULL,
    ..metadata_cols
  ]
  metadata_full_variants <- unique(metadata_full_variants)

  all_models <- unique(metadata$source_id)
  dropped_models <- setdiff(all_models, metadata_full_variants$source_id)

  ## Ideally, all scenarios come from the same variant. Some models create a new
  ## variant for each scenario though.
  ## Just pick the first variant for each scenario
  metadata_odd_variants <- metadata[source_id %in% dropped_models,
    .(
      experiments = list(unique(.SD$experiment_id)),
      has_all = all(c(env_future_scenarios, env_hist_scenarios) %in%
      .SD$experiment_id),
      data_nodes = list(unique(.SD$data_node)),
      variants = list(unique(.SD$variant_label))
    ),
    keyby = list(source_id, variable_id)
  ]

  metadata_random_variant <- metadata[
    source_id %in% metadata_odd_variants[has_all == TRUE]$source_id,
    .I[1], # row number of the first variant in metadata
    keyby = list(source_id, variable_id, experiment_id)
  ]$V1

  metadata_random_full <- metadata[
    metadata_random_variant,
    ..metadata_cols
  ]
  metadata_random_full <- unique(metadata_random_full)
  metadata_full <- rbind(metadata_random_full, metadata_full_variants)

  return(metadata_full)
}


  ## helper functions for fetch_future_env_file_data
  ## Assumes a string in YYYYMM format
  fetch_future_env_as_date_set_day <- function(d) {
    as.Date(glue::glue("{d}01"), "%Y%m%d")
  }
  fetch_future_env_get_start_date <- function(x) {
    date_pos <- regexpr("\\d+-\\d+.nc$", x)
    fetch_future_env_as_date_set_day(substr(x, date_pos, date_pos + 5))
  }

  fetch_future_env_get_end_date <- function(x) {
    date_pos <- regexpr("\\d+.nc$", x)
    fetch_future_env_as_date_set_day(substr(x, date_pos, date_pos + 5))
  }

  fetch_future_env_file_data <- function(metadata_full = metadata_full,
                                         shards = shards,
                                         node_thredds_blacklist =
                                           node_thredds_blacklist,
                                         target_hist_start = target_hist_start,
                                         target_hist_end = target_hist_end,
                                         target_2050_start = target_2050_start,
                                         target_2050_end = target_2050_end,
                                         target_2100_start = target_2100_start,
                                         target_2100_end = target_2100_end) {
    metadata_full[, r_id := seq_len(nrow(metadata_full))]
    file_data <- purrr::pmap_dfr(
      metadata_full[data.table::CJ(shards,
        r_id = seq_len(nrow(metadata_full)),
        sorted = FALSE
      ),
      on = "r_id"
      ],
      function(...) {
        .x <- data.frame(...)

        query_str <- glue::glue(
          "https://{.x$shards}/esg-search/search/",
          "?offset=0&limit=2000&type=File&latest=true&distrib=true&",
          "retracted=false&",
          "source_id={.x$source_id}&",
          "experiment_id={.x$experiment_id}&",
          "variant_label={.x$variant_label}&",
          "variable_id={.x$variable_id}&",
          "table_id={.x$table_id}&",
          "grid_label={.x$grid_label}&",
          "format=application%2Fsolr%2Bjson"
        )

        ret <- tryCatch({
            ESGF_file_query <- jsonlite::fromJSON(query_str)
            ret <- data.table::as.data.table(ESGF_file_query$response$docs)
          },
          error = function(e) {
            warning("Query failed")
            return(NULL)
          }
        )
        message(glue::glue("Fetched {nrow(ret)} file records for dataset ",
        "{.x$master_id} from {length(unique(ret$data_node))} data nodes via ",
        "shard {.x$shards}"))
        return(ret)
      }
    )

    ## Test servers to see which servers are responsive
    ## Timeout =3 gave 2 live servers, timeout = 10 gave ~9
    nodes_active <- unique(file_data$data_node)[
      sapply(unique(file_data$data_node),
             function(url) {
               RCurl::url.exists(url, timeout = 10)
             }
             )
    ]
    nodes_whitelist <- setdiff(nodes_active, node_thredds_blacklist)
    file_data <- file_data[data_node %in% nodes_whitelist]

    data.table::set(file_data,
      i = NULL, "url_opendap",
      gsub(
        ".html\\|application/opendap-html\\|OPENDAP", "",
        unlist(lapply(file_data$url, function(url) {
          unlist(url[grepl("OPENDAP", url)])[1]
        }))
      )
    )

    # Remove list cols that cannot be converted to a vector
    ## Convert all other cols to vectors
    drop_cols <- purrr::map(names(file_data), ~ {
      flat_col <- unlist(file_data[[.x]])
      if (length(flat_col) == nrow(file_data)) {
        data.table::set(file_data, i = NULL, .x, flat_col)
        return(NULL)
      } else {
        message(glue::glue("Dropping col {.x} from file_data, ",
                           "uneven vector produced"))
        return(.x)
      }
    })
    ## unlist will drop NULLs
    drop_cols <- unlist(drop_cols)
    file_data[, (drop_cols) := NULL]
    file_data <- unique(file_data)

    target_dates <- list(
      baseline = list(
        start = fetch_future_env_as_date_set_day(target_hist_start),
        end = fetch_future_env_as_date_set_day(target_hist_end)
      ),
      f_2050 = list(
        start = fetch_future_env_as_date_set_day(target_2050_start),
        end = fetch_future_env_as_date_set_day(target_2050_end)
      ),
      f_2100 = list(
        start = fetch_future_env_as_date_set_day(target_2100_start),
        end = fetch_future_env_as_date_set_day(target_2100_end)
      )
    )

    for (target_yr in names(target_dates)) {
      target_dates[[target_yr]]$interval <-
        lubridate::interval(
                     target_dates[[target_yr]]$start,
                     target_dates[[target_yr]]$end
                   )
    }

    file_data[, `:=`(
      start_date = get_start_date(url_opendap),
      end_date = get_end_date(url_opendap)
    )]

    for (target_yr in names(target_dates)) {
      data.table::set(file_data,
        i = NULL, glue::glue("any_{target_yr}"),
        lubridate::int_overlaps(
          lubridate::interval(
            file_data$start_date,
            file_data$end_date
          ),
          target_dates[[target_yr]]$interval
        )
      )
    }

    ## NCI does not have all the files for some models
    ## Get a file from:
    ##  1. NCI if available
    ##  2. First node if NCI not available
    ## Assuming that there is only one set of files for a run
    ## and that  each node chooses to either host or not host it.
    ## Therefore for each file, choose one node
    keep_files <- file_data[,
      if ("esgf.nci.org.au" %in% .SD$data_node) {
        .I[.SD$data_node == "esgf.nci.org.au"]
      } else {
        .I[1]
      },
      by = title
    ]
    file_data <- file_data[keep_files$V1]

    file_data[, dataset_id_stripped :=
                  gsub("\\.v[0-9]{8}\\|.*", "", dataset_id)]
    dataset_gaps <- setdiff(
      unique(metadata_full$master_id),
      unique(file_data$dataset_id_stripped)
    )
    if (length(dataset_gaps) > 0) {
      stop(glue::glue("Problems accessing file records for ",
                      "datasets: {dataset_gaps}"))
    }
  }

fetch_future_env_grid_des <- function(env_bounds_rounded,
                                      out_dir,
                                      grid_des_file) {
  grid_desc <- glue::glue(
    "gridtype = lonlat\n",
    "xfirst = {env_bounds_rounded$x[1]}\n",
    "xinc = {regrid_resolution}\n",
    "xsize = {(env_bounds_rounded$x[2] - ",
    "env_bounds_rounded$x[1]) / regrid_resolution}\n",
    "yfirst = {env_bounds_rounded$y[1]}\n",
    "yinc = {regrid_resolution}\n",
    "ysize = {(env_bounds_rounded$y[2] - ",
    "env_bounds_rounded$y[1]) / regrid_resolution}\n"
  )

  file_conn <- file(file.path(out_dir, grid_des_file))
  writeLines(grid_desc, file_conn)
  close(file_conn)
}

  ## helper functions for fetch_future_env_get_nc_files
  fetch_future_env_format_with_decimal <- function(num) {
    ret <- as.character(num)
    if (grepl("\\.", ret)) {
      return(ret)
    } else {
      return(format(num, nsmall = 1))
    }
  }
  format_yrmon <- function(d) {
    format(d, "%Y%m")
  }
  fetch_future_env_extract_rect <- function(.x,
                                            .y,
                                            x_dim_name,
                                            y_dim_name,
                                            env_bounds_rounded,
                                            out_dir,
                                            file_data_target,
                                            grid_des_file,
                                            target_dates) {
    ncrcat_retry(glue::glue(
      "-R -O -d time,{target_dates[[.x]]$start},{target_dates[[.x]]$end} ",
      "-v {.y$variable_id} ",
      "-d {x_dim_name},",
      "{fetch_future_env_format_with_decimal(env_bounds_rounded$x[1])},",
      "{fetch_future_env_format_with_decimal(env_bounds_rounded$x[2])} ",
      "-d {y_dim_name},",
      "{fetch_future_env_format_with_decimal(env_bounds_rounded$y[1])},",
      "{fetch_future_env_format_with_decimal(env_bounds_rounded$y[2])} ",
      "-l ./  {paste(file_data_target$url_opendap, collapse = \" \")} ",
      "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
      "{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"
    ))

    ClimateOperators::cdo(glue::glue(
      "-sellonlatbox,{env_bounds_rounded$x[1]},{env_bounds_rounded$x[2]},",
      "{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
      "-remapbil,{grid_des_file}, ",
      "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
      "{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc ",
      "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
      "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
    ))


    ## Check date range
    start_date <- as.Date(ClimateOperators::cdo(glue::glue(
      "outputtab,date,nohead -selgridcell,1 -seltimestep,1 ",
      "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
      "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
    )))
    end_date <- as.Date(ClimateOperators::cdo(glue::glue(
      "outputtab,date,nohead -selgridcell,1 -seltimestep,-1 ",
      "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
      "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
    )))

    if (!all(
      format_yrmon(start_date) == format_yrmon(target_dates[[.x]]$start),
      format_yrmon(end_date) == format_yrmon(target_dates[[.x]]$end)
    )) {
      warning(glue::glue(
        "{paste(file_data_target$url_opendap, collapse = \" \")} ",
        "do not cover full date range of {target_dates$interval}"
      ))
    }
  }

         fetch_future_env_extract_unstructured <- function(.x,
                                                           .y,
                                                           x_dim_name,
                                                           y_dim_name,
                                                           env_bounds_rounded,
                                                           out_dir,
                                                           file_data_target,
                                                           grid_des_file,
                                                           grid_cells,
                                                           target_dates) {
           ## 1 dimensional unstructured array
           ## nco offers --auxilliary coordinates option for 1d arrays
           ## however manually pulling indices took ~300s ve ~4800s
           lonlat_indicies <- which(grid_cells)
           ## 100 is a magic number. Smaller values means more -d commands,
           ## which may be slower or even break the CLI call, but loads less
           ## data.
           lonlat_runs <- diff(lonlat_indicies) > 100
           start_cells <- c(TRUE, lonlat_runs)
           end_cells <- c(lonlat_runs, TRUE)

           cells <- paste(glue::glue("-d {x_dim_name},",
           "{lonlat_indicies[start_cells]},{lonlat_indicies[end_cells]}"),
           collapse = " ")

           ncrcat_retry(glue::glue(
             "-R -O -d time,{target_dates[[.x]]$start},",
             "{target_dates[[.x]]$end} -v {.y$variable_id} ",
             "{cells} ",
             "-l ./  {paste(file_data_target$url_opendap, collapse = \" \")} ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"
           ))

           ## remapbil is good and fast over regular grids, but won't accept
           ## unstructured. Using remapcon here, which will take unstructured
           ClimateOperators::cdo(glue::glue(
             "-sellonlatbox,{env_bounds_rounded$x[1]},",
             "{env_bounds_rounded$x[2]},",
             "{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
             "-remapcon,{grid_des_file}, ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
           ))
           ## Check date range
           start_date <- as.Date(ClimateOperators::cdo(glue::glue(
             "outputtab,date,nohead -selgridcell,1 -seltimestep,1  ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_{.y$source_id}_",
             "{.y$experiment_id}_{.x}_raw_grid.nc"
           )))
           end_date <- as.Date(ClimateOperators::cdo(glue::glue(
             "outputtab,date,nohead -selgridcell,1 -seltimestep,-1  ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_{.y$source_id}_",
             "{.y$experiment_id}_{.x}_raw_grid.nc"
           )))
           if (!all(
             format_yrmon(start_date) == format_yrmon(target_dates[[.x]]$start),
             format_yrmon(end_date) == format_yrmon(target_dates[[.x]]$end)
           )) {
             warning(glue::glue(
               "{paste(file_data_target$url_opendap, collapse = \" \")} ",
               "do not cover full date range of {target_dates$interval}"
             ))
           }
         }
         fetch_future_env_extract_curvilinear <- function(.x,
                                                          .y,
                                                          x_dim_name,
                                                          y_dim_name,
                                                          env_bounds_rounded,
                                                          out_dir,
                                                          file_data_target,
                                                          grid_des_file,
                                                          grid_cells,
                                                          target_dates,
                                                          nc_stats) {
           lonlat_indicies <- which(grid_cells, arr.ind = TRUE)
           x_min <- min(lonlat_indicies[, 1]) - 1 ## nco indexes from 0
           if (x_min > 0) x_min <- x_min - 1 # add buffer
           x_max <- max(lonlat_indicies[, 1]) - 1
           if (x_max < nc_stats$dim[[x_dim_name]]$len - 1) x_max <- x_max + 1
           y_min <- min(lonlat_indicies[, 2]) - 1
           if (y_min > 0) y_min <- y_min - 1
           y_max <- max(lonlat_indicies[, 2]) - 1
           if (y_max < nc_stats$dim[[y_dim_name]]$len - 1) y_max <- y_max + 1
           ##
           ## This seems to be the best approach.
           ## all files that contain any part of 2005-2014 (target_hist_start to
           ## target_hist_end( are passed in. -d time,...,... tels ncrcat to
           ## only take time slices in the target range and concatenate them
           ## together. Time slice outside the desired range are not even
           ## downloaded -d i,...,... and -d j,...,... extract just a subset of
           ## space, in whatever grid system the nc file uses. Data outside
           ## those i,j coordinates are not downloaded.  I may get values
           ## outside the bounding box in rectangular lat lon, but that is fine.
           ## The loading is pretty fast, and subsequent operations only work on
           ## the data being used.

           ncrcat_retry(glue::glue(
             "-R -O -d time,{target_dates[[.x]]$start},",
             "{target_dates[[.x]]$end} -v {.y$variable_id} ",
             "-d {x_dim_name},{x_min},{x_max} ",
             "-d {y_dim_name},{y_min},{y_max}  ",
             "-l ./  {paste(file_data_target$url_opendap, collapse = \" \")} ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"
           ))
           ClimateOperators::cdo(glue::glue(
             "-sellonlatbox,{env_bounds_rounded$x[1]},",
             "{env_bounds_rounded$x[2]},",
             "{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
             "-remapbil,{grid_des_file}, ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
           ))
           ## Check date range
           start_date <- as.Date(ClimateOperators::cdo(glue::glue(
             "outputtab,date,nohead -selgridcell,1 -seltimestep,1  ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
           )))
           end_date <- as.Date(ClimateOperators::cdo(glue::glue(
             "outputtab,date,nohead -selgridcell,1 -seltimestep,-1  ",
             "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
             "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
           )))
           if (!all(
             format_yrmon(start_date) == format_yrmon(target_dates[[.x]]$start),
             format_yrmon(end_date) == format_yrmon(target_dates[[.x]]$end)
           )) {
             warning(glue::glue(
               "{paste(file_data_target$url_opendap, collapse = \" \")}",
               "do not cover full date range of {target_dates$interval}"
             ))
           }
         }
         fetch_future_env_get_nc_files <- function(out_dir,
                                                   metadata_full,
                                                   file_data,
                                                   env_hist_scenarios,
                                                   env_future_scenarios,
                                                   lat_options,
                                                   lon_options) {
           ## Assumes grid natural (gn) and monthly data (Omon)
           ## Within each model/variable combination, get historical, and
           ## 2040-2050 + 2090-2100 for each future scenario
           pairs <- purrr::cross(list(
             source_id = unique(metadata_full$source_id),
             variable_id = unique(metadata_full$variable_id),
             experiment_id = unique(metadata_full$experiment_id)
           ))
           nc_open_retry <- purrr::insistently(ncdf4::nc_open,
                                               rate_delay(2, 10),
                                               quiet = FALSE
                                               )
           ncrcat_retry <- purrr::insistently(ClimateOperators::ncrcat,
                                              rate_delay(2, 10),
                                              quiet = FALSE
                                              )
           purrr::walk(pairs, ~ {
             metadata_local <- metadata_full[source_id == .x$source_id &
             variable_id == .x$variable_id & experiment_id == .x$experiment_id]
             file_data_local <- file_data[source_id == .x$source_id &
             variable_id == .x$variable_id & experiment_id == .x$experiment_id]

             ## for historical, we want baseline
             if (.x$experiment_id %in% env_hist_scenarios) {
               target_names <- c("baseline")
             } else {
               # experiment_id is a future scenario. Use f_2050 and f_2100
               target_names <- c("f_2050", "f_2100")
             }
             purrr::walk(target_names, .y = .x, ~ {
               file_data_target <- file_data_local[
                 file_data_local[[glue::glue("any_{.x}")]] == TRUE]
               if (nrow(file_data_target) == 0) {
                 stop(glue::glue("dataset had no files to load: ",
                 "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}. ",
                 "That should not happen."))
               }
               message(file_data_target$url_opendap[1])
               ## assuming all files in a run have the same spatial dims
               nc_stats <- nc_open_retry(file_data_target$url_opendap[1])

               message(file_data_target$url_opendap[1])
               message(file.path(
                 out_dir,
                 glue::glue("{.y$variable_id}_{.y$source_id}_",
                 "{.y$experiment_id}_{.x}_raw.nc")
               ))
               ## Primitive caching, don't download again
               if (!file.exists(file.path(
                 out_dir,
                 glue::glue("{.y$variable_id}_{.y$source_id}_",
                 "{.y$experiment_id}_{.x}_raw.nc")
               ))) {
                 guess_lat <- lat_options[
                   lat_options %in% names(nc_stats$var)][1]
                 guess_lon <- lon_options[
                   lon_options %in% names(nc_stats$var)][1]
                 x_dim_name <- nc_stats$var[[.y$variable_id]]$dim[[1]]$name
                 y_dim_name <- nc_stats$var[[.y$variable_id]]$dim[[2]]$name

                 message(glue::glue("guess_lat: {guess_lat}, ",
                                    "guess_lon: {guess_lon}, ",
                                    "x_dim_name: {x_dim_name}, ",
                                    "y_dim_name: {y_dim_name}"))
                 if (all(x_dim_name %in% lon_options) & is.na(guess_lon)) {
                   ## Rectangular grid
                   is_rect <- TRUE
                 } else {
                   ## Curvilinera or unstructured
                   ## adapted from
### https://publicwiki.deltares.nl/display/OET/OPeNDAP+subsetting+with+R
                   ## The indicies i and j are not in rectangular lat,lon
                   ## format.
                   ## subsetting is not as easy as setting the bounding boxe
                   is_rect <- FALSE
                   grid_x <- ncdf4::ncvar_get(nc_stats, guess_lon)
                   grid_y <- ncdf4::ncvar_get(nc_stats, guess_lat)

                   ## Longitude is periodic, and not all models use 0 to 360.
                   ## modulo can be used to rescale all longitude values into
                   ## the same period. basic modulo %% in R will bring all
                   ## values into the 0 to 360 range
                   ## If your bounding box crosses longitude == 0, see:
### https://en.wikipedia.org/wiki/Modulo_operation#Modulo_with_offset
                   ## for an algorithm to set a different longitude block
                   grid_cells <- (grid_x %% 360) >
                     (env_bounds_rounded$x[1] %% 360) &
                     grid_x %% 360 < env_bounds_rounded$x[2] %% 360 &
                     grid_y > env_bounds_rounded$y[1] &
                     grid_y < env_bounds_rounded$y[2]
                 }

                 if (is_rect) {
                   ## rectangular coordinate system, lat and lon
                   fetch_future_env_extract_rect(
                     .x = .x,
                     .y = .y,
                     x_dim_name = x_dim_name,
                     y_dim_name = y_dim_name,
                     env_bounds_rounded = env_bounds_rounded,
                     out_dir = out_dir,
                     file_data_target = file_data_target,
                     grid_des_file = grid_des_file,
                     target_dates = target_dates,
                   )
                 } else if (y_dim_name == "time") {
                   ## 1 dimensional unstructured array
                   fetch_future_env_extract_unstructured(
                     .x = .x,
                     .y = .y,
                     x_dim_name = x_dim_name,
                     y_dim_name = y_dim_name,
                     env_bounds_rounded = env_bounds_rounded,
                     out_dir = out_dir,
                     file_data_target = file_data_target,
                     grid_des_file = grid_des_file,
                     grid_cells = grid_cells,
                     target_dates = target_dates
                   )
                 } else {
                   ## Curvilinear data

                   fetch_future_env_extract_curvilinear(
                     .x = .x,
                     .y = .y,
                     x_dim_name = x_dim_name,
                     y_dim_name = y_dim_name,
                     env_bounds_rounded = env_bounds_rounded,
                     out_dir = out_dir,
                     file_data_target = file_data_target,
                     grid_des_file = grid_des_file,
                     grid_cells = grid_cells,
                     target_dates = target_dates,
                     nc_stats = nc_stats
                   )
                 }
               }

               ## Regridding is occasionally introducing invalid numbers. Set to
               ## NA and write to file.
               grid_file <- ncdf4::nc_open(
                 glue::glue(
                   "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                   "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"
                 ),
                 write = TRUE
               )
               grid_var <- ncdf4::ncvar_get(grid_file, .y$variable_id)
               grid_var[which(grid_var < -1e10 | grid_var > 1e10)] <- NA
               ncdf4::ncvar_put(grid_file, .y$variable_id, grid_var)
               ncdf4::nc_close(grid_file)
               ## File should exist now, local operations are cheap, run them
               ## every time

               ## Calculate all the statistics
               ## long term mean
               ClimateOperators::cdo(glue::glue(
                 "-timmean ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_long_mean.nc"
               ))
               ## average annual min max
               ClimateOperators::cdo(glue::glue(
                 "-timmean -yearmin ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_avg_yr_min.nc"
               ))
               ClimateOperators::cdo(glue::glue(
                 "-timmean -yearmax ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_avg_yr_max.nc"
               ))
               ## average annual range
               ClimateOperators::cdo(glue::glue(
                 "-timmean -yearrange ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                 "{out_dir}{.Platform$file.sep}{.y$variable_id}_",
                 "{.y$source_id}_{.y$experiment_id}_{.x}_avg_yr_range.nc"
                 ))
             })
           })
         }
