#' Fetch future environmental conditions
#'
#' Returns deltas for 2050 and 2100 under each
#' SSP, ready for adding to baseline levels
#' in 2005-2015.
#'


fetch_future_env <- function(
                             ## Step 1.
                             env_future_vars <- c("tos")
                             env_future_scenarios <- c("ssp126", "ssp245", "ssp585")
                             env_hist_scenarios <- c("historical")
                             node_thredds_blacklist <- c("esgf3.dkrz.de", "aims3.llnl.gov", "esgf.rcec.sinica.edu.tw")

                             ## List? Param?
                             ## Must be in format YYYYMM
    target_hist_start <- "200501"
    target_hist_end <- "201412"
    target_2050_start <- "204001"
    target_2050_end <- "205001"
    target_2100_start <- "209001"
    target_2100_end <- "209912"
    library(lubridate)
    library(data.table)
    library(purrr)
    library(ncdf4)
    library(ClimateOperators)
                             ) {

  ## Project for the day, get this function working!

  ## The user provides a lot of information
  ## then this function fetches a subset,
  ## reprojects it onto a rectangular lat lon
  ## grid, and calculates summaries to match
  ## bioORACLE.

  ## Step 1. Get URLs of thredds
  ## use esgf search
  ## user specifies a list of variables
  ## user specifies a list of scenarioes
  ## realm is assumed ocean
  realm_l <- c("ocean", "ocnBgChem", "ocnBgchem")
  ## frequency is assumed monthly
  freq_l <- c("mon", "month")
  ## always use grid native
  grid_l <- "gn"

  search_vars <- paste(env_future_vars, collapse = "%2C")
  search_scenarios <- paste(c(env_future_scenarios, env_hist_scenarios), collapse = "%2C")
  search_realm <- paste(realm_l, collapse = "%2C")
  search_freq <- paste(freq_l, collapse = "%2C")
  search_grid <- paste(grid_l, collapse = "%2C")
  ## These facets somehow allow access to other data nodes.
  ## facets=* does not, and the list of fields I wanted also does not
  use_facets <- c(
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
  )
  return_facets <- paste(use_facets,
    collapse = "%2C"
  )
  use_fields <- c(
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
    "activity_id",
    "institution_id",
    "table_id",
    "version",
    "datetime_start",
    "datetime_stop"
    )
  return_fields <- paste(use_fields,
                         collapse = "%2C"
                         )
  shards <- c(
#"localhost",
"esgf-node.llnl.gov",
"esgdata.gfdl.noaa.gov",
"esgf.nci.org.au",
"esgf-data.dkrz.de",
"esgf-node.ipsl.upmc.fr",
"esg-dn1.nsc.liu.se"
## "esg.pik-potsdam.de",
## "crd-esgf-drc.ec.gc.ca",
## "esg-cccr.tropmet.res.in",
## "polaris.pknu.ac.kr"
)
##limit=0
##{search_vars}
##{search_sealm}
##search_freq
##search_scenarios
##search grid
##
 query_str <- glue::glue(
                  "https://{shards}/esg-search/search/?offset=0&limit=900&",
                  "type=Dataset&latest=true&variable_id={search_vars}&distrib=true&",
                  "realm={search_realm}&project=CMIP6&frequency={search_freq}&",
                  "experiment_id={search_scenarios}&grid_label={search_grid}&",
                  "fields=*&facets={return_facets}&",
                  "format=application%2Fsolr%2Bjson")
#fields={return_fields}
#facets={return_facets}&
#shards={search_shards}&",
 metadata <- data.table::rbindlist(lapply(query_str, function(qr) {
   tryCatch({
    ESGF_dataset_query <- jsonlite::fromJSON(qr)
    return(data.table::as.data.table(ESGF_dataset_query$response$docs))
    },
    error = function(e) {
      print(qr)
      return(NULL)
      }
    )

    }), fill = TRUE)

## ESGF_file_query <- jsonlite::fromJSON(query_str)


  ## Need to filter down to a single variant label
  ## a unique combination of variable, realm, scenario, freq


##  metadata <-  data.table::as.data.table(ESGF_file_query$response$docs)

  ## convert to vectors, but skip URL which is multi-valued
  for(f in use_fields[c(-1)]) {
    data.table::set(metadata, i = NULL, f, unlist(metadata[[f]]))
  }

  ## Need to find models and variants that have a historical and at least one SSP
  ## Make that all SSPs

    ## metadata_valid_groups <- metadata[,
    ##         if(all(any(metadata[.I,experiment_id] =="historical"),
    ##                             any(metadata[.I,experiment_id] %in% env_future_scenarios))) .SD,
    ##           by = list(source_id, variable_id, variant_label)]
    metadata_valid_groups <- metadata[,
            if(all(c(env_future_scenarios, env_hist_scenarios) %in% .SD[,experiment_id] )) .SD,
              by = list(source_id, variable_id, variant_label)]
  ## Any model/variant combinations without a historical AND future scenario for a variable have been eliminated
  ## TODO check if any models (source_id) have been eliminated
##metadata[source_id == "CMCC-CM2-HR4"]
## I checked two models, It seems like the models eliminated here don't have any ssps

  ## I have a bit of a complex model in my mind
  ## I have a variable, tos, and I need deltas to add to biooracle, for each ssp and 2050, 2100.
  ## becaues cmip models vary a lot, I will take the average deltas
  ## to get the average deltas, I need to pull in a lot of models, and
  ## calculate the deltas per model, then take the average of the deltas
  ## Which models do I pull in?
  ## I need the model to
  ## - have my variable.
  ## - have historical, for baseline to compare with ssps
  ## - have ssps. technically, I only need one ssp from a model
  ## - have 2050 and 2100, which will be true if it has an ssp
  ##
  ## That's all good, but I still get a huge number of runs
  ## The challenge is reducing the number of runs.
  ## I want a variant within a model.
  ## I want the variant with all four experiments, hosted at nci if possible
  ## Currently all surviving variants have all ssps
  ## data node is not important
  ## For a given model,
  ## pick the first variant
  ##

  ## New plan, folding
  ## Timeout =3 gave 2 live servers, timeout = 10 gave ~9
  nodes_active <- unique(metadata$data_node)[sapply(unique(metadata$data_node), function(url) {RCurl::url.exists(url, timeout = 10)})]
  nodes_whitelist <- setdiff( nodes_active,node_thredds_blacklist)


  ## move whitelisting to file level
  ## metadata_whitelist <- metadata[data_node %in% nodes_whitelist]


  metadata_by_model_variant <- metadata[, .(all_scenarios = all(c(env_hist_scenarios, env_future_scenarios) %in% .SD[,experiment_id])),
                                by = list(source_id, variable_id, variant_label)]
metadata_by_model_variant[, variant_label_numeric := as.numeric(gsub("[^[:digit:]]", "", variant_label))]
  first_full_variant <- metadata_by_model_variant[all_scenarios == TRUE, .I[which.min(.SD$variant_label_numeric)], by = c("source_id", "variable_id")]$V1



  metadata_full_variants <- metadata[metadata_by_model_variant[first_full_variant],
                                     on = c("source_id", "variable_id", "variant_label"),
                                     nomatch = NULL,
                                .(
                                  ##data_node, ## dont filter by data_node at the dataset level
                                source_id,
                                variant_label,
                                variable_id,
                                project,
                                activity_id,
                                institution_id,
                                experiment_id,
                                table_id,
                                grid_label,
                                version,
                                grid_label,
                                ## replica, ## only affects data_node, but adds duplicates
                                master_id,
                                instance_id,
                                datetime_start,
                                datetime_stop)
                                ]
  metadata_full_variants <- unique(metadata_full_variants)

  all_models <- unique(metadata$source_id)
  dropped_models <- setdiff(all_models, metadata_full_variants$source_id)
  ## Ideally, all scenarios come from the same variant. Some models create a new variant for each scenario though.
  ## Just pick the first variant for each scenario
  metadata_odd_variants <- metadata[source_id %in% dropped_models,
                                       .(experiments = list(unique(.SD$experiment_id)),
                                         has_all = all(c(env_future_scenarios, env_hist_scenarios) %in% .SD$experiment_id),
                                         data_nodes = list(unique(.SD$data_node)),
                                         variants = list(unique(.SD$variant_label))),
                                    keyby = list(source_id, variable_id)]
  metadata_random_variant <- metadata[source_id %in% metadata_odd_variants[has_all == TRUE]$source_id,
                                          #print(.SD[, .(source_id, variable_id, experiment_id, data_node, variant_label)])
                                      .I[1] #row number of the first variant in metadata
                                      ,
                                      keyby = list(source_id, variable_id, experiment_id)]$V1
  metadata_random_full <- metadata[metadata_random_variant,
  ## Need all the following variables
                                .(
                                  ## data_node, ## don't filter by data_node at the dataset level
                                source_id,
                                variant_label,
                                variable_id,
                                project,
                                activity_id,
                                institution_id,
                                experiment_id,
                                table_id,
                                grid_label,
                                version,
                                grid_label,
                                ## replica, ## only affects data_node, but adds duplicates
                                master_id,
                                instance_id,
                                datetime_start,
                                datetime_stop)
           ]
  metadata_random_full <- unique(metadata_random_full)
  metadata_full <- rbind(metadata_random_full, metadata_full_variants)
  ## metadata_by_data_node <- metadata[, .(all_scenarios = all(c(env_hist_scenarios, env_future_scenarios) %in% .SD[,experiment_id])),
  ##                               by = list(source_id, variable_id, variant_label, data_node)]
  ## metadata_by_data_node_full <- metadata_by_data_node[all_scenarios == TRUE]

  ## metadata_full_variants <- metadata[metadata_by_data_node_full,
  ##                                    on = c("source_id", "variable_id", "variant_label", "data_node"),
  ##                                    nomatch = NULL]

  ## metadata_nci <- metadata_full_variants[
  ##   data_node == "esgf.nci.org.au", .(data_node),
  ## by  = list(source_id, variable_id, variant_label) ] [
  ##   , .SD[1,], by = list(source_id, variable_id) ]

  ## metadata_nci_full <- metadata[metadata_nci, on=names(metadata_nci), nomatch = NULL,
  ##                               .(data_node,
  ##                               source_id,
  ##                               variant_label,
  ##                               variable_id,
  ##                               project,
  ##                               activity_id,
  ##                               institution_id,
  ##                               experiment_id,
  ##                               table_id,
  ##                               grid_label,
  ##                               version,
  ##                               grid_label,
  ##                               replica,
  ##                               master_id,
  ##                               instance_id,
  ##                               datetime_start,
  ##                               datetime_stop)
  ##          ]



##   ##find any missed models, and choose the first variant from any data node
##   ## models_full_no_nci <- setdiff(unique(metadata_full_variants$source_id), metadata_nci_full$source_id)
##   ## empty! Check other models
##   all_models <- unique(metadata$source_id)
##   dropped_models <- setdiff(all_models, metadata_nci_full$source_id)
##   metadata_check_dropped <- metadata[source_id %in% dropped_models,
##                                      .(all_scenarios = all(c(env_hist_scenarios, env_future_scenarios) %in% .SD$experiment_id),
##                                        scenarios = list(unique(.SD$experiment_id))),
##                                 by = list(source_id, variable_id, variant_label)]
## #metadata[source_id == "MPI-ESM1-2-LR"] ## seems to be creating a separate variant for each run,
## #metadata[source_id == "BCC-ESM1"] ## no future scenarios

##   ## Ideally, all scenarios come from the same variant. Some models create a new variant for each scenario though.
##   ## Just pick the first variant for each scenario
##   metadata_odd_variants <- metadata[source_id %in% dropped_models,
##                                        .(experiments = list(unique(.SD$experiment_id)),
##                                          has_all = all(c(env_future_scenarios, env_hist_scenarios) %in% .SD$experiment_id),
##                                          data_nodes = list(unique(.SD$data_node)),
##                                          variants = list(unique(.SD$variant_label))),
##                                     keyby = list(source_id, variable_id)]
##   ## For the last few models, just picking the first variant for each experiement_id
##   metadata_random_variant <- metadata[source_id %in% metadata_odd_variants[has_all == TRUE]$source_id,
##                                           #print(.SD[, .(source_id, variable_id, experiment_id, data_node, variant_label)])
##                                         data.table::fifelse("esgf.nci.org.au" %in% .SD$data_node,
##                                                             .I[.SD[data_node == "esgf.nci.org.au",.I[1]]],
##                                                             .I[1])
##                                       ,
##                                       keyby = list(source_id, variable_id, experiment_id)]
##   metadata_random_full <- metadata[metadata_random_variant$V1,
##   ## Need all the following variables
##                                 .(data_node,
##                                 source_id,
##                                 variant_label,
##                                 variable_id,
##                                 project,
##                                 activity_id,
##                                 institution_id,
##                                 experiment_id,
##                                 table_id,
##                                 grid_label,
##                                 version,
##                                 grid_label,
##                                 replica,
##                                 master_id,
##                                 instance_id,
##                                 datetime_start,
##                                 datetime_stop)
##            ]
##   metadata_full <- rbind(metadata_random_full, metadata_nci_full)




  ## Pick a model and a variable.
  ## Try to get nci, and a variant that has all scenarios.
  ## pick an ssp
  ## If that doesn't exist, go through other data nodes and stop when you find a variant that has all scenarios

##   metadata_valid_groups[, {
## print(.N )
##     } , by= list(source_id, variable_id, variant_label)]

##   metadata_valid_groups[, {
##     if (.N < 4 ) {
##     print(.BY)
##     print(.SD[, c("experiment_id","activity_id")])
##     }
##     } , by= list(source_id, variable_id, variant_label, data_node)]
##   metadata_valid_groups[, {
##     if (.N > 8 ) {
##     print(.BY)
##     print(.SD[, c("data_node","experiment_id","activity_id")])
##     }
##     } , by= list(source_id, variable_id, variant_label)]
##   metadata_valid_groups[, {
##     print(.BY)
##     print(.SD[, c("data_node","experiment_id","activity_id")])

##     } , by= list(source_id, variable_id, variant_label)]



  ## Pick one variant per model/variable
  ## Ideally, pick a variant with
  ##   all historical and ssps,
  ##   hosted at nci
  ## metadata_valid_groups[,
  ## {
  ##   ## I look within a group,
  ##   has_nci <- any(data_node %in% "esgf.nci.org.au")
  ##   if ( has_nci ) {
  ##     has_all_scenarios <- all(c(env_future_scenarios, env_hist_scenarios) %in%
  ##       .SD[data_node == "esgf.nci.org.au", experiment_id])
  ##     if ( has_all_scenarios ) {
  ##         return(.SD[data_node == "esgf.nci.org.au", ])
  ##       } else {




  ##     }


  ##                       {
  ##                     , by = list(source_id, variable_id, variant_label)]

  ## ## Select just the first variant listed for each model.
  ## metadata_variant <- metadata[metadata[ ,.I[1]  , by =list(variable_id, realm, grid_label, experiment_id, source_id, frequency)]$V1]


  ## NCI does not have all the files for some models
  ## Get a file from:
  ##  1. NCI if available
  ##  2. First node if NCI not available
  metadata_full_copy <- data.table::copy(metadata_full)
  metadata_full[, chunks := ceiling(seq.int(1, nrow(metadata_full))/18)]
  file_data <- data.table::rbindlist(lapply(unique(metadata_full$chunks), function(chunk, shards) {
    print(chunk)
    tryCatch({
      all_dataset_id <- glue::glue_collapse(glue::glue_data(metadata_full[chunks == chunk], "{instance_id}|{data_node}"), sep = "%2C")
      query_str <- glue::glue("https://{shards}/esg-search/search?type=File&",
                              "limit=9000&",
                              "dataset_id={all_dataset_id}&",
                              ##"&master_id={master_id}&",#&replica={fifelse(replica, \"true\", \"false\")}&",
                              "format=application%2Fsolr%2Bjson")
      file_data <- data.table::rbindlist(lapply(query_str, function(qr) {
        tryCatch({
          ESGF_file_query <- jsonlite::fromJSON(qr)
          return(data.table::as.data.table(ESGF_file_query$response$docs))
        },
        error = function(e) {
          print(qr)
          return(NULL)
        }
        )

      }), fill = TRUE)
    print(file_data)
   },
   error = function(e) {
     print(e)
     return(NULL)
   }
   )

 }, shards = shards), fill = TRUE)

  backup_file_data<-data.table::copy(file_data)
  file_data<-data.table::copy(backup_file_data)
    target_hist_start <- "200501"
    target_hist_end <- "201412"
    target_2050_start <- "204001"
    target_2050_end <- "205001"
    target_2100_start <- "209001"
    target_2100_end <- "209912"

  data.table::set(file_data, i= NULL, "url_opendap",
                  gsub(".html\\|application/opendap-html\\|OPENDAP", "",
                       unlist(lapply(file_data$url, function(url){
                         unlist(url[grepl("OPENDAP", url)])[1]
                       }))
                    )
                  )

  # Remove list cols that cannot be converted to a vector
  drop_cols <- c("url", "source_type", "directory_format_template_")
  file_data[, (drop_cols) := NULL]
  ## Convert all other cols to vectors
  for(f in names(file_data)) {
      data.table::set(file_data, i = NULL, f, unlist(file_data[[f]]))
  }
  file_data <- unique(file_data)
  ## Assumes a string in YYYYMM format
  as_date_set_day <- function(d){
    as.Date(glue::glue("{d}01"), "%Y%m%d")
  }
    get_start_date <- function(x){
      date_pos <- regexpr("\\d+-\\d+.nc$",x)
      as_date_set_day(substr(x, date_pos, date_pos+5))
      }

    get_end_date <- function(x){
      date_pos <- regexpr("\\d+.nc$",x)
      as_date_set_day(substr(x, date_pos, date_pos+5))
      }


  target_dates <- list(
    baseline = list(
      start = as_date_set_day(target_hist_start),
      end = as_date_set_day(target_hist_end)
      ),
    f_2050 = list(
    start = as_date_set_day(target_2050_start),
    end = as_date_set_day(target_2050_end)
    ),
    f_2100 = list(
    start = as_date_set_day(target_2100_start),
    end = as_date_set_day(target_2100_end)
    )
    )

  for (target_yr in names(target_dates)) {
    target_dates[[target_yr]]$interval <- lubridate::interval(target_dates[[target_yr]]$start, target_dates[[target_yr]]$end)
  }

   ## file_data[,  `:=`(start_date  = get_start_date(instance_id),
   ##                   end_date = get_end_date(instance_id))]
  # TODO ~400 files have no opendap url?!
   file_data[,  `:=`(start_date  = get_start_date(url_opendap),
                     end_date = get_end_date(url_opendap))]
             #date_interval = lubridate::interval(get_start_date(instance_id), get_end_date(instance_id)))]

  ## manually drop bad data_nodes
  ## file_data <- file_data[!is.na(start_date)]

  for(target_yr in names(target_dates)) {
    data.table::set(file_data, i = NULL, glue::glue("any_{target_yr}"),
                    lubridate::int_overlaps(
                                 lubridate::interval(
                                              file_data$start_date,
                                              file_data$end_date
                                            ),
                                 target_dates[[target_yr]]$interval
                               )
                    )
    }

    ## file_data[, `:=`(any_hist = ,
    ##                  any_2050 = lubridate::int_overlaps(lubridate::interval(start_date, end_date), target_2050_interval),
    ##                  any_2100 = lubridate::int_overlaps(lubridate::interval(start_date, end_date), target_2100_interval),
    ##                  all_hist = target_hist_interval %within% lubridate::interval(start_date, end_date),
    ##                  all_2050 = target_2050_interval %within% lubridate::interval(start_date, end_date),
    ##                  all_2100 = target_2100_interval %within% lubridate::interval(start_date, end_date)
    ##                  )]

  ## metadata_full[ , by = chunks, {
  ##   all_dataset_id <- glue::glue_collapse(glue::glue_data(.SD, "{instance_id}|{data_node}"), sep = "%2C")
  ##   query_str <- glue::glue("https://esgdata.gfdl.noaa.gov/esg-search/search?type=File&",
  ##                         "limit=9000&fields=",
  ##                              "dataset_id={all_dataset_id}&",
  ##                              #"&master_id={master_id}&",#&replica={fifelse(replica, \"true\", \"false\")}&",
  ##                              "format=application%2Fsolr%2Bjson")
  ##   #ESGF_file_query <- jsonlite::fromJSON(query_str)
  ##   #file_data <- data.table::as.data.table(ESGF_file_query$response$docs)
  ##   file_data <- data.table(t=1)
  ##   }]
  ## split(metadata_full,  ceiling(seq_along(d)/20))
  ## ## fromJSON stops treating x as a url when it becomes longer than 2048 chars, so need to chunk the requests
  ## all_dataset_id <- glue::glue_collapse(glue::glue_data(metadata_full[1:18,], "{instance_id}|{data_node}"), sep = "%2C")
  ## query_str <- glue::glue("https://esgdata.gfdl.noaa.gov/esg-search/search?type=File&",
  ##                         "limit=9000&",
  ##                              "dataset_id={all_dataset_id}&",
  ##                              #"&master_id={master_id}&",#&replica={fifelse(replica, \"true\", \"false\")}&",
  ##                              "format=application%2Fsolr%2Bjson")
  ##   ESGF_dataset_query <- jsonlite::fromJSON(query_str)
  ## file_data <- data.table::as.data.table(ESGF_dataset_query$response$docs)

  ## ## search for files
  ## query_str <- glue::glue_data(metadata_full, "https://esgdata.gfdl.noaa.gov/esg-search/search?type=File&",
  ##                              "dataset_id={instance_id}|{data_node}&",
  ##                              #"&master_id={master_id}&",#&replica={fifelse(replica, \"true\", \"false\")}&",
  ##                              "format=application%2Fsolr%2Bjson")

  ##   ESGF_dataset_query <- jsonlite::fromJSON(query_str[2])
  ## file_data <- data.table::as.data.table(ESGF_dataset_query$response$docs)
env_bounds = list(
  x = c(109 + 1 / 24, 163 + 23 / 24),
  y = c(-47 - 23 / 24, -8 - 1 / 24)
)
env_offset = 0
##in lat lon degrees, use 1/integer fraction
##for proper rastering later,
##currently 1/12 to allign with BioORACLE
regrid_resolution = 1 / 2 #TODO: 1 / 12,

      env_bounds_rounded <- list(
        x = c(floor(((env_bounds$x[1])*regrid_resolution)/regrid_resolution + env_offset),
              ceiling(((env_bounds$x[2])*regrid_resolution)/regrid_resolution + env_offset)),
        y = c(floor(((env_bounds$y[1])*regrid_resolution)/regrid_resolution + env_offset),
              ceiling(((env_bounds$y[2])*regrid_resolution)/regrid_resolution + env_offset)))
        grid_desc <- glue::glue("gridtype = lonlat\n",
"xfirst = {env_bounds_rounded$x[1]}\n",
"xinc = {regrid_resolution}\n",
"xsize = {(env_bounds_rounded$x[2] -  env_bounds_rounded$x[1]) / regrid_resolution}\n",
"yfirst = {env_bounds_rounded$y[1]}\n",
"yinc = {regrid_resolution}\n",
"ysize = {(env_bounds_rounded$y[2] -  env_bounds_rounded$y[1]) / regrid_resolution}\n")

file_conn<-file("grid_des.txt")
writeLines(grid_desc, file_conn)
close(file_conn)
  ##Within each model/variable combination, get historical, and 2040-2050 + 2090-2100 for each future scenario
  pairs <- purrr::cross(list(
                          source_id = unique(metadata_full$source_id),
                            variable_id = unique(metadata_full$variable_id),
                            experiment_id = unique(metadata_full$experiment_id)
                  ))
  purrr::walk(pairs, ~{
    metadata_local <- metadata_full[source_id == .x$source_id & variable_id == .x$variable_id & experiment_id == .x$experiment_id]
    file_data_local <- file_data[source_id == .x$source_id & variable_id == .x$variable_id & experiment_id == .x$experiment_id]

    ## for historical, we want baseline
    if(.x$experiment_id %in% env_hist_scenarios) {
      target_names <- c("baseline")
    } else {
        # experiment_id is a future scenario. Use f_2050 and f_2100
      target_names <- c("f_2050", "f_2100")
    }
        purrr::walk(target_names, .y = .x, ~ {
      file_data_target <- file_data_local[file_data_local[[glue::glue("any_{.x}")]] == TRUE]
      print(file_data_target$url_opendap[1])
      nc_stats <- ncdf4::nc_open(file_data_target$url_opendap[1]) ## assuming all files in a run have the same spatial dims
## nc_stats<-ncdf4::nc_open(
##      "http://esgf.nci.org.au/thredds/dodsC/replica/CMIP6/CMIP/NCAR/CESM2/historical/r1i1p1f1/Omon/tos/gn/v20190308/tos_Omon_CESM2_historical_r1i1p1f1_gn_185001-201412.nc"
##"http://esgf.nci.org.au/thredds/dodsC/replica/CMIP6/CMIP/BCC/BCC-CSM2-MR/historical/r1i1p1f1/Omon/tos/gn/v20181126/tos_Omon_BCC-CSM2-MR_historical_r1i1p1f1_gn_185001-201412.nc"
##       )
##       print(nc_stats)

      print(file_data_target$url_opendap[1])
      print(glue::glue("{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"))
      if (!file.exists(glue::glue("{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"))) {
      lat_options <- c("nav_lat", "lat", "latitude")
      guess_lat <- lat_options[lat_options %in% names(nc_stats$var)][1]
      lon_options <- c("nav_lon", "lon", "longitude")
      guess_lon <- lon_options[lon_options %in% names(nc_stats$var)][1]
      x_dim_name <- nc_stats$var[[.y$variable_id]]$dim[[1]]$name
      y_dim_name <- nc_stats$var[[.y$variable_id]]$dim[[2]]$name

      print(glue::glue("guess_lat: {guess_lat}, guess_lon: {guess_lon}, x_dim_name: {x_dim_name}, y_dim_name: {y_dim_name}"))
      if(all(x_dim_name %in% lon_options) & is.na(guess_lon)){
        ##Rectangular grid
        is_rect <- TRUE
      } else {
        ## Curvilinera or unstructured
        ## adapted from https://publicwiki.deltares.nl/display/OET/OPeNDAP+subsetting+with+R
        ## The indicies i and j are not in rectangular lat,lon format.
        ## subsetting is not as easy as setting the bounding boxe
        is_rect <- FALSE
        grid_x=ncvar_get(nc_stats,guess_lon)
        grid_y=ncvar_get(nc_stats,guess_lat)

        ## Longitude is periodic, and not all models use 0 to 360.
        ## modulo can be used to rescale all longitude values into the same
        ## period. basic modulo %% in R will bring all values into the 0 to 360 range
        ## If your bounding box crosses longitude == 0, see:
        ## https://en.wikipedia.org/wiki/Modulo_operation#Modulo_with_offset
        ## for an algorithm to set a different longitude block
        grid_cells <- grid_x %% 360 > env_bounds_rounded$x[1] %% 360 & grid_x %% 360 < env_bounds_rounded$x[2] %% 360 &
          grid_y > env_bounds_rounded$y[1] & grid_y < env_bounds_rounded$y[2]
      }

        format_with_decimal <- function(num) {
          ret <- as.character(num)
          if (grepl("\\.", ret)) {
            return(ret)
          } else {
            return(format(num, nsmall = 1))
          }
        }
      if (is_rect) {

        ncrcat(glue::glue("-R -O -d time,{target_dates[[.x]]$start},{target_dates[[.x]]$end} -v {.y$variable_id} ",
                        "-d {x_dim_name},{format_with_decimal(env_bounds_rounded$x[1])},{format_with_decimal(env_bounds_rounded$x[2])} ",
                        "-d {y_dim_name},{format_with_decimal(env_bounds_rounded$y[1])},{format_with_decimal(env_bounds_rounded$y[2])} ",
                        "-l ./  {paste(file_data_target$url_opendap, collapse = \" \")} {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"))

        cdo(glue::glue("-sellonlatbox,{env_bounds_rounded$x[1]},{env_bounds_rounded$x[2]},{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
                       "-remapbil,grid_des.txt, ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"))

        ##Check date range
        start_date <- as.Date(cdo(glue::glue("outputtab,date,nohead -selgridcell,1 -seltimestep,1  {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc")))
        end_date <- as.Date(cdo(glue::glue("outputtab,date,nohead -selgridcell,1 -seltimestep,-1  {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc")))

        format_yrmon <- function(d) {
          format(d, "%Y%m")
        }
        if( !all(format_yrmon(start_date) == format_yrmon(target_dates[[.x]]$start),
            format_yrmon(end_date) == format_yrmon(target_dates[[.x]]$end))) {
          print(glue::glue("{paste(file_data_target$url_opendap, collapse = \" \")} do not cover full date range of {target_dates$interval}"))
        }



       } else if (y_dim_name == "time") {

         ## 1 dimensional unstructured array
         ## nco offers --auxilliary coordinates option for 1d arrays
         ## however manually pulling indices took ~300s ve ~4800s
      lonlat_indicies <- which(grid_cells)
         ## 100 is a magic number. Smaller values means more -d commands, which may be slower or even break the CLI call, but loads less data.
         lonlat_runs <- diff(lonlat_indicies) > 100
         start_cells <- c(TRUE, lonlat_runs)
         end_cells <- c(lonlat_runs, TRUE)

         cells <- paste(glue::glue("-d {x_dim_name},{lonlat_indicies[start_cells]},{lonlat_indicies[end_cells]}"), collapse = " ")

        ncrcat(glue::glue("-R -O -d time,{target_dates[[.x]]$start},{target_dates[[.x]]$end} -v {.y$variable_id} ",
                        "{cells} ",
                        "-l ./  {paste(file_data_target$url_opendap, collapse = \" \")} {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"))

         ##remapbil is good and fast over regular grids, but won't accept unstructured. Using remapcon here, which will take unstructured
        cdo(glue::glue("-sellonlatbox,{env_bounds_rounded$x[1]},{env_bounds_rounded$x[2]},{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
                       "-remapcon,grid_des.txt, ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"))
        ##Check date range
        start_date <- as.Date(cdo(glue::glue("outputtab,date,nohead -selgridcell,1 -seltimestep,1  {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc")))
        end_date <- as.Date(cdo(glue::glue("outputtab,date,nohead -selgridcell,1 -seltimestep,-1  {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc")))
        format_yrmon <- function(d) {
          format(d, "%Y%m")
        }
        if( !all(format_yrmon(start_date) == format_yrmon(target_dates[[.x]]$start),
            format_yrmon(end_date) == format_yrmon(target_dates[[.x]]$end))) {
          print(glue::glue("{paste(file_data_target$url_opendap, collapse = \" \")} do not cover full date range of {target_dates$interval}"))
        }
       } else {
         ## Curvilinear data


      lonlat_indicies <- which(grid_cells, arr.ind=TRUE)
      x_min <- min(lonlat_indicies[,1]) -1 ## nco indexes from 0
      if (x_min > 0) x_min <- x_min - 1 #add buffer
      x_max <- max(lonlat_indicies[,1]) -1
      if (x_max < nc_stats$dim[[x_dim_name]]$len - 1) x_max <- x_max + 1
      y_min <- min(lonlat_indicies[,2]) -1
      if (y_min > 0) y_min <- y_min- 1
      y_max <- max(lonlat_indicies[,2]) -1
      if (y_max < nc_stats$dim[[y_dim_name]]$len - 1) y_max <- y_max + 1
      ##
      ## This seems to be the best approach.
      ## all files that contain any part of 2005-2014 (target_hist_start to target_hist_end(
      ## are passed in. -d time,...,... tels ncrcat to only take time slices in the target range and
      ## concatenate them together. Time slice outside the desired range are not even downloaded
      ## -d i,...,... and -d j,...,... extract just a subset of space, in whatever grid system
      ## the nc file uses. Data outside those i,j coordinates are not downloaded.
      ## I may get values outside the bounding box in rectangular lat lon, but that is fine.
      ## The loading is pretty fast, and subsequent operations only work on the data being used.

      ## Primitive caching, don't download again
        ncrcat(glue::glue("-R -O -d time,{target_dates[[.x]]$start},{target_dates[[.x]]$end} -v {.y$variable_id} ",
                        "-d {x_dim_name},{x_min},{x_max} -d {y_dim_name},{y_min},{y_max}  ",
                        "-l ./  {paste(file_data_target$url_opendap, collapse = \" \")} {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc"))
        cdo(glue::glue("-sellonlatbox,{env_bounds_rounded$x[1]},{env_bounds_rounded$x[2]},{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
                       "-remapbil,grid_des.txt, ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw.nc {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc"))
        ##Check date range
        start_date <- as.Date(cdo(glue::glue("outputtab,date,nohead -selgridcell,1 -seltimestep,1  {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc")))
        end_date <- as.Date(cdo(glue::glue("outputtab,date,nohead -selgridcell,1 -seltimestep,-1  {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc")))
        format_yrmon <- function(d) {
          format(d, "%Y%m")
        }
        if( !all(format_yrmon(start_date) == format_yrmon(target_dates[[.x]]$start),
            format_yrmon(end_date) == format_yrmon(target_dates[[.x]]$end))) {
          print(glue::glue("{paste(file_data_target$url_opendap, collapse = \" \")} do not cover full date range of {target_dates$interval}"))
        }
      }
      }
      ## File should exist now, local operations are cheap, run them anyway
        ## cdo(glue::glue("-remapbil,grid_des.txt, -sellonlatbox,{env_bounds_rounded$x[1]},{env_bounds_rounded$x[2]},{env_bounds_rounded$y[1]},{env_bounds_rounded$y[2]}, ",
        ##              "{variable_id}_{source_id}_{experiment_id}_raw.nc {variable_id}_{source_id}_{experiment_id}_raw_grid.nc"))

      ## Now calculate all the statistics
      ## long term mean, min and max
      cdo(glue::glue("-timmean {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_long_mean.nc"))
      ## long term average min max
      cdo(glue::glue("-timmean -yearmin {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_avg_yr_min.nc"))
      cdo(glue::glue("-timmean -yearmax {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_avg_yr_max.nc"))
      ## average range
      cdo(glue::glue("-timmean -yearrange {.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_raw_grid.nc ",
                     "{.y$variable_id}_{.y$source_id}_{.y$experiment_id}_{.x}_avg_yr_range.nc"))

        })

    })


    ## Now walk over each variable experiment stat combo and calculate the ensemble means
  pairs <- purrr::cross(list(
                            variable_id = unique(metadata_full$variable_id),
                            experiment_id = unique(metadata_full$experiment_id)
                    target_name <- names(target_dates)
                  ))
  purrr::walk(pairs, ~{

      ## long term mean, min and max
      cdo(glue::glue("ensmean {.x$variable_id}_*_{.x$experiment_id}_{.x$target_name}_long_mean.nc ",
                     "{.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_long_mean.nc"))
      ## long term average min max
      cdo(glue::glue("ensmean {.x$variable_id}_*_{.x$experiment_id}_{.x$target_name}_avg_yr_min.nc ",
                     "{.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_avg_yr_min.nc"))
      cdo(glue::glue("ensmean {.x$variable_id}_*_{.x$experiment_id}_{.x$target_name}_avg_yr_max.nc ",
                     "{.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_avg_yr_max.nc"))
      ## average range
      cdo(glue::glue("ensmean {.x$variable_id}_*_{.x$experiment_id}_{.x$target_name}_avg_yr_range.nc ",
                     "{.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_avg_yr_range.nc"))
      })
  ## Finally, walk over all variables and create deltas from baseline to each future year for each scenario
  ## Assuming for now that all historical experiments use baseline
  pairs <- purrr::cross(list(
                            variable_id = unique(metadata_full$variable_id),
                            experiment_id = env_future_scenarios,
                    historical_exp <- env_hist_scenarios,
                    target_name <- c("f_2050", "f_2100")
                  ))
  purrr:walk(unique(metadata_full$variable_id, ~{
      ## long term mean, min and max
      cdo(glue::glue("sub {.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_long_mean.nc ",
                     "{.x$variable_id}_ensemble_{.x$historical_exp}_baseline_long_mean.nc ",
                     "{.x$variable_id}_delta_{.x$target_name}_{.x$experiment_id}_{.x$historical_exp}_baseline_long_mean.nc"))
      ## long term average min max
      cdo(glue::glue("sub {.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_avg_yr_min.nc ",
                     "{.x$variable_id}_ensemble_{.x$historical_exp}_baseline_avg_yr_min.nc ",
                     "{.x$variable_id}_delta_{.x$target_name}_{.x$experiment_id}_{.x$historical_exp}_baseline_avg_yr_min.nc"))
      cdo(glue::glue("sub {.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_avg_yr_max.nc ",
                     "{.x$variable_id}_ensemble_{.x$historical_exp}_baseline_avg_yr_max.nc ",
                     "{.x$variable_id}_delta_{.x$target_name}_{.x$experiment_id}_{.x$historical_exp}_baseline_avg_yr_max.nc"))
      ## average range
      cdo(glue::glue("sub {.x$variable_id}_ensemble_{.x$experiment_id}_{.x$target_name}_avg_yr_range.nc ",
                     "{.x$variable_id}_ensemble_{.x$historical_exp}_baseline_avg_yr_range.nc ",
                     "{.x$variable_id}_delta_{.x$target_name}_{.x$experiment_id}_{.x$historical_exp}_baseline_avg_yr_range.nc"))
  })
    ## for all future, we want f_2050 and f_2100

  ##   purrr::map(c(env_hist_, ~{
  ##     ## if (nrow(file_data_target) >= 1) {

  ##       # need experiments somewher here

  ##     ## find slices

  ##     ## start_slice <- lubridate::interval(file_data_hist_all$start_date, target_hist_start) %/% months(1) #nco indexes from 0
  ##     ## end_slice <- start_slice +  target_hist_interval %/% months(1)


  ##     ## best way to summarize a netcdf file
  ##     nc_file <- ncdf4::nc_open(file_data_hist$url_opendap[1])
  ##     ## valid_slices <- start_slice < nc_file$dim$time$len &
  ##     ##   end_slice <= nc_file$dim$time$len &
  ##     ##   start_slice >= 1 &
  ##     ##   end_slice >= 2
  ##     ## if (!valid_slices) {
  ##     ##   print(glue::glue("Issue with {file_data_hist_all$url_opendap}"))
  ##     ## }


  ##     grid_x=ncvar_get(nc_file,"longitude")
  ##     grid_y=ncvar_get(nc_file,"latitude")

  ##     grid_cells <- grid_x > env_bounds_rounded$x[1] & grid_x < env_bounds_rounded$x[2] &
  ##       grid_y > env_bounds_rounded$y[1] & grid_y < env_bounds_rounded$y[2]

  ##     lonlat_indicies <- which(grid_cells, arr.ind=TRUE)
  ##     x_min <- min(lonlat_indicies[,1])-1 -1 ## nco indexes from 0. Also add buffer, currently naive
  ##     if (x_min > 0) x_min <- x_min - 1
  ##     x_max <- max(lonlat_indicies[,1])-1 +1
  ##     if (x_max < nc_file$dim$i$len - 1) x_max <- x_max + 1
  ##     y_min <- min(lonlat_indicies[,2])-1 -1
  ##     if (y_min > 0) y_min <- y_min- 1
  ##     y_max <- max(lonlat_indicies[,2])-1 +1
  ##     if (y_max < nc_file$dim$j$len - 1) y_max <- y_max + 1


  ## print(paste("Indices:",x_min,y_min,x_max,y_max));# <== print bbox in indices




        ## ncks (from ndo) subsets both time and space over opendap, while cdo subsets one or the other,
        ## then does the second subset on a local copy, resulting in a larger download.
        ## benchmarks, cdo sellatlonbox first, ~180 seconds
        ## cdo seltimestep first, ~30 seconds
        ## ncks first, ~3 seconds
        ## cdo makes regridding simpler, so using cdo on the local dataset.
        ## nco (and cdo) can use date ranges, and calculate the appropriate slices from dates.
      ##  It is just as fast (~3 seconds to ~4 seconds, could be internet issues), safer and clearer
      ## Date ranges bigger than the file just return the extents of the file, no harm going over.
        ## ncks(glue::glue("-q -R -O -d time,{start_slice},{end_slice} ",
        ##                 "-d i,{x_min},{x_max} -d j,{y_min},{y_max}  ",
        ##                 "-l ./  {file_data_hist_all$url_opendap} {variable_id}_{source_id}_raw_ncks.nc"))
        ## cdo(glue::glue("-remapbil,grid_des.txt, ",
        ##              "{variable_id}_{source_id}_raw_ncks.nc {variable_id}_{source_id}_raw_ncks_regrid.nc"))

        ## ncks(glue::glue("-q -R -O -d time,{target_hist_start},{target_hist_end} ",
        ##                 "-d i,{x_min},{x_max} -d j,{y_min},{y_max}  ",
        ##                 "-l ./  {file_data_hist_all$url_opendap} {variable_id}_{source_id}_raw_ncks_date.nc"))
        ## cdo(glue::glue("-remapbil,grid_des.txt, ",
        ##              "{variable_id}_{source_id}_raw_ncks.nc {variable_id}_{source_id}_raw_ncks_date_regrid.nc"))


## http://nco.sourceforge.net/nco.html#Subcycle
## http://nco.sourceforge.net/nco.html#Specifying-Input-Files
## http://nco.sourceforge.net/nco.html#ncrcat-netCDF-Record-Concatenator
     ## test a multifile pull
     #file_data_multi <- file_data[variable_id == "tos" & source_id == "EC-Earth3-Veg" & experiment_id == "historical" & any_hist == TRUE]
     #

        ## If I am getting errors, look back here
##       ## There is no effective way to read the error message from nco, short of changing the climateoperators packaeg
##       nco.cmd <- function(cmd,...,debug) {
##   #Build command
##   cmd.args <- paste(c(getOption("ClimateOperators")$nco,
##                       unlist(list(...))),collapse=" ")
##   this.cmd <-paste(cmd,cmd.args)

##   if(!debug) {
##     #Run the command
##     suppressWarnings(
##     run.time <- system.time(rtn <- system2(cmd,cmd.args, stderr = TRUE, stdout = TRUE))
##     )
##     if(length(rtn ) > 0 ){
##       if(attr(rtn, "status")!=0) stop( sprintf("%s command failed with error code %s.\n Message: [%s]\n Command issued: \n %s",cmd, attr(rtn, "status"), rtn,this.cmd))
##     }
##     attr(this.cmd,"run.time") <- run.time["elapsed"]
##   }
##   return(this.cmd)
## }
## assignInNamespace("nco.cmd",nco.cmd,ns="ClimateOperators")

 ## file_data_hist$url_opendap <- "http://esgf-cnr.hpc.cineca.it/thredds/dodsC/cmip6/vhis/CMIP6/CMIP/EC-Earth-Consortium/EC-Earth3-Veg/historical/r4i1p1f1/Omon/tos/gn/v20200425/tos_Omon_EC-Earth3-Veg_historical_r4i1p1f1_gn_185001-185012.nc"
 ## ret <-  tryCatch(
 ##        ncrcat(glue::glue("-R -O -d time,1900-01-01,1910-01-01 ",
 ##                        "-d i,{x_min},{x_max} -d j,{y_min},{y_max}  ",
 ##                        "-l ./  {paste(file_data_hist$url_opendap, collapse = \" \")} {variable_id}_{source_id}_{experiment_id}_raw.nc")),
 ##        error = function(e) return(e)
 ##        )
#test ret for success or error

    }



      }


    thredds_urls_future <- purrr::map(env_future_scenarios,
                ~{
                  glue::glue_data(metadata_local[experiment_id == .x,],
                                    "https://{data_node}/thredds/dodsC/master/{project}/{activity_id}/",
                                    "{institution_id}/{source_id}/{experiment_id}/{variant_label}/",
                                    "{table_id}/{variable_id}/{grid_label}/v{version}/{variable_id}_",
                                    "{table_id}_{source_id}_{experiment_id}_{variant_label}_",
                                    "{grid_label}_{format(lubridate::as_date(datetime_start), \"%Y%m\")}-",
                                    "{format(lubridate::as_date(datetime_stop), \"%Y%m\")}.nc")
                })

    ## Data nodes that are giving me trouble:
    ##esgf3.dkrz.de
    ##aims3.llnl.gov
    thredds_urls <- purrr::set_names(c(thredds_urls_hist, thredds_urls_future), nm = glue::glue("{variable_id}_{source_id}_{c(env_hist_scenarios[1], env_future_scenarios)}"))

    })
  ##A param, extract later

  microbenchmark::microbenchmark("box" ={
  cdo(glue::glue("-remapbil,r360x180, -seltimestep,1/60 -sellonlatbox,{env_bounds$x[1]},{env_bounds$x[2]},{env_bounds$y[1]},{env_bounds$y[2]}, {thredds_urls[1]} regrid_file2.nc"))
  },
  "time" = {
  cdo(glue::glue("-remapbil,r360x180,  -sellonlatbox,{env_bounds$x[1]},{env_bounds$x[2]},{env_bounds$y[1]},{env_bounds$y[2]}, -seltimestep,1/60 {thredds_urls[1]} regrid_file1.nc"))
  }, times = 2)


thredds_urls <- glue::glue_data(.x = metadata_full, "https://{data_node}/thredds/dodsC/master/{project}/{activity_id}/{institution_id}/{source_id}/{experiment_id}/{variant_label}/{table_id}/{variable_id}/{grid_label}/v{version}/{variable_id}_{table_id}_{source_id}_{experiment_id}_{variant_label}_{grid_label}_{format(lubridate::as_date(datetime_start), \"%Y%m\")}-{format(lubridate::as_date(datetime_stop), \"%Y%m\")}.nc")


  ##A param, extract later
env_bounds = list(
  x = c(109 + 1 / 24, 163 + 23 / 24),
  y = c(-47 - 23 / 24, -8 - 1 / 24)
)


  microbenchmark::microbenchmark("box" ={
  cdo(glue::glue("-remapbil,r360x180, -seltimestep,1/60 -sellonlatbox,{env_bounds$x[1]},{env_bounds$x[2]},{env_bounds$y[1]},{env_bounds$y[2]}, {thredds_urls[1]} regrid_file2.nc"))
  },
  "time" = {
  cdo(glue::glue("-remapbil,r360x180,  -sellonlatbox,{env_bounds$x[1]},{env_bounds$x[2]},{env_bounds$y[1]},{env_bounds$y[2]}, -seltimestep,1/60 {thredds_urls[1]} regrid_file1.nc"))
  }, times = 2)



  ## Make sure the datetime_start is January 2015

  all(date(metadata$datetime_start) == date("2015-01-16"))

  ## Then 2040 is (2040-2015)*12 = 300 months
  date("2015-01-16") %m+% months(300) == date("2040-01-16")

  ## Taking into account indexing from 1, Jan 2040 is slice 1+300
  ## A decade is 120 months
  mid_decade = glue::glue("301/{301+120}")


  ## 2090 is (2090-2015)*12 = 900 months
  date("2015-01-16") %m+% months(900) == date("2090-01-16")

  ## Taking into account indexing from 1, Jan 2090 is slice 1+900
  ## A decade is 120 months
  end_decade = glue::glue("901/{901+120}")
  time_slices <- paste(mid_decade, end_decade, sep = ",")

  ## The historical data will come from either a historical or esm-hist experiment
  ## I should be able to find a match pulled in already

  metadata_hist <- metadata[experiment_id %in% env_hist_scenarios, ]
  metadata_future <- metadata[experiment_id %in% env_future_scenarios, ]

 metadata_matched_hist <- metadata_future[metadata_hist, list(variant_label, variable_id, source_id,experiment_id, source_type, i.source_type, i.experiment_id ) ,  on =  list(variant_label, variable_id, source_id), nomatch = NULL]

## Shows simulations with no historical scenarios
 ## metadata_future[!metadata_hist, list(variant_label, variable_id, source_id,experiment_id) ,  on =  list(variant_label, variable_id, source_id)]
## Shows simulations with no future scenarios
 ## metadata_hist[!metadata_future, list(variant_label, variable_id, source_id,experiment_id) ,  on =  list(variant_label, variable_id, source_id)]

  ## For each variable, we want the different products to match bioORACLE, min, max, mean, range
  ## For each variable, there are multiple models, so we take the average of any models we can find.
  ## For convenience, we only take the first variant of a model.
  ## I only want models
  ##

  ## I have a list of models that may be relevant.
  ## For each variable:
  ##    For each model (source_id):
  ##      eliminate variants (variant_label) and models (source_id) options that are missing either future scenarios or historical scenarios
  ##       select one variant label (whatever is on top of list, not exactly random but not guarenteed to be smallest)
  ##      pick all
  ##      pick "esm-hist" if available, otherwise "historical" (experiment_id)



  tx<- nc_open("regrid_file1.nc")
 ## Years 2040-2050, 2090-2100
date("2040-01-01")-date("1850-01-01")

ncvar_get(tx, "time")
ncvar_get(tx, "time") +date("1850-01-01")


  nc_open("regrid_file2.nc")
library("ncdf4")
test <- nc_open(thredds_urls)

  ## May have to check for common variants of lat, latitude,LATITUDE etc.
t2 <- ncvar_get(test, varid = "time", start = c(1), count = c(10), verbose=TRUE)





generate_esgf_urls <-
