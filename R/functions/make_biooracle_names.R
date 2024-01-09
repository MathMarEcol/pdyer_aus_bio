make_biooracle_names <- function(env_vars,
                                 env_modes,
                                 env_year,
                                 env_pathway,
                                 env_bathy) {
  future_conditions <- data.table::CJ(env_year, env_pathway)
  env_pairs <- data.table::as.data.table(
    merge.data.frame(env_vars, env_modes, all = TRUE)
  )

  env_biooracle_names <- lapply(1:nrow(future_conditions),
    function(r, future_conditions, env_pairs, env_bathy) {
      year_path <- as.vector(future_conditions[r, ])
      env_biooracle_names <- apply(
        env_pairs, 1,
        function(vars_modes, year_path) {
          sprintf("BO22_%s_%s_%s%s_ss", year_path[2], year_path[1], vars_modes[1], vars_modes[2])
        },
        year_path = year_path
      )

      ## Add bathymetry separately
      if (!is.na(env_bathy)) {
        env_biooracle_names <- c(env_biooracle_names, "MS_bathy_5m")
      }
      return(env_biooracle_names)
    },
    future_conditions = future_conditions,
    env_pairs = env_pairs,
    env_bathy = env_bathy
  )

  future_conditions[, env_biooracle_names := env_biooracle_names]

  present_conditions <- apply(
    env_pairs, 1,
    function(vars_modes) {
      sprintf("BO22_%s%s_ss", vars_modes[1], vars_modes[2])
    }
  )
  ## Add bathymetry separately
  if (!is.na(env_bathy)) {
    present_conditions <- c(present_conditions, "MS_bathy_5m")
  }

  return(rbind(
    data.table::data.table(
      env_year = "present",
      env_pathway = "present",
      env_biooracle_names = list(present_conditions)
    ),
    future_conditions
  ))
}
