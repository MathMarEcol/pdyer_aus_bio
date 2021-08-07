predict_gfbootstrap <- function(
                                gfbootstrap_survey,
                                env_domain,
      env_biooracle_names,
      extrap,
      env_id_col
                                ) {


  if (is.na(gfbootstrap_survey$gfbootstrap)) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gfbootstrap_survey[, .(env_domain, trophic, survey, depth_cat)],
      gfboot_env_pred = NA
    ))
  }
  env_dom <- env_domain[[gfbootstrap_survey$env_domain]]


    predicted <- data.table::setDT(predict(object = gfbootstrap_survey$gfbootstrap[[1]],
                                    newdata = env_dom[env_biooracle_names],
                         ## Just take points, and calculate full coefficient matrix from points
                                    type = c("points"),
                                    extrap = extrap))
  predicted_stats <- predicted[type == "points",
                               {
                                 wide_boot <- data.table::dcast(
                                                            .SD[, .(var, y, gf_model)],
                                                            gf_model ~ var,
                                                            value.var = "y")
                                 wide_boot[, gf_model := NULL]
                              site_mean <- colMeans(wide_boot)
                              site_sigma <- cov(wide_boot)
                                 out <- data.table::data.table(site_mean = list(site_mean),
                                                   site_sigma = list(site_sigma))
                                 },
                               by = c("type", "x_row")]


  ## Assumes that predict() preserves the order of newdata
  data.table::setkey(predicted_stats, "x_row")
  predicted_stats[ , c(env_id_col) := env_dom[[env_id_col]]]

    return(data.table(gfbootstrap_survey[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(predicted_stats),
      env_pred_raw = list(predicted)
    ))

}
