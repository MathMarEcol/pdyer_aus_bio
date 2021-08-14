predict_gfbootstrap <- function(
                                gfbootstrap_survey,
                                env_domain,
      env_biooracle_names,
      extrap,
      env_id_col
                                ) {


  if (is.na(gfbootstrap_survey$gfbootstrap[[1]])) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gfbootstrap_survey[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(NA),
      env_pred_raw = list(NA),
      dist_mat = list(NA)
    ))
  }
  env_dom <- env_domain[domain ==  gfbootstrap_survey$env_domain, data][[1]]


    predicted <- data.table::setDT(predict(object = gfbootstrap_survey$gfbootstrap[[1]],
                                    newdata = env_dom[,env_biooracle_names],
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

  ## Calculate p-matrix here

  row_pairs <- data.table::CJ(i = predicted_statsx_row, j = predicted_statsx_row)

  dist_long <- purrr::map2_dbl(row_pairs$i, row_pairs$j,
              ~ {
                if(.x < .y) {
                    b_dist <- fpc::bhattacharyya.dist(
                                   as.numeric(predicted_stats[.x, site_mean][[1]]),
                                   as.numeric(predicted_stats[.y, site_mean][[1]]),
                                   as.numeric(predicted_stats[.x, site_sigma][[1]]),
                                   as.numeric(predicted_stats[.y, site_sigma][[1]]),
                                 )
                    b_coeff_sim <- exp(-b_dist)
                  return(b_coeff_sim)
                } else {
                  return(NA)
                }
              }, predicted_stats)
  dist_mat <- matrix(dist_long, nrow(predicted_stat), nrow(predicted_stat))
  dist_mat[lower.tri(dist_mat)] <- t(dist_mat)[lower.tri(dist_mat)]
  diag(dist_mat) <- 1

    return(data.table(gfbootstrap_survey[, .(env_domain, trophic, survey, depth_cat)],
      env_pred_stats = list(predicted_stats),
      env_pred_raw = list(predicted),
      dist_mat = list(dist_mat)
    ))

}
