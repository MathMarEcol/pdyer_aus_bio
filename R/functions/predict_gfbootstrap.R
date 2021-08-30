predict_gfbootstrap <- function(
                                gfbootstrap_combined,
                                env_domain,
      env_biooracle_names,
      extrap,
      pred_importance_top,
      env_id_col
                                ) {

 if (any(is.na(gfbootstrap_combined$gfbootstrap[[1]]))) {
    ## Upstream target decided survey was not usable.
    ## Propagating
    ##
    return(data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid)],
      env_pred_stats = list(NA),
      env_pred_raw = list(NA),
      imp_preds = list(NA),
      sim_mat = list(NA)
    ))
  }
  env_dom <- env_domain[domain ==  gfbootstrap_combined$env_domain, data][[1]]

    predicted <- predict(object = gfbootstrap_combined$gfbootstrap[[1]],
                                    newdata = env_dom[,..env_biooracle_names],
                         ## Just take points, and calculate full coefficient matrix from points
                                    type = c("points"),
                                    extrap = extrap)
  pred_points <- predicted$points
  data.table::setDT(pred_points)
  ## With only 20 trees, and therefore 20 samples, for fitting 28 dimensions, covariance matrices are coming out
  ## singular. Test by dropping most predictors.
  ## May even automatically add a top 80% of variance method
  imp <- importance(gfbootstrap_combined$gfbootstrap[[1]], sort = TRUE)
  if (pred_importance_top >= 1) {
    imp_preds <- names(imp)[seq.int(1,min(length(imp), pred_importance_top))]
  } else {
    imp_explained <- cumsum(imp)/sum(imp)
    ## Take all predictors below threshold, then one more
    n_preds <- sum(imp_explained < pred_importance_top) + 1
    imp_preds <- names(imp)[seq.int(1,n_preds)]
  }

  predicted_stats <- pred_points[pred %in% imp_preds,
  #predicted_stats <- pred_points[,
                               {
                                 wide_boot <- data.table::dcast(
                                                            .SD[, .(pred, y, gf)],
                                                            gf ~ pred,
                                                            value.var = "y")
                                 wide_boot[, gf := NULL]
                              site_mean <- colMeans(wide_boot)
                              site_sigma <- cov(wide_boot)
                                 out <- data.table::data.table(site_mean = list(site_mean),
                                                   site_sigma = list(site_sigma),
                                                   site_sigma_det = determinant(site_sigma, logarithm=FALSE)$modulus)
                                 },
                               by = c("x_row")]


  if(FALSE){

    library(ggplot2)

    ggplot(data.table::dcast(pred_points[x_row == 1, .(pred, y, gf)],
                                                            gf ~ pred,
                                                            value.var = "y") ,
           aes_string(x = env_biooracle_names[1], y = env_biooracle_names[6])) + geom_point()



    }

  ## Assumes that predict() preserves the order of newdata
  data.table::setkey(predicted_stats, "x_row")
  predicted_stats[ , c(env_id_col) := env_dom[[env_id_col]]]

  ## Calculate p-matrix here
  row_pairs <- data.table::CJ(i = predicted_stats$x_row, j = predicted_stats$x_row)

  ## Benchmarking tests
  is_benchmark <- FALSE
  if (is_benchmark) {

    microbenchmark::microbenchmark(
  expand.grid = expand.grid(i = predicted_stats$x_row, j = predicted_stats$x_row),
  dt_CJ = data.table::CJ(i = predicted_stats$x_row, j = predicted_stats$x_row),
  times  = 5)
## + Unit: milliseconds
##         expr       min       lq      mean    median       uq       max neval
##  expand.grid 299.29575 325.8230 358.19717 328.50285 409.2855 428.07873     5
##        dt_CJ  25.49798  30.2899  45.40107  35.53718  54.2142  81.46611     5
## Conclusion: Use data.table::CJ


    ## `$colname` vs `[[char]]`
 x<- as.data.table(matrix(runif(10000), 100,100))
 microbenchmark::microbenchmark(a= x[[y]], b = x$V87, times = 1000)
## Unit: microseconds
##  expr    min      lq      mean median      uq     max neval
##     a 12.594 12.8345 15.143436 12.951 13.3085 149.856  1000
##     b  7.403  7.4600  8.393999  7.583  7.7045 109.657  1000
##   }


    ## Why are we spending more time in DT `[` than solve()?
  .x <- 1
    microbenchmark::microbenchmark(
                                   dt_lookup = predicted_stats[.x, site_mean][[1]],
                                   dollar_lookup = predicted_stats$site_mean[[.x]],
    times = 100)
## + Unit: microseconds
##           expr     min       lq       mean   median      uq      max neval
##      dt_lookup 477.607 614.0275 1252.90690 712.8040 846.524 8549.186   100
##  dollar_lookup   2.285   3.3760    6.29809   5.4975   7.291   78.464   100
##  Conclusion:: data.table's `[` is massively more expensive than dollar lookup and
## getting the nth element of a list via `[[`
##


    ## Quicker way to square a value
    microbenchmark::microbenchmark(
                      hat = sqrt(exp(1))^2,
                      tmp = {tmp <- sqrt(exp(1)); tmp*tmp},
                      times = 1000)
## + Unit: nanoseconds
##  expr  min   lq     mean median   uq   max neval
##   hat  737  852  934.134    862  874 40656  1000
##   tmp 1265 1408 1472.286   1425 1445 27953  1000
## Conclusion: Hat is faster, but times are insignificant

  .x <- 1
  .y <- 100
    ## 4 way comparison, general solve, cholesky decomp, base R, Rfast

    mat_to_inv <- predicted_stats$site_sigma[[.x]] +
      predicted_stats$site_sigma[[.y]]
    microbenchmark::microbenchmark(
                      r_sol = solve(mat_to_inv),
                      r_chol = chol2inv(chol(mat_to_inv)),
                      rfast_chol = Rfast::spdinv(mat_to_inv),
                      times = 1000)
## + Unit: microseconds
##        expr    min      lq     mean  median       uq      max neval
##       r_sol 67.419 74.2855 503.6337 82.9260 104.0895 47134.38  1000
##      r_chol 36.302 40.0500 380.4320 43.6235  56.6290 16310.57  1000
##  rfast_chol 53.514 57.9350 361.5582 64.2555  85.9290 16350.16  1000
##  Conclusion: the built-in chol2inv(chol()) is best

    ## Did I waste my time writing a bhattacharyya function?
    microbenchmark::microbenchmark(
                    mine = my_bhattacharyya_dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma_det[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.y]],
                                   predicted_stats$site_sigma_det[[.y]]
                                 ),
                    fpc = fpc::bhattacharyya.dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma[[.y]]
                                 ),
                      times = 1000)
## + Unit: microseconds
##  expr     min       lq     mean   median      uq      max neval
##  mine 118.921 152.7595 2007.996 185.0625 265.060 23911.71  1000
##   fpc 306.596 390.6460 2673.587 457.9545 871.825 25065.96  1000
##   Conclusion: No, ~50% speedup


    ## Det or determinant
  .x <- 1
  .y <- 100
    ## 4 way comparison, general solve, cholesky decomp, base R, Rfast

    mat_to_inv <- predicted_stats$site_sigma[[.x]] +
      predicted_stats$site_sigma[[.y]]
    microbenchmark::microbenchmark(
                      det = det(mat_to_inv),
                      determinant_log = exp(determinant(mat_to_inv, logarithm=TRUE)$modulus),
                      determinant_straight = determinant(mat_to_inv, logarithm=FALSE)$modulus,
                      times = 1000)

  .x <- 1
  .y <- 100



profvis(expr = {
  for(i in 1:1000) {
                if(.x < .y) {
                    b_dist <- my_bhattacharyya_dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma_det[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.y]],
                                   predicted_stats$site_sigma_det[[.y]]
                                 )
                    b_coeff_sim <- exp(-b_dist) ## Do this later, vectorise
                } else {
                  b_coeff_sim <- NA
                }
                }
  }, 0.005 )
profvis(expr = {
  for(i in 1:1000) {
                if(.x < .y) {
                    b_dist <- fpc::bhattacharyya.dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma[[.y]]
                                 )
                    b_coeff_sim <- exp(-b_dist)
                } else {
                  b_coeff_sim <- NA
                }
                }
  }, 0.005 )
profvis(expr = {
  for(i in 1:1000) {
                if(.x < .y) {
                    b_dist <- fpc::bhattacharyya.dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma[[.y]]
                                 )
                    b_coeff_sim <- exp(-b_dist)
                } else {
                  b_coeff_sim <- NA
                }
                }
  }, 0.005 )
profvis(expr = {
  for(i in 1:1000) {
                if(.x < .y) {
                    b_dist <- fpc::bhattacharyya.dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma[[.y]]
                                 )
                    b_coeff_sim <- exp(-b_dist)
                } else {
                  b_coeff_sim <- NA
                }
                }
  }, 0.005 )
Rprof(NULL)
  print(b_coeff_sim)

  rm(".x")
  rm(".y")
  }
  dist_long <- purrr::map2_dbl(row_pairs$i, row_pairs$j,
              ~ {
                if(.x < .y) {
                    b_dist <- my_bhattacharyya_dist(
                                   predicted_stats$site_mean[[.x]],
                                   predicted_stats$site_sigma[[.x]],
                                   predicted_stats$site_sigma_det[[.x]],
                                   predicted_stats$site_mean[[.y]],
                                   predicted_stats$site_sigma[[.y]],
                                   predicted_stats$site_sigma_det[[.y]]
                                 )
                  return(b_dist)
                } else {
                  return(NA)
                }
              }, predicted_stats)
  ## Bhattacharyya coefficient = exp(-Bhattacharyya distance)
  sim_mat <- matrix(exp(-dist_long), nrow(predicted_stats), nrow(predicted_stats))
  sim_mat[upper.tri(sim_mat)] <- t(sim_mat)[upper.tri(sim_mat)]
  diag(sim_mat) <- 1

    return(data.table::data.table(gfbootstrap_combined[, .(env_domain, trophic, survey, depth_cat, is_combined, surv_full_name, frac_valid)],
      env_pred_stats = list(predicted_stats),
      env_pred_raw = list(predicted),
      imp_preds = list(imp_preds),
      sim_mat = list(list(sim_mat)) ##double wrap the sim mat so data.table doesn't try to print it
    ))

}

  my_bhattacharyya_dist <- function(x_mean,
                               x_sigma,
                               x_det,
                               y_mean,
                               y_sigma,
                               y_det
                               ){
    ## If the input determinants are 0,
    ## then bhattacharyya dist will be infinite
    if (x_det == 0 | y_det == 0) {
      return(Inf)
    }

    joint_cov <- (x_sigma + y_sigma)/2
    joint_det <- determinant(joint_cov, logarithm = FALSE)$modulus
    joint_cov_inv <- tryCatch(
      chol2inv(chol(joint_cov)),
    error = function(e){
      return(MASS::ginv(joint_cov))
      }
    )

    joint_mean <- x_mean-y_mean

    bhattacharyya_dist <- 0.125 * sum((t(joint_mean) %*% joint_cov_inv) * joint_mean) +
      0.5 * log(joint_det / sqrt(x_det * y_det))
    return(bhattacharyya_dist)
    }
