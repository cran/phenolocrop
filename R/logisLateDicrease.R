#' Three-parameter logistic with the decrease in the late growth phase
#'
#' @description Apply a time-series model, three-parameter logistic with the decrease in the
#' late growth phase, to the time-series trait data.
#' This function was originally developed for the time-series data of rice canopy height.
#' Fitting the time-series model is done by the "two-step procedure".
#' For more information, see Taniguchi et al. (2022).
#'
#' @param dat data.frame including date and trait (e.g. canopy height).
#' @param x Column name (character) for the date after sowing or plantingl.
#' @param y Column name (character) for the trait.
#' @param returnModels Logical value whether to return the time-series model object. Default is F.
#' @param start Start values to estimate 'd0', 'r' and 'a'. Default is 'c(d0 = 50, r = 0.05, a = 0.0001)'.
#' @param upper Upper bounds to estimate 'd0', 'r' and 'a'. Default is 'c(d0 = 200, r = Inf, a = 1)'.
#' @param lower Lower bounds to estimate 'd0', 'r' and 'a'. Default is 'c(d0 = 0, r = 0, a = 0)'.
#'
#' @return
#' logisLateDicr function returns the vector of estimated parameter values.
#' If returnModels = TRUE, this function also returns the cubic polynomial
#' regression object and logistic with the decrease in the
#' late growth phase regression object.
#'
#' @examples
#' library(phenolocrop)
#' riceCH_eg |>
#'    logisLateDicr("x", "height")
#'
#' @references
#' S. Taniguchi et al., (2022) Prediction of heading date, culm length, and
#' biomass from canopy-height-related parameters derived from time-series UAV observations of rice.
#' Frontiers in Plant Science, 13:998803.
#'
#' @importFrom stats lm
#' @importFrom stats nls
#' @importFrom stats predict
#'
#' @export


logisLateDicr <- function(dat, x, y,
                          returnModels = FALSE,
                          start = c(d0 = 50, r = 0.05, a = 0.0001),
                          upper = c(d0 = 200, r = Inf, a = 1),
                          lower = c(d0 = 0, r = 0, a = 0)){

  x <- dat |> purrr::pluck(x)
  y <- dat |> purrr::pluck(y)

  ### 1st step: cubic polynomial regression

  dat_poly <- data.frame(y = as.numeric(y),
                         x1 = x,
                         x2 = x^2,
                         x3 = x^3)

  lm_poly <- lm(y ~ x1 + x2 + x3, data = dat_poly)
  x_max <- max(x)
  x_new_vec <- 1:x_max
  y_pred <- predict(lm_poly,
                    newdata = list(x1 = x_new_vec,
                                   x2 = x_new_vec^2,
                                   x3 = x_new_vec^3))

  d1 <- findMaxima(y_pred)
  if(is.null(d1)){
    stop(message =
           "The cubic polynomial did not detect maximum time point 'd1'")
  }

  ### 2nd step: 3-parameter logistic with decrease in the late growth phase

  K <- max(y)
  x0 <- x
  x0 <- x0 - d1
  id_zero <- which(x < d1)
  x0[id_zero] <- 0
  dat_logi <- data.frame(y = y,
                         x = x,
                         x0 = x0)

  nls_logi <- tryCatch(nls(y ~ K/(1 + exp(r * (d0 - x))) - a * x0^2,
                           data = dat_logi,
                           start = start,
                           algorithm = "port",
                           upper = upper,
                           lower = lower),
                           error = function(e){
                             stop(message =
                                    "Failed to apply nls. Modifying the start, upper, and lower values may solve the problem.")
                           })

  nls_logi_summ <- summary(nls_logi)
  param <- nls_logi_summ$coefficients[, "Estimate"]
  param <- c(K = K, param, d1 = d1)
  if(returnModels == FALSE){
    return(param)
  }else{
    return(list(param, lm_poly, nls_logi))
  }
}

findMaxima <- function(x_vec){
  n <- length(x_vec) - 2
  for(i in 2:(n + 1)){
    if(x_vec[i - 1] < x_vec[i] && x_vec[i + 1] < x_vec[i]){
      return(i)
    }
  }
}
