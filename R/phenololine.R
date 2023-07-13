#' Return time-series curve given the time-series model and parameter values
#'
#' @description phenololine function returns the predicted values given the
#' model name and model parameters.
#' @param param Vector of model parameter values.
#' @param x Vector of time (e.g. vector of dates).
#' @param method Character name of the time-series model. At present, only "logisLateDicr" is accepted.
#' @return phenololine function returns the trait values given x.
#' @details
#' If method = "logisLateDicr", param should be the vector of c(K, d0, r, a, d1).
#' @examples
#' library(phenolocrop)
#' y <- riceCH_eg |>
#'    logisLateDicr("x", "height") |>
#'    phenololine(x = 1:160, method = "logisLateDicr")
#' plot(1:160, y, type = "l")
#'
#' @export

phenololine <- function(param, x, method){
  if(method == "logisLateDicr"){
    if(length(param) != 5){
      stop(message =
             "\"logisLateDicr\" method need 5 parameters (K, d0, r, a, d1).")
    }
    K <- param[1]
    d0 <- param[2]
    r <- param[3]
    a <- param[4]
    d1 <- param[5]

    x0 <- x
    x0 <- x0 - d1
    id_zero <- which(x < d1)
    x0[id_zero] <- 0

    y <- K/(1 + exp(r * (d0 - x))) - a * x0^2

    return(y)
  }else{
    stop(message = "Method name is invalid.")
  }
}
