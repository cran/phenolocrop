## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(phenolocrop)

## -----------------------------------------------------------------------------
riceCH_eg

## -----------------------------------------------------------------------------
riceCH_eg |>
  logisLateDicr("x", "height")

## -----------------------------------------------------------------------------
x_vec <- 1:max(riceCH_eg$x)
y <- riceCH_eg |>
  logisLateDicr("x", "height") |>
  phenololine(x = x_vec, method = "logisLateDicr")
plot(x_vec, y, type = "l", ylab = "rice CH", xlab = "Dayes after sowing")
points(riceCH_eg$x, riceCH_eg$height)

