# load your libraries, R or Python
# library(multitaper)
# library(pracma)
# library(fields)
# library(png)
# library(signal)
# library(R.matlab)
# library(writexl)
# library(readxl)
# library(ggplot2)
# library(viridis)
# library(reshape2)


turbo <- viridis::turbo
`%OF%` <- dipsaus::`%OF%`

FREQUENCY_BAND_PRESTS <- list(
  delta = list(
    range = c(0.5, 4)
  ),
  theta = list(
    range = c(4, 8)
  ),
  alpha = list(
    range = c(8, 13)
  ),
  beta = list(
    range = c(13, 30)
  ),
  gamma = list(
    range = c(30, 90)
  ),
  high_gamma = list(
    range = c(90, NA)
  )
)


layout_heat_maps <- function (k, max_col, ratio = 4, layout_color_bar = TRUE, colorbar_cm = 3.5) {
  opars <- par(no.readonly = TRUE)
  # if (plotting_to_file()) {
  #   colorbar_cm <- 3
  # }
  nr <- ceiling(k/max_col)
  max_col = min(k, max_col)
  m <- 1:k
  mat <- matrix(c(m, rep.int(0, nr * max_col - k)), byrow = TRUE,
                nrow = nr, ncol = max_col)
  widths <- rep(ratio, max_col)
  if (layout_color_bar) {
    mat <- cbind(mat, k + 1)
    widths <- c(widths, lcm(colorbar_cm))
  }
  layout(mat, widths = widths)
}
