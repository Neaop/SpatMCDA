#' @title Sigmoid fuzzy membership function
#' @description Based on sigmoid fuzzy membership function to make the
#'     value of the raster range between 0 and 1.
#' @details The formula of sigmoid fuzzy membership function is 1/(1+exp(-a*(raster-c))).
#' @param raster Raster* object.
#' @param a Number.
#' @param c Number.
#' @return raster Raster* object..
#' @export
#' @examples
#' \donttest{
#'library(raster)
#'raster <- raster(ncols = 30, nrows = 15)
#'values(raster) <- runif(n = ncell(raster),min = 0, max = 100)
# Create a grid with values in the range of 0-100
#'plot(raster)
#'raster_sigmoidfmf <- sigmoid_fmf(raster = raster,a = 1, c = 2)
#'plot(raster_sigmoidfmf[[1]])
#' }
sigmoid_fmf <- function(raster,a,c) {
  #FMF
  sigmoid_fmf <- function(raster) {
    1/(1+exp(-a*(raster-c)))
  }
  ######Input value
  sapply(c(sigmoid_fmf), function(F) F(raster))
}
