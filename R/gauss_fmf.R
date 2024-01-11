#' @title Gaussian fuzzy membership function
#' @description Based on Gaussian fuzzy membership function to make the
#'     value of the raster range between 0 and 1.
#' @details The formula of Gaussian fuzzy membership function is exp(-(x - c)^2/(2 * sig^2)).
#' @param raster Raster* object.
#' @param sig Numeric.
#' @param c Numeric.
#' @param h Numeric.
#' @return raster Raster* object..
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' raster <- raster(ncols = 30, nrows = 15)
#' values(raster) <- runif(n = ncell(raster),min = 0, max = 100)
#' #Create a grid with values in the range of 0-100
#' plot(raster)
#' raster_gaussfmf <- gauss_fmf(raster = raster,sig = 50, c = 100, h=1)
#' plot(raster_gaussfmf[[1]])
#' }
gauss_fmf <- function(raster,sig,c,h) {
  ######MF
  gauss_fmf <- function(raster) {
    exp(-(raster - c)^2 / (2 * sig^2)) * h
  }
  ######Input value
  sapply(c(gauss_fmf), function(F) F(raster))
}
