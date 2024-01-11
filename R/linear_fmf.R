#' @title Linear fuzzy membership function
#' @description Based on Linear fuzzy membership function to make the
#'     value of the raster range between 0 and 1.
#' @details The formula of linear fuzzy membership function is ax+b.
#' @param raster Raster* object.
#' @param a Numeric.
#' @param b Numeric.
#' @return raster Raster* object..
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' raster <- raster(ncols = 30, nrows = 15)
#' values(raster) <- runif(n = ncell(raster),min = 0, max = 5)
#' #Create a grid with values in the range of 0-100
#' plot(raster)
#' raster_linearfmf <- linear_fmf(raster = raster,a = 0.2, b = 0)
#' plot(raster_linearfmf[[1]])
#' }
linear_fmf <- function(raster,a,b) {
  #FMF
  linear_fmf <- function(raster) {
    a*raster + b
  }
  ######Input value
  sapply(c(linear_fmf), function(F) F(raster))
}

