#' @title Generalised bell fuzzy membership function
#' @description Based on Generalised bell fuzzy membership function to make the
#'     value of the raster range between 0 and 1.
#' @details The formula of generalised bell fuzzy membership function
#' function is 1/(1+(((x-c) a)^2)^b).
#' @param raster Raster* object.
#' @param a Numeric.
#' @param b Numeric.
#' @param c Numeric.
#' @return raster Raster* object..
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' raster <- raster(ncols = 30, nrows = 15)
#' values(raster) <- runif(n = ncell(raster),min = 0, max = 100)
#' #Create a grid with values in the range of 0-100
#' plot(raster)
#' raster_gbellfmf <- gbell_fmf(raster,a = 1, b = 2,c = 3)
#' #Make the value of the grid between 0 and 1
#' plot(raster_gbellfmf[[1]])
#' }
gbell_fmf <- function(raster,a,b,c) {
  #FMF
  gbell_fmf <- function(raster) {
   1/(1+(((raster-c)/a)^2)^b)
  }
  ######Input value
  sapply(c(gbell_fmf), function(F) F(raster))
}




