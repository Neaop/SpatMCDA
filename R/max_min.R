#' @title Normalization of maximum and minimum values
#' @description The maximum-minimum normalization is the normalization process using the maximum and minimum values in the data column,
#'     and the normalized value is between 0 and 1. It is calculated by making a difference between the
#'     data and the minimum value of the column and dividing it by the extreme difference.
#' @details The specific formula is: x' = x - min/max - min
#' @param raster Raster* object.
#' @return raster Raster* object(value is between 0 and 1).
#' @export
#' @importFrom raster maxValue
#' @importFrom raster minValue
#' @examples
#' \donttest{
#' library(raster)
#' raster <- raster(ncols = 30, nrow = 15)
#' values(raster) <- runif(ncell(raster),0,100)
#' #Normalization of maximum and minimum values
#' maxmin_raster <- max_min(raster)
#' plot(maxmin_raster)
#' }
max_min <- function(raster){
  stand_raster <- (raster - raster::minValue(raster))/(raster::maxValue(raster) - raster::minValue(raster))
  return(stand_raster)
}

