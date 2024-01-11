#' @title Uncertainty Analysis
#' @description Standard deviation of all risk maps after adjusting the weights.
#' @details All the risk maps with adjusted weights are put together
#' and the to calculate the standard deviation (uncertainty map).
#' @param rasters Raster* object.
#' @return raster Raster* object.
#' @importFrom terra stdev
#' @export
#' @examples
#' \donttest{
#' library(terra)
#' #Read all risk maps after adjusting the weights.
#' rasters <- terra::rast(dir("//unceratin_map",full.names = TRUE))
#' unceratin_map <- uncertain(rasters)
#' terra::plot(unceratin_map)
#' }
uncertain <- function(rasters) {
  stdev = terra::stdev(rasters)
  return(stdev)
}
