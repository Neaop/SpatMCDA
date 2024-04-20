#' @title Uncertainty Analysis of Risk Maps
#'
#' @description Calculate the standard deviation across multiple risk maps with adjusted weights
#' to generate an uncertainty map.
#'
#' @details This function aggregates risk maps where the weights have been adjusted, then computes
#' the standard deviation across these maps. The result is an uncertainty map that highlights areas
#' of high variability across the risk assessments.
#'
#' @param rasters A Raster* object containing multiple risk maps.
#' @return A Raster* object representing the standard deviation (uncertainty map).
#' @importFrom terra stdev
#' @export
#' @examples
#' \donttest{
#' library(terra)
#' # Assuming risk maps are stored in a directory and have been adjusted post-OAT analysis
#' rasters <- terra::rast(dir("//all_risk_maps", full.names = TRUE))
#' uncertainty_map <- uncertain(rasters)
#' terra::plot(uncertainty_map)
#' }
uncertain <- function(rasters) {
  # Calculate the standard deviation across the given raster layers
  stdev_map = terra::stdev(rasters)
  return(stdev_map)
}
