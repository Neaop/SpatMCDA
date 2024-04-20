#' @title Perform Weighted Linear Combination on Raster Layers
#'
#' @description This function applies a weighted linear combination to a stack of raster layers to produce a decision map. Each raster is multiplied by a corresponding weight and the results are summed to create a single output raster. This method is commonly used in spatial decision-making processes.
#'
#' @details The weighted linear combination (WLC) formula is:
#' \deqn{a \cdot x_1 + b \cdot x_2 + c \cdot x_3 + \ldots}
#' where `a`, `b`, `c`,... are the weights for factors `x_1`, `x_2`, `x_3`,... respectively.
#' The sum of all weights should be 1.

#' @param rasters A `terra::SpatRaster` object or a compatible object from the `raster` package.
#' @param weights Numeric vector specifying the weights for each raster layer.
#'                The weights should correspond in order to the layers in the `rasters` object.
#' @return A `terra::SpatRaster` object representing the weighted combination of input rasters.
#' @export
#' @importFrom terra rast
#' @importFrom terra app
#' @importFrom terra nlyr
#' @examples
#' \donttest{
#' library(terra)
#'
#' # Assuming rasters are loaded or created here
#' rasters <- rast(c(system.file("extdata", "1_AH.tif", package = "SpatMCDA"),
#'                   system.file("extdata", "2_Climate.tif", package = "SpatMCDA"),
#'                   system.file("extdata", "3_CV.tif", package = "SpatMCDA")))
#'
#' # Define the weights
#' weights <- c(0.2, 0.3, 0.5)
#'
#' # Perform the weighted linear combination
#' result <- wlc(rasters, weights)
#'
#' # Plot result
#' plot(result)
#' }
wlc <- function(rasters, weights) {
  # Ensure rasters is a SpatRaster, convert if necessary
  if (!inherits(rasters, "SpatRaster")) {
    rasters <- terra::rast(rasters)  # Convert using terra if not already a SpatRaster
  }

  # Check if the number of weights matches the number of layers
  if (length(weights) != terra::nlyr(rasters)) {
    stop("The number of weights must match the number of rasters in the stack.")
  }

  if (sum(weights) != 1) {
    warning("The sum of weights does not equal 1. Results may be biased.")
  }

  # Perform the weighted linear combination using terra::app
  weighted_sum <- terra::app(rasters, fun = function(x) { sum(x * weights, na.rm = TRUE) })

  return(weighted_sum)
}
