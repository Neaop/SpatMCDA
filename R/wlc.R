#' @title Weighted linear combination
#' @description Input the raster and weight values for each factor to
#'     output a decision map.
#' @details The formula of weighted linear combination function is ax1+bx2+cx3...,
#' the weights a, b, c.... of factors x1, x2 and x3... of the sum of 1
#' @param rasters Raster* object.
#' @param weights Numeric. The weight of each factor should correspond to the order of the rasters.
#' @return raster Raster* object.
#' @export
#' @examples
#' \donttest{
#' library(raster)
#'
#' # Create a stack of rasters
#' rasters <- stack("1_cattle.tif", "2_pig.tif", "3_sheep.tif")
#'
#' # Define the weights
#' #The order of the weights is aligned with the order of the rasters
#' weights <- c(0.2,0.3,0.5)
#'
#' # Ensure that the variable names are consistent
#' result <- wlc(rasters, weights)
#'
#' # Plot result
#' plot(result)
#' }
wlc <- function(rasters, weights) {

  # Check if the number of weights matches the number of rasters
  if (length(weights) != length(names(rasters))) {
    stop("The number of weights must match the number of rasters")
  }

  # Initialize the weighted sum with the first raster
  weighted_sum <- rasters[[1]] * as.numeric(weights[1])

  # Add the weighted values of the remaining rasters
  for (i in 2:length(names(rasters))) {
    weighted_sum <- weighted_sum + rasters[[i]] * as.numeric(weights[i])
  }

  return(weighted_sum)
}
