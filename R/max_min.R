#' @title Normalize Raster Values Using Min-Max scaling
#' 
#' @description This function applies a Min-Max normalization to a raster object, scaling the values so that they fall within the range of 0 to 1. The transformation is performed by subtracting the minimum value of the raster from each cell, and then dividing by the range of the data (max - min).
#' 
#' @details The specific formula is: x' = x - min/max - min
#' 
#' @param raster Raster* object representing the raster to be normalized.
#' @return raster Raster* object(value is between 0 and 1).
#' @export
#' @importFrom raster maxValue
#' @importFrom raster minValue
#' @examples
#' \donttest{
#' library(raster)
#' # Create a raster object with random values between 0 and 100
#' raster_obj <- raster(ncols = 30, nrows = 15)
#' values(raster_obj) <- runif(ncell(raster_obj), 0, 100)
#' # Apply Min-Max normalization
#' normalized_raster <- max_min(raster_obj)
#' # Plot the normalized raster
#' plot(normalized_raster)
#' }
max_min <- function(raster) {
  # Calculate the minimum and maximum values of the raster
  min_val <- raster::minValue(raster)
  max_val <- raster::maxValue(raster)
  # Perform Min-Max normalization
  stand_raster <- (raster - min_val) / (max_val - min_val)
  return(stand_raster)
}

