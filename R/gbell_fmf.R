#' @title Normalize Raster Values Using Generalized Bell Fuzzy Membership Function
#' 
#' @description This function scales raster values to the range [0, 1] using the Generalized Bell Fuzzy Membership Function. This transformation is useful for fuzzy logic operations and other applications where data scaling is necessary.
#' 
#' @details The formula of generalised bell fuzzy membership function is 1/(1+(((x-c) a)^2)^b).
#' 
#' @param raster Raster* object representing the raster to be transformed.
#' @param a Numeric, width parameter of the bell curve.
#' @param b Numeric, slope parameter of the bell curve.
#' @param c Numeric, center of the bell curve.
#' @return Raster* object with values scaled between 0 and 1.
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' # Create a raster with random values
#' raster_obj <- raster(ncols = 30, nrows = 15)
#' values(raster_obj) <- runif(ncell(raster_obj), min = 0, max = 100)
#' # Apply the Generalized Bell Fuzzy Membership function
#' transformed_raster <- gbell_fmf(raster_obj, a = 2, b = 3, c = 50)
#' # Visualize the transformed raster
#' plot(transformed_raster)
#' }
gbell_fmf <- function(raster, a, b, c) {
  # Calculate the Generalized Bell Fuzzy Membership values for each cell
  transformed_values <- 1 / (1 + (((values(raster) - c) / a) ^ 2) ^ b)
  # Replace original raster values with the transformed values
  setValues(raster, transformed_values)
}



