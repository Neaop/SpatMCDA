#' @title Normalize Raster Values Using Sigmoid Fuzzy Membership Function
#' 
#' @description This function uses a sigmoid fuzzy membership function to scale raster values between 0 and 1. The sigmoid function is particularly useful for transforming data into a format suitable for binary classification or probabilities.
#' 
#' @details The sigmoid fuzzy membership function is defined by:
#' \deqn{f(x) = \frac{1}{1 + \exp(-a \cdot (x - c))}}
#' where \code{x} is the raster value, \code{a} is the steepness of the curve, and
#' \code{c} is the midpoint of the sigmoid curve.
#' 
#' @param raster Raster* object representing the raster to be transformed.
#' @param a Numeric, controls the steepness of the sigmoid curve.
#' @param c Numeric, the midpoint of the sigmoid curve.
#' @return Raster* object with values scaled between 0 and 1.
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' # Create a raster with random values between 0 and 100
#' raster_obj <- raster(ncols = 30, nrows = 15)
#' values(raster_obj) <- runif(ncell(raster_obj), min = 0, max = 100)
#' # Apply the sigmoid fuzzy membership function
#' transformed_raster <- sigmoid_fmf(raster_obj, a = 1, c = 50)
#' # Visualize the transformed raster
#' plot(transformed_raster)
#' }
sigmoid_fmf <- function(raster, a, c) {
  # Calculate the sigmoid fuzzy membership values for each cell
  transformed_values <- 1 / (1 + exp(-a * (values(raster) - c)))
  # Replace original raster values with the transformed values
  setValues(raster, transformed_values)
}
