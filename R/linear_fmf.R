#' @title Normalize Raster Values Using Linear Fuzzy Membership Function
#' 
#' @description This function modifies raster values using a linear fuzzy membership function, scaling the output so that values potentially range between 0 and 1, depending on the parameters. This simple linear transformation is defined by the equation `ax + b`.
#' 
#' @details The linear fuzzy membership function uses the formula:
#' \deqn{f(x) = a \cdot x + b}
#' where \code{x} is the raster value, \code{a} is the slope, and \code{b} is the y-intercept.
#' 
#' @param raster Raster* object representing the raster to be transformed.
#' @param a Numeric, the slope of the linear function.
#' @param b Numeric, the y-intercept of the linear function.
#' @return Raster* object with transformed values.
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' # Create a raster with random values between 0 and 5
#' raster_obj <- raster(ncols = 30, nrows = 15)
#' values(raster_obj) <- runif(ncell(raster_obj), min = 0, max = 5)
#' # Apply the linear fuzzy membership function
#' transformed_raster <- linear_fmf(raster_obj, a = 0.2, b = 0)
#' # Visualize the transformed raster
#' plot(transformed_raster)
#' }
linear_fmf <- function(raster, a, b) {
  # Calculate the linear fuzzy membership values for each cell
  transformed_values <- a * values(raster) + b
  # Replace original raster values with the transformed values
  setValues(raster, transformed_values)
}

