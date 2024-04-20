#' @title Normalize Raster Values Using Gaussian Fuzzy Membership Function 
#' 
#' @description This function applies a Gaussian fuzzy membership function to a raster object to scale its values between 0 and 1 based on the specified parameters of the Gaussian function.
#' 
#' @details The formula of Gaussian fuzzy membership function is exp(-(x - c)^2/(2 * sig^2)).
#' 
#' @param raster Raster* object representing the raster to be transformed.
#' @param sig Numeric value for the standard deviation of the Gaussian function.
#' @param c Numeric value representing the center of the Gaussian curve.
#' @param h Numeric height scaling factor.
#' @return Raster* object with transformed values.
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' # Create a raster with random values between 0 and 100
#' raster_obj <- raster(ncols = 30, nrows = 15)
#' values(raster_obj) <- runif(ncell(raster_obj), min = 0, max = 100)
#' plot(raster_obj)
#' # Apply Gaussian fuzzy membership function
#' transformed_raster <- gauss_fmf(raster = raster_obj, sig = 50, c = 50, h = 1)
#' plot(transformed_raster)
#' }
gauss_fmf <- function(raster, sig, c, h) {
  # Apply the Gaussian fuzzy membership function to each cell
  transformed_values <- exp(-((values(raster) - c)^2) / (2 * sig^2)) * h
  # Replace original raster values with transformed values
  setValues(raster, transformed_values)
}
