#' @title Reclassify Raster Values Based on Specified Ranges
#'
#' @description This function reclassifies values of a Raster* object according to specified ranges. It allows for grouping and transforming ranges of values into new single values. For example, values between 1 and 50 can be reclassified to 1, and values between 51 and 100 to 2. This is particularly useful for data categorization and simplification.
#'
#' @details Reclassification is performed using a matrix (`rcl`) that specifies the ranges:
#' The first two columns define the "from" and "to" values of the range, respectively, and the third column specifies the "becomes" value.
#'
#' @param raster Raster* object to be reclassified.
#' @param rcl A matrix with three columns, specifying the value ranges and their new values.
#' Each row corresponds to a reclassification rule: [from, to, becomes].
#' @return Reclassified Raster* object.
#' @export
#' @importFrom raster reclassify
#' @examples
#' \donttest{
#' library(raster)
#' # Create a raster with random values between 0 and 100
#' raster_obj <- raster(ncols = 30, nrows = 15)
#' values(raster_obj) <- runif(ncell(raster_obj), 0, 100)
#' # Define reclassification rules: values > 0 and <= 30 become 0.3, etc.
#' rcl_matrix <- matrix(c(0, 30, 0.3, 30, 60, 0.6, 60, 100, 1), ncol = 3, byrow = TRUE)
#' # Apply reclassification
#' reclassified_raster <- reclass_raster(raster_obj, rcl_matrix)
#' plot(reclassified_raster)
#' }
reclass_raster <- function(raster, rcl) {
  # Reclassify the raster using the provided matrix
  reclassified_raster <- raster::reclassify(raster, rcl, right = TRUE)
  return(reclassified_raster)
}




