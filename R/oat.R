#' @title One-Factor-At-a-Time Sensitivity Analysis
#'
#' @description Performs sensitivity analysis by adjusting the weight of one factor at a time
#'     and assessing the impact on the resulting raster. This helps in understanding the influence
#'     of each factor on the outcome while other factors are held constant.
#'
#' @details Implements the One-Factor-At-a-Time method where each factor's weight is adjusted
#'     individually and the effect is observed in the output. The formula used is wi=(1-wa)*wi0/(1-wa0) for 1≤i≤n, i≠a.
#'     wi0 is the initial weight of each risk factor, wa0 is the initial weight of the adjusted risk factor.
#'     The sum of all weights equals 1. This function also calculates the difference from the baseline scenario
#'     where no weights are adjusted and can plot the results if desired.
#'
#' @param rasters RasterStack object containing the layers to be analyzed.
#' @param weights Numeric vector, initial weights of each raster layer, must correspond to the order in the raster stack.
#' @param range Numeric vector of length 2, defining the range (min, max) of weight adjustment in percentage points.
#' @param step Numeric, the increment in weight adjustment across the specified range.
#' @param output_dir Character string, path to the directory where output rasters will be saved.
#' @param plot Logical, whether to plot the raster differences using `rasterVis::levelplot`.
#' @param dominant_factor Character string, the name of the dominant factor.
#' @return Returns nothing but writes adjusted raster files and optionally plots them.
#' @importFrom raster writeRaster
#' @importFrom raster nlayers
#' @importFrom raster calc
#' @importFrom raster values
#' @importFrom rasterVis levelplot
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' library(rasterVis)
#'
#' # Assume there are three raster files representing different risk factors
#' # For example, these raster files might be located in a subdirectory of your working directory
#' rasters <- raster::stack(
#'   system.file("extdata", "1_AH.tif", package = "SpatMCDA"),
#'   system.file("extdata", "2_Climate.tif", package = "SpatMCDA"),
#'   system.file("extdata", "3_CV.tif", package = "SpatMCDA")
#' )
#'
#' # Assign names to each raster layer, which is necessary for specifying the dominant_factor
#' names(rasters) <- c("1_AH", "2_Climate", "3_CV")
#'
#' # Set initial weights, ensuring the order of weights matches the order of raster layers
#' weights <- c("1_AH" = 0.2, "2_Climate" = 0.3, "3_CV" = 0.5)
#'
#' # Set the range and step size for weight adjustments
#' range <- c(-20, 20)  # Percentage range for weight adjustment (%)
#' step <- 1            # Step size for weight adjustment (%)
#'
#' # Set the output directory, ensure this directory exists or you have permission to create files in it
#' output_dir <- "/oat"
#'
#' # Call the improved oat() function
#' # Specify 'pig' as the main factor to adjust
#' oat(
#'   rasters = rasters,
#'   weights = weights,
#'   dominant_factor = "1_AH"    # Specify '1_AH' as the dominant factor
#'   range = range,
#'   step = step,
#'   output_dir = output_dir,
#'   plot = TRUE               # Set to TRUE if you want to see visualizations of the differences
#' )
#' }



oat <- function(rasters, weights, range, step, output_dir, plot = FALSE, dominant_factor) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("The 'raster' package is required but not installed.")
  }

  if (length(weights) != raster::nlayers(rasters)) {
    stop("The number of weights must match the number of layers in the raster stack.")
  }

  if (is.null(dominant_factor) || !(dominant_factor %in% names(weights))) {
    stop("A valid 'dominant_factor' must be specified and match one of the raster layer names.")
  }
  # Debug: print current order before change
  cat("Current order of layers and weights before adjustment:\n")
  print(names(rasters))
  print(weights)

  # Reorder rasters and weights based on the dominant factor
  dominant_index <- which(names(weights) == dominant_factor)
  if (dominant_index != 1) {
    weights <- c(weights[dominant_index], weights[-dominant_index])
    rasters <- raster::stack(rasters[[dominant_index]], rasters[-dominant_index])
  }

  # Create a subdirectory for the dominant factor in the specified output directory
  factor_output_dir <- file.path(output_dir, dominant_factor)
  if (!dir.exists(factor_output_dir)) {
    tryCatch({
      dir.create(factor_output_dir)
    }, error = function(e) {
      stop("Failed to create directory: ", factor_output_dir, ". Error: ", e$message)
    })
  }
  # Create a subdirectory for difference rasters
  difference_dir <- file.path(output_dir, paste0(dominant_factor,"_","difference"))
  if (!dir.exists(difference_dir)) {
    dir.create(difference_dir)
  }

  # Calculate the baseline raster with initial weights
  baseline <- raster::calc(rasters, fun = function(x) sum(x * weights))
  #baseline_file <- file.path(factor_output_dir, "baseline.tif")
  #raster::writeRaster(baseline, filename = baseline_file, format = "GTiff", overwrite = TRUE)

  base_weight <- weights[1]  # Base weight of the main factor
  for (i in seq(range[1], range[2], step)) {
    main_factor_weight <- base_weight + base_weight * i * 0.01
    remaining_weight_sum <- 1 - main_factor_weight
    weighted_sum <- rasters[[1]] * main_factor_weight

    for (a in 2:raster::nlayers(rasters)) {
      adjusted_weight <- remaining_weight_sum * weights[a] / (1 - base_weight)
      weighted_sum <- weighted_sum + raster::subset(rasters, a) * adjusted_weight
    }

    # Calculate and write the adjusted raster
    output_file_name <- paste0("result_", i, ".tif")
    output_file <- file.path(factor_output_dir, output_file_name)
    raster::writeRaster(weighted_sum, filename = output_file, format = "GTiff", overwrite = TRUE)

    # Calculate and write the difference raster
    difference_file_name <- paste0("difference_", i, ".tif")
    diff_file <- file.path(difference_dir, difference_file_name)
    difference <- baseline - weighted_sum
    raster::writeRaster(difference, filename = diff_file, format = "GTiff", overwrite = TRUE)


    # Calculate and print the max difference as a percentage of the baseline max
    baseline_max <- max(raster::values(baseline), na.rm = TRUE)
    difference_max <- max(raster::values(difference), na.rm = TRUE)
    max_difference_percentage <- (difference_max / baseline_max) * 100
    print(paste("Max difference for step", i, "%", ":", round(max_difference_percentage, 2), "% of baseline max"))

    # Optionally plot the differences
    if (plot && requireNamespace("rasterVis", quietly = TRUE)) {
      library(rasterVis)
      plot_title <- paste("Difference Plot for Weight Adjustment of", i, "%")
      plot_diff <- levelplot(difference, main = plot_title)
      print(plot_diff)
    }

    print(output_file)
  }
}
