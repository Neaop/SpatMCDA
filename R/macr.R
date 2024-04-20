#' @title Mean of Absolute Change Rates
#'
#' @description Computes enhanced statistical metrics for the results from a One-Factor-at-a-Time (OAT) sensitivity analysis of a main factor. This function aims to provide a deeper insight into how changes in a factor's weight affect the model's outcomes by analyzing mean, standard deviation, minimum, and maximum absolute change rates.
#'
#' @details This function enhances the traditional Mean of Absolute Change Rates (MACR) analysis by not only calculating the mean change rate but also providing additional statistics such as the standard deviation, minimum, and maximum change rates. These metrics offer a more comprehensive view of the variability and distribution of change rates, helping to identify how stable or volatile the results are under different weight adjustments. The function requires a RasterStack of OFAT results, a baseline raster to compare against, and outputs the results in a well-organized CSV file within a directory named after the analyzed factor.
#'
#' @param oat_rasters A RasterStack object containing rasters from OFAT analysis for various factors.
#' @param wlc_raster A RasterLayer object that represents the baseline or weighted linear combination from which deviations are calculated.
#' @param output_dir The directory path where the output files will be saved. This path must be accessible and writable.
#' @param factor_name A string specifying the name of the main factor. This name is used to organize the output files into a dedicated subdirectory within the output directory, ensuring that the results are easy to locate and review.
#' @return Does not return a value but writes a CSV file with detailed statistical analysis results, along with individual change rate raster files.
#' @importFrom raster writeRaster
#' @importFrom raster cellStats
#' @importFrom utils write.csv
#' @export
#' @examples
#' \donttest{
#' # Calculate the mean of absolute change rates for the factor "1_AH".
#' library(raster)
#' # Read the results of the One-Factor-at-a-Time analysis for the factor "AH".
#' oat_rasters <- raster::stack(dir("//oat//1_AH", full.names = TRUE))
#' # Read the decision map obtained with the weighted linear combination.
#' wlc_raster <- raster::raster("//wlc//decision_map.tif")
#' # Specify the output directory.
#' output_dir <- "//macr"
#' # Calculate enhanced MACR and save the results under the specified factor's directory.
#' macr(oat_rasters = oat_rasters, wlc_raster = wlc_raster,
#'      output_dir = output_dir, factor_name = "1_AH")
#' }

macr <- function(oat_rasters, wlc_raster, output_dir, factor_name) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("The 'raster' package is required but not installed.")
  }

  if (is.null(factor_name)) {
    stop("A valid 'factor_name' must be specified.")
  }

  # Create a specific directory for the factor
  factor_output_dir <- file.path(output_dir, factor_name)
  if (!dir.exists(factor_output_dir)) {
    dir.create(factor_output_dir)
  }

  result <- data.frame(
    CRW = character(0),
    MACRs = numeric(0),
    Factor_Name = character(0),
    SD = numeric(0),
    Min = numeric(0),
    Max = numeric(0)
  )

  for (i in 1:nlayers(oat_rasters)) {
    change_rate <- abs((oat_rasters[[i]] - wlc_raster) / wlc_raster)
    name <- paste0(names(oat_rasters[[i]]), ".tif")
    raster::writeRaster(x = change_rate, filename = paste0(factor_output_dir, "/", name), format = "GTiff", overwrite = TRUE)

    mean_abs_change_rate <- raster::cellStats(x = change_rate, stat = "mean") * 100
    sd_change_rate <- raster::cellStats(x = change_rate, stat = "sd") * 100
    min_change_rate <- raster::cellStats(x = change_rate, stat = "min") * 100
    max_change_rate <- raster::cellStats(x = change_rate, stat = "max") * 100

    print(names(oat_rasters[[i]]))
    print(paste0("Mean: ", round(mean_abs_change_rate, digits = 2), "%, SD: ", round(sd_change_rate, digits = 2),
                 "%, Min: ", round(min_change_rate, digits = 2), "%, Max: ", round(max_change_rate, digits = 2), "%"))

    # Append the results to the data frame
    result <- rbind(result, data.frame(
      CRW = names(oat_rasters[[i]]),
      MACRs = round(mean_abs_change_rate, digits = 2),
      Factor_Name = factor_name,
      SD = round(sd_change_rate, digits = 2),
      Min = round(min_change_rate, digits = 2),
      Max = round(max_change_rate, digits = 2)
    ))
  }

  # Write the results to a CSV file
  utils::write.csv(x = result, file = paste0(factor_output_dir, "/", factor_name, "_macr.csv"), row.names = FALSE)
}
