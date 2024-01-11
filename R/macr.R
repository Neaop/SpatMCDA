#' @title Mean of Absolute Change Rates
#' @description Input the results of the One-Factor-a-time at analysis of the main factor to
#'     output its mean of absolute change rates.
#' @details The calculation formula can be found in this literature:
#' Wang H, Xiao J, Ouyang M, et al. Assessment of foot-and-mouth disease risk areas in
#' mainland China based spatial multi-criteria decision analysis. BMC Veterinary Research,
#' 2021, 17: 1-12.
#' @param oat_rasters Raster stacks.
#' @param wlc_raster Raster* object.
#' @param output_dir Either a character string naming a file or a connection open for writing.
#' "" indicates output to the console.
#' @return raster Raster* object.
#' @return csv Mean of Absolute Change Rates(%)
#' @importFrom raster writeRaster
#' @importFrom raster cellStats
#' @importFrom utils write.csv
#' @export
#' @examples
#' \donttest{
#' #Calculate the mean of absolute change rates the factor Cattle.
#' library(raster)
#' #Read the results of the One-Factor-a-time of factor Cattle.
#' oat_rasters <- raster::stack(dir("//oat//cattle",full.names = TRUE))
#' #Read the decision map obtained with the weighted linear combination.
#' wlc_raster <- raster::raster("//wlc//decision_map.tif")
#' #All results are output in out_dir
#' macr_cattle <- macr(oat_rasters = oat_rasters,wlc_raster = wlc_raster,
#'                     output_dir = "//macr//cattle")
#' }
# macr <- function(oat_rasters, wlc_raster, output_dir) {
#   result <- data.frame(matrix(ncol = length(names(oat_rasters)), nrow = 1))
#   names(result) <- names(oat_rasters)
#   for (i in 1:length(names(oat_rasters))){
#     a <- (oat_rasters[[i]]-wlc_raster)/wlc_raster
#     name <- paste0(names(oat_rasters[[i]]),".tif")
#     raster::writeRaster(x = a,filename= paste0(output_dir,"/",name),
#                         format="GTiff",overwrite=TRUE)
#     b <- a^2
#     c <- sqrt(b)
#     d <- raster::cellStats(x = c,stat = "mean")
#     print(names(oat_rasters[[i]]))
#     print(d)
#     result[1,i] <- d*100
#   }
#   utils::write.csv(x = result, file = paste0(output_dir,"/","macr.csv"))
# }
macr <- function(oat_rasters, wlc_raster, output_dir) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("The 'raster' package is required but not installed.")
  }

  result <- data.frame(matrix(ncol = nlayers(oat_rasters), nrow = 1))
  names(result) <- names(oat_rasters)

  for (i in 1:nlayers(oat_rasters)) {
    change_rate <- abs((oat_rasters[[i]] - wlc_raster) / wlc_raster)
    name <- paste0(names(oat_rasters[[i]]), ".tif")
    raster::writeRaster(x = change_rate, filename = paste0(output_dir, "/", name), format = "GTiff", overwrite = TRUE)

    mean_abs_change_rate <- raster::cellStats(x = change_rate, stat = "mean") * 100
    print(names(oat_rasters[[i]]))
    print(paste0(round(mean_abs_change_rate,digits = 2),"%"))

    result[1, i] <- paste0(round(mean_abs_change_rate,digits = 2))
  }

  utils::write.csv(x = result, file = paste0(output_dir, "/macr.csv"))
}

