#' @title One-Factor-At-a-Time
#' @description By changing the weight value of only one factor at a time,
#'     while keeping the other factors as constant as possible,
#'     the degree and regularity of the effect of the change in the weight of
#'     a single factor on the results is reflected.
#' @details The formula of One-Factor-At-a-Time is wi=(1-wa)*wi0/1-wa0 1≤i≤n,i≠a
#' wi0 is the initial weight of each risk factor and wa0 is the initial
#' weight of the major changing risk factor. The sum of all weights is equal to 1.
#' @param rasters Raster* object.
#' @param weights The weight of each factor should correspond to the order of the rasters.
#' The main factor needs to be ranked first.
#' @param range Range of factor weight adjustment.
#' @param step The step size of the factor weight adjustment.
#' @param output_dir Either a character string naming a file or a connection open for writing.
#' "" indicates output to the console.
#' @return raster Raster* object.
#' @importFrom raster writeRaster
#' @importFrom raster nlayers
#' @export
#' @examples
#' \donttest{
#' library(raster)
#' #cattle as the main factor
#' rasters <- raster::stack("1_cattle.tif",
#'                          "2_pig.tif",
#'                          "3_sheep.tif")
#' #The order of the weights is aligned with the order of the rasters
#' weights <- c(0.2,0.3,0.5)
#' #Range of weight adjustment (%)
#' range <- c(-10,10)
#' #Step size of weight adjustment (%)
#' step <- 1
#' #All results are output in out_dir
#' oat(rasters = rasters,weights = weights,
#' range = range,step = step,output_dir = "oat//1_cattle")
#' }

oat <- function(rasters, weights, range, step, output_dir) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop("The 'raster' package is required but not installed.")
  }

  if (length(weights) != raster::nlayers(rasters)) {
    stop("The number of weights must match the number of layers in the raster stack.")
  }

  base_weight <- weights[1] # Base weight of the main factor
  for(i in seq(range[1], range[2], step)) {
    main_factor_weight <- base_weight + base_weight * i * 0.01
    remaining_weight_sum <- 1 - main_factor_weight
    weighted_sum <- rasters[[1]] * main_factor_weight

    for (a in 2: raster::nlayers(rasters)) {
      adjusted_weight <- remaining_weight_sum * weights[a] / (1 - base_weight)
      weighted_sum <- weighted_sum + raster::subset(rasters, a) * adjusted_weight
    }
    output_file <- paste0(output_dir,"/result_", i, ".tif")
    raster::writeRaster(weighted_sum, filename = output_file, format = "GTiff", overwrite = TRUE)
    print(output_file)
  }
}

