% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/macr.R
\name{macr}
\alias{macr}
\title{Mean of Absolute Change Rates}
\usage{
macr(oat_rasters, wlc_raster, output_dir)
}
\arguments{
\item{oat_rasters}{Raster stacks.}

\item{wlc_raster}{Raster* object.}

\item{output_dir}{Either a character string naming a file or a connection open for writing.
"" indicates output to the console.}
}
\value{
raster Raster* object.

csv Mean of Absolute Change Rates(\%)
}
\description{
Input the results of the One-Factor-a-time at analysis of the main factor to
output its mean of absolute change rates.
}
\details{
The calculation formula can be found in this literature:
Wang H, Xiao J, Ouyang M, et al. Assessment of foot-and-mouth disease risk areas in
mainland China based spatial multi-criteria decision analysis. BMC Veterinary Research,
2021, 17: 1-12.
}
\examples{
\donttest{
#Calculate the mean of absolute change rates the factor Cattle.
library(raster)
#Read the results of the One-Factor-a-time of factor Cattle.
oat_rasters <- raster::stack(dir("//oat//cattle",full.names = TRUE))
#Read the decision map obtained with the weighted linear combination.
wlc_raster <- raster::raster("//wlc//decision_map.tif")
#All results are output in out_dir
macr_cattle <- macr(oat_rasters = oat_rasters,wlc_raster = wlc_raster,
                    output_dir = "//macr//cattle")
}
}
