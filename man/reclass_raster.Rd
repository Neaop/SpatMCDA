% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reclass_raster.R
\name{reclass_raster}
\alias{reclass_raster}
\title{Reclassify values in a raster}
\usage{
reclass_raster(raster, rcl)
}
\arguments{
\item{raster}{Raster* object.}

\item{rcl}{Matrix for reclassification. This matrix has 3 columns.
The first two columns are the input values "from" - "to" and the third column "becomes" is the new value for that range.}
}
\value{
Raster* object.
}
\description{
Reclassify values of a Raster* object.
The function (re)classifies groups of values to other values.
For example, all values between 1 and 50 become 1, and all values between 51 and 100 become 2.
Reclassification is done with matrix rcl, in the row order of the reclassify table.
}
\examples{
\donttest{
library(raster)
raster <- raster(ncols=30, nrows=15)
values(raster) <- runif(ncell(raster),0,100)
# all values > 0 and <= 30 become 0.3, etc.
rcl <- c(0, 30, 0.3,  30, 60, 0.6,  60, 100, 1)
rcl <- matrix(rcl, ncol=3, byrow=TRUE)
reclass_raster(raster,rcl)
}
}
