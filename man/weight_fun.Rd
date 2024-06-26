% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weight_fun.R
\name{weight_fun}
\alias{weight_fun}
\title{Calculate Weights from a Judgment Matrix}
\usage{
weight_fun(data)
}
\arguments{
\item{data}{A \code{data.frame} representing a judgment matrix from pairwise factor comparisons.}
}
\value{
A list containing two elements:
\itemize{
\item{decision_matrix}{The decision matrix normalized by column sums.}
\item{weight_factor}{Normalized weights of all factors, summing to 1.}
}
}
\description{
This function takes a judgment matrix derived from pairwise comparisons of factors and outputs normalized weights for all factors, ensuring the sum of all weights equals 1. The judgment matrix should have factors compared in rows and columns, with each cell representing the relative importance of a factor over another.
}
\examples{
\donttest{
a <- c(1, 3/4, 1/2)
b <- c(4/3, 1, 1/3)
c <- c(2, 3/2, 1)
data <- data.frame(a, b, c)
weights <- weight_fun(data)
print(weights)
}
}
