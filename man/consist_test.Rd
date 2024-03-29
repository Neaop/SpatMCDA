% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consist_test.R
\name{consist_test}
\alias{consist_test}
\title{Output consistency test results}
\usage{
consist_test(λ, n)
}
\arguments{
\item{λ}{Numeric, calculated by aw_weight().}

\item{n}{Integer, the number of factors.}
}
\value{
Results of the consistency test.
}
\description{
Input λ and the number of factors to
get the results of the consistency test.
}
\details{
A maximum of 15 factors can be tested.
The reference value of RI is c(0,0,0.52,0.89,1.12,1.26,1.36,1.41,1.46,1.49,1.52,1.54,1.56,1.58,1.59).
}
\examples{
\donttest{
λ <- 1.5
n <- 10
consist_test(λ,n)
}
}
