% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_weight.R
\name{plot_weight}
\alias{plot_weight}
\title{Create a weighting plot of the factors}
\usage{
plot_weight(data)
}
\arguments{
\item{data}{List,the weights of all factors obtained by weight_fun().}
}
\value{
A weighting plot of the factors
}
\description{
Input the weights of all factors obtained by weight_fun() to obtain a weight plot.
}
\examples{
\donttest{
a <- c(1,3/4,1/2)
b <- c(4/3,1,1/3)
c <- c(2,3/2,1)
data <- data.frame(a,b,c)
weight_factor <- weight_fun(data)$weight_factor
plot_weight(weight_factor)
}
}
