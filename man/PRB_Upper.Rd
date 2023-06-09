% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/real.R
\name{PRB_Upper}
\alias{PRB_Upper}
\title{Price Related Bias}
\usage{
PRB_Upper(x, y)
}
\arguments{
\item{x}{Appraisal value}

\item{y}{Sales price}
}
\value{
PRB Upper
}
\description{
calculates the bottom confidence interval.
}
\examples{
x <- c(25500,57000,39000,90000,51000,93000,49500);
y <- c(75000,150000,90000,180000,90000,150000,75000);
prbu <- PRB_Upper(x,y);
}
