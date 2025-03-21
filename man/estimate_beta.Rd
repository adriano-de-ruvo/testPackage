% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myfunc.r
\name{estimate_beta}
\alias{estimate_beta}
\title{My second function}
\usage{
estimate_beta(y, X)
}
\arguments{
\item{y}{A numerical vector.}

\item{X}{A matrix.}
}
\value{
A numerical vector of \eqn{(X'X)^{-1} X' y}.
}
\description{
My second function
}
\examples{
X <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
y <- c(1, 2, 3)
myfunc(y, X)
}
