\name{lin.ratio}

\alias{lin.ratio}

\title{Linearization of the ratio estimator}

\description{Computes linearized variable for the ratio estimator.}

\usage{lin.ratio(Y, Z, weight, Dom=NULL)}

\arguments{

\item{Y}{Matrix of numerator variables. Any object convertible to \code{data.table} with numeric values, \code{NA} values are not allowed.}

\item{Z}{Matrix of denominator variables. Any object convertible to \code{data.table} with numeric values, \code{NA} values are not allowed.}

\item{weight}{Weight variable. One dimensional object convertible to one-column \code{data.frame}}

\item{Dom}{Optional variables used to define population domains. If supplied, the linearized variables are computed for each domain. An object convertible to \code{data.table}}
}

\value{The function returns the \code{data.table} of the linearized variables for the ratio estimator.}

\references{ 
Carl-Erik Sarndal, Bengt Swensson, Jan Wretman. Model Assisted Survey Sampling. Springer-Verlag, 1992, p.178.
}

\examples{
Y <- data.table(rchisq(10, 3))
Z <- data.table(rchisq(10, 3))
weights <- rep(2, 10)
lin.ratio(Y, Z, weights)
}

\seealso{
  \code{\link{domain}},
  \code{\link{vardom}},
  \code{\link{vardomh}},
}

\keyword{survey}
