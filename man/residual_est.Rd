\name{residual_est}
\alias{residual_est}
\title{Residual estimation of calibration}
\description{Computes the estimation residuals of calibration.}
\usage{residual_est(Y, X, weight, q, period=NULL, ind_gr)}
\arguments{
\item{Y}{matrix of the variable of interest; its length is equal to n, the sample size.}
\item{X}{matrix of the auxiliary variables; for the calibration estimator, this is the matrix of the sample calibration variables.} 
\item{weight}{vector of the weights; its length is equal to n, the sample size.}
\item{q}{vector of positive values accounting for heteroscedasticity; its size is n, the sample size.}  
\item{period}{optional; either a data.frame, matrix, data.table
  with column names giving different periods, or (if 
  \code{datasetX} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'datasetX' column count) specifying the corresponding
  columns of \code{datasetX}. If supplied, the values for
  each period are computed independently.
  }
\item{ind_gr}{optional; either a vector, 1 column data.frame, matrix,
  data.table giving the variable by which divided independently
  auxiliary variables, or (if \code{datasetX} is not
  \code{NULL}) a character string,  an integer or a logical vector
  (length is the same as 'datasetX' column count) specifying the
  corresponding column of \code{datasetX}. This variable is \code{NULL}
  by default.}
}
\value{
  A numeric data.frame containing the estimated residuals of calibration.
}
\details{
The function implements the following estimator:
\deqn{e_k=Y_k-X_k^{'}B }
where
\deqn{\hat{B} = \left(\sum_{s} weight_k q_k X_k X^{'}_{k} \right)^{-1} \left(\sum_{s} weight_k q_k X_k Y_k \right)}.}

\references{ 
Sixten Lundstrom and Carl-Erik Sarndal. Estimation in the presence of Nonresponse and 
Frame Imperfections. Statistics Sweden, 2001, p. 43-44.
}
\examples{
Y <- matrix(rchisq(10, 3), 10, 1)
X <- matrix(rchisq(20, 3), 10, 2)
w <- rep(2, 10)
q <- rep(1, 10)
residual_est(Y, X, w, q)

### Test2
Y <- matrix(rchisq(10, 3), 10, 1)
X <- matrix(c(rchisq(10, 2), rchisq(10, 2) + 10), 10, 2)
w <- rep(2, 10)
q <- rep(1, 10)
residual_est(Y, X, w, q)
as.matrix(lm(Y ~ X - 1, weights = w * q)$residuals)
} 
\seealso{
   \code{\link{domain}},   \code{\link{lin.ratio}},    \code{\link{linarpr}},
   \code{\link{linarpt}},  \code{\link{lingini}},      \code{\link{lingini2}},
   \code{\link{lingpg}},   \code{\link{linpoormed}},   \code{\link{linqsr}},
   \code{\link{linrmpg}},  \code{\link{vardom}},       \code{\link{vardomh}},
   \code{\link{varpoord}}, \code{\link{variance_est}}, \code{\link{variance_othstr}}
}
\keyword{survey}
