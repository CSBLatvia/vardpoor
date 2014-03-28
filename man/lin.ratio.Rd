\name{lin.ratio}
\alias{lin.ratio}
\title{Linearization of the ratio estimator}
\description{Computes linearized variables of the ratio estimator.}
\usage{lin.ratio(Y, Z, weight, Dom=NULL)}
\arguments{
\item{Y}{data.frame, matrix, data.table with column names of the variable of interest; its length is equal to n, the sample size.}
\item{Z}{data.frame, matrix, data.table with column names of auxiliary information; its length is equal to n, the sample size.}
\item{weight}{vector or 1 column data.frame, matrix, data.table of the weights; its length is equal to n, the sample size.}
\item{Dom}{optional; either a data.frame, matrix, data.table
  with column names giving different domains, or (if 
  \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{dataset}. If supplied, the values for
  each domain are computed.
  }
}
\value{The function returns the data.table of the linearized variables of the ratio estimator.}

\references{ 
  Guillaume Osier and Emilio Di Meglio. The linearisation approach
  implemented by Eurostat for the first wave of EU-SILC: what could
  be done from the second wave onwards? 2012

  Guillaume Osier (2009). Variance estimation for complex
  indicators of poverty and inequality. \emph{Journal of
  the European Survey Research Association}, Vol.3, No.3,
  pp. 167-195, ISSN 1864-3361, 
  URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr
}

\examples{
Y <- data.table(rchisq(10, 3))
Z <- data.table(rchisq(10, 3))
weightss <- rep(2, 10)
lin.ratio(Y, Z, weightss)
}
\seealso{
  \code{\link{domain}},   \code{\link{residual_est}},
  \code{\link{vardom}},   \code{\link{vardomh}},
  \code{\link{var_srs}},  \code{\link{variance_est}}
  \code{\link{variance_othstr}}
}

\keyword{survey}
