\name{var_srs}

\alias{var_srs}

\title{The estimation of the simple random sampling.}

\description{Computes the estimation of the simple random sampling.}

\usage{var_srs(Y, w)}

\arguments{
\item{Y}{data.frame, matrix, data.table of the variable of interest; its length is equal to n, the sample size.}
\item{w}{vector or 1 column data.frame, matrix, data.table of the weights; its length is equal to n, the sample size.}
}

\value{a \code{data.frame} containing the values of the variance estimation of the simple random sampling by period.
}


\references{ 
   Pascal Ardilly and Guillaume Osier (2007). Cross-sectional
   variance estimation for the French Labour Force Survey  
   \emph{Journal of the European Survey Research Association},
   Vol.1 , No.2 , pp. 75-83, ISSN 1864-3361,
   URL \url{https://ojs.ub.uni-konstanz.de/srm/article/viewFile/77/55}. \cr
}

\examples{
Ys <- matrix(rchisq(10, 3), 10, 1)
ws <- rep(2, 10)
var_srs(Ys, ws)
} 

\seealso{
\code{\link{vardom}}, \code{\link{vardomh}}, \code{\link{varpoord}}} 
\keyword{variance}

