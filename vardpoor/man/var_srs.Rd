\name{var_srs}

\alias{var_srs}

\title{The estimation of the simple random sampling.}

\description{Computes the estimation of the simple random sampling.}

\usage{var_srs(Y, w)}

\arguments{
\item{Y}{The variables of interest.}
\item{w}{Weight variable. One dimensional object convertible to one-column \code{data.frame}.}
}

\value{\code{data.frame} containing the values of the variance estimation of the simple random sampling.
}


\references{ 
Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr
}

\examples{
Ys <- matrix(rchisq(10, 3), 10, 1)
ws <- rep(2, 10)
var_srs(Ys, ws)
} 

\seealso{
\code{\link{vardom}}, \code{\link{vardomh}}, \code{\link{varpoord}}} 
\keyword{variance}

