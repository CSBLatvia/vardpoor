\name{incPercentile}
\alias{incPercentile}
\title{Weighted income percentile}
\usage{
  incPercentile(inc, weights = NULL, sort = NULL,
    Dom = NULL, k = c(20, 80), dataset = NULL, na.rm = FALSE)
}
\arguments{
  \item{inc}{either a numeric vector, 1 column data.frame,
  matrix, data.table giving the (equivalized disposable)
  income, or (if \code{dataset} is not \code{NULL}) a character
  string, an integer or a logical vector (length is the same
  as 'dataset' column count) specifying the corresponding
  column of \code{dataset}.
  }
  \item{weights}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal sample
  weights, or (if \code{dataset} is not \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.
  }
  \item{sort}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal IDs to
  be used as tie-breakers for sorting, or (if \code{dataset}
  is not \code{NULL}) a character string, an integer or a
  logical vector (length is the same as 'dataset' column
  count) specifying the corresponding column of \code{dataset}.
  }
  \item{Dom}{optional; either a numeric data.frame, matrix,
  data.table with column names giving different domains, or
  (if \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 'dataset'
  column count)specifying the corresponding columns of 
  \code{dataset}.  If supplied, the values for each domain
  are computed.
  }
  \item{k}{a vector of integers between 0 and 100 specifying
  the percentiles to be computed (0 gives the minimum, 100 the
  maximum).
  }
  \item{dataset}{an optional; name of the individual dataset
  \code{data.frame}.}

  \item{na.rm}{a logical indicating whether missing values
  should be removed.}
}
\value{
  A numeric vector (if \code{Dom} is \code{NULL}) or
  data.frame (if \code{Dom} is not \code{NULL}) containing
  the values of the weighted income percentiles specified by
  \code{k} are returned.
}
\description{
  Compute weighted income percentiles.
}
\references{
  Working group on Statistics on Income and Living
  Conditions (2004) Common cross-sectional EU indicators
  based on EU-SILC; the gender pay gap.  \emph{EU-SILC
  131-rev/04}, Eurostat.
}
\examples{
data(eusilc)
incPercentile("eqIncome", weights = "rb050",Dom = "db040", data = eusilc)
}
\seealso{
\code{\link{linarpt}}, \code{\link{linarpr}}, \code{\link{linqsr}}
}
\keyword{Linearization}

