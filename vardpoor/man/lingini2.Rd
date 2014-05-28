\name{lingini2}
\alias{lingini2}
\title{Linearization of the GINI coefficient II}
\usage{
  lingini2(inc, id = NULL, weight = NULL, sort = NULL, Dom = NULL,
          period=NULL, dataset = NULL, na.rm = FALSE, var_name="lin_gini2")
}
\arguments{
  \item{inc}{either a numeric vector, 1 column data.frame,
  matrix, data.table giving the equivalized disposable
  income, or (if \code{dataset} is not \code{NULL})
  a character string, an integer or a logical vector
  (length is the same as 'dataset' column count) specifying
  the corresponding column of \code{dataset}.}

  \item{id}{optional; either 1 column data.frame, matrix,
  data.table with column names giving the personal IDs, or
  (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as
  'dataset' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{weight}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal sample
  weights, or (if \code{dataset} is not \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.}

  \item{sort}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal IDs
  to be used as tie-breakers for sorting, or (if \code{dataset}
  is not \code{NULL}) a character string, an integer or a
  logical vector (length is the same as 'dataset' column
  count) specifying the corresponding column of \code{dataset}.}

  \item{Dom}{optional; either a data.frame, matrix,
  data.table with column names giving different domains, or
  (if \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as
  'dataset' column count)specifying the corresponding columns
  of \code{dataset}.  If supplied, the values for each domain
  are computed.}
  
  \item{period}{optional; either a data.frame, matrix, data.table
  with column names giving different periods, or (if 
  \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{dataset}. If supplied, the values for
  each period are computed independently.
  }

  \item{dataset}{an optional; name of the individual dataset
  \code{data.frame}.}

  \item{na.rm}{a logical indicating whether missing values
  should be removed.}

  \item{var_name}{a character string specifying the name of the 
  linearized variable.}
}
\value{
 The function returns two values: 
  \item{value}{a \code{data.frame} containing the estimate(s)
  the gini coeffients (in percentage) by domain, or (if
  \code{Dom} is \code{NULL}) totals, using Tille definition
  and Eurostat definition.}

  \item{lin}{a \code{data.frame} containing the values of
  linearized variables of the GINI coefficient (in precentage)
  by domains or (if \code{Dom} is \code{NULL}) totals.}

}
\description{
  Estimate the Gini coefficient, which is a measure for
  inequality, and its linerization.
}

\examples{
data(eusilc)
dati <- data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"
dat1 <- lingini2("eqIncome","IDd",weight = "rb050",Dom=c("db040"),dataset=dati)
dat2 <- lingini2("eqIncome","IDd",weight = "rb050",dataset=dati)

dat1$value
dat2$value
}

\references{
  Working group on Statistics on Income and Living
  Conditions (2004) Common cross-sectional EU indicators
  based on EU-SILC; the gender pay gap.  \emph{EU-SILC
  131-rev/04}, Eurostat.\cr
  
  MATTI LANGEL - YVES TILLE, Corrado Gini, a pioneer in balanced sampling
  and inequality theory. \emph{METRON - International Journal of Statistics},
  2011, vol. LXIX, n. 1, pp. 45-65, 
  URL \url{ftp://metron.sta.uniroma1.it/RePEc/articoli/2011-1-3.pdf}. \cr

  Deville, J. C. (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}
\seealso{
  \code{\link{lingini}}, \code{\link{varpoord}},
  \code{\link{linqsr}}, \code{\link{var_srs}}
}
\keyword{Linearization}
