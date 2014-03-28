\name{linarpr}
\alias{linarpr}
\title{Linearization of the at-risk-of-poverty rate}
\usage{
  linarpr(inc, id, weight = NULL, income_thres=NULL, wght_thres=NULL,
          sort = NULL, Dom = NULL, period=NULL, dataset = NULL,
          percentage = 60, order_quant=50, na.rm=FALSE, var_name="lin_arpr")
}
\arguments{
  \item{inc}{either a numeric vector, 1 column data.frame,
  matrix, data.table giving the equivalized disposable
  income, or (if \code{dataset} is not \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.
  }
  \item{id}{optional; either 1 column data.frame, matrix,
  data.table with column names giving the personal IDs, or
  (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as
  'dataset' column count) specifying the corresponding
  column of \code{dataset}.
  }
  \item{weight}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal sample
  weights, or (if \code{dataset} is not  \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.
  }
  \item{income_thres}{either a numeric vector, 1 column
  data.frame, matrix, data.table giving the equivalized
  disposable income for computation and linearization
  of the poverty threshold, or (if \code{dataset} is not
  \code{NULL}) a character string, an integer or a logical
  vector (length is the same as 'dataset' column count)
  specifying the corresponding column of \code{dataset}.
  If \code{income_thres} is \code{NULL}, it is \code{inc}.
  }
  \item{wght_thres}{either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal sample
  weights for computation and linearization of the poverty
  threshold, or (if \code{dataset} is not \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}. If \code{wght_thres}
  is \code{NULL}, it is \code{weight}.
  }
  \item{sort}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal IDs to
  be used as tie-breakers for sorting, or (if \code{dataset}
  is not \code{NULL}) a character string, an integer or a
  logical vector (length is the same as 'dataset' column count)
  specifying the corresponding column of \code{dataset}.
  }
  \item{Dom}{optional; either a data.frame, matrix, data.table
  with column names giving different domains, or (if 
  \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{dataset}. If supplied, the values for
  each domain are computed.
  }
  \item{period}{optional; either a data.frame, matrix, data.table
  with column names giving different periods, or (if 
  \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{dataset}. If supplied, the values for
  each period are computed independently.
  }
  \item{dataset}{an optional; name of the individual dataset
  \code{data.frame}.
  }
  \item{percentage}{a numeric value in \eqn{[0,100]} giving the
  percentage of the income quantile to be used for the
  at-risk-of-poverty threshold (see \code{\link{linarpt}}).
  }
  \item{order_quant}{a numeric value in \eqn{[0,100]} giving the
  order of the income quintale (in percentage) to be used for the
  at-risk-of-poverty threshold (see \code{\link{linarpt}}).
  }
  \item{na.rm}{a logical indicating whether missing values should
  be removed.
  }
  \item{var_name}{a character string specifying the name of the
  linearized variable.}
}
\value{
  \item{quantile}{a \code{data.frame} containing the estimated 
  value of the quintale by totals.
  }
  \item{threshold}{a \code{data.frame} containing the estimated
  at-risk-of-poverty threshold by totals.
  }
  \item{value}{a \code{data.frame} containing the estimate(s) the
  at-risk-of-poverty rate by domain or (if \code{Dom} is \code{NULL})
  totals.
  }
  \item{lin}{a \code{data.frame} containing the values of linearized
  variables of the poverty rate (in precentage) by domains, or (if
  \code{Dom} is \code{NULL}) totals.
  }
}
\description{
  Estimate the at-risk-of-poverty rate, which is defined as the
  proportion of persons with equivalized disposable income below
  the at-risk-of-poverty threshold, and its linearization.
}
\details{
  The implementation strictly follows the Eurostat definition.
}
\examples{
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"
d<-linarpr("eqIncome", id="IDd", weight = "rb050", Dom = NULL,
         dataset = dati, percentage = 60, order_quant=50, na.rm = FALSE)

dd<-linarpr("eqIncome", id="IDd", weight = "rb050", Dom = "db040",
         dataset = dati, percentage = 60, order_quant=50, na.rm = FALSE)

}
\references{
  Working group on Statistics on Income and Living
  Conditions (2004) Common cross-sectional EU indicators
  based on EU-SILC; the gender pay gap.  \emph{EU-SILC
  131-rev/04}, Eurostat.\cr
  
  Guillaume Osier (2009). Variance estimation for complex
  indicators of poverty and inequality. \emph{Journal of
  the European Survey Research Association}, Vol.3, No.3,
  pp. 167-195, ISSN 1864-3361,
  URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

  Deville, J. C. (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}
\seealso{
  \code{\link{linarpt}}, \code{\link{varpoord}}, \code{\link{var_srs}}
}
\keyword{Linearization}



