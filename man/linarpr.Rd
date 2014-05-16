\name{linarpr}

\alias{linarpr}

\title{Linearization of the at-risk-of-poverty rate}

\usage{
linarpr(inc, id, weight = NULL,
  income_thres = NULL, wght_thres = NULL,
  sort = NULL, Dom = NULL, period = NULL, dataset = NULL,
  percentage = 60, order_quant = 50, na.rm = FALSE,
  var_name = "lin_arpr")
}

\arguments{

  \item{inc}{Study variable (for example equivalized disposable income). One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{id}{Optional variable for unit ID codes. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{weights}{Optional weight variable. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{income_thres}{Variable (for example equivalized disposable income) used for computation and linearization of poverty threshold. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). Variable spcified for \code{inc} is used as \code{income_thres} if \code{income_thres} is  not defined.}

  \item{wght_thres}{Weight variable used for computation and linearization of poverty threshold. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). Variable spcified for \code{weight} is used as \code{wght_thres} if \code{wght_thres} is  not defined.}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the at-risk-of-poverty rate is done for each domain. An object convertable to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{period}{Optional variable for survey period. If supplied, linearization of the at-risk-of-poverty rate is done for each time period. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{dataset}{Optional survey data object convertable to \code{data.frame}.}

  \item{percentage}{A numeric value in \eqn{[0,100]} for \eqn{p}{p} in the formula for poverty threshold computation
  \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}{p/100 * Z(alpha/100)}}

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



