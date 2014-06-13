\name{linqsr}

\alias{linqsr}

\title{Linearization of the income quintile share ratio}

\description{
  Estimate the quintile share ratio, which is defined as the ratio of the sum of equivalized disposable income received by the top 20\% to the sum of equivalized disposable income received by the bottom 20\%, and its linearization.
}

\usage{
linqsr(inc, id=NULL, weight=NULL, sort = NULL,
       Dom = NULL, period=NULL, dataset = NULL,
       alpha = 20, na.rm = FALSE, var_name="lin_qsr")
}

\arguments{

  \item{inc}{Study variable (for example equivalized disposable income). One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{id}{Optional variable for unit ID codes. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{weight}{Optional weight variable. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the at-risk-of-poverty rate is done for each domain. An object convertable to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{period}{Optional variable for survey period. If supplied, linearization of the at-risk-of-poverty rate is done for each time period. One dimentional object convertable to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{dataset}{Optional survey data object convertable to \code{data.frame}.}

  \item{alpha}{a numeric value in range \eqn{[0,100]} for the order of the income quantile share ratio (in percentage).}

  \item{na.rm}{A logical value indicating whether missing values in study variable should be removed.}

  \item{var_name}{A character specifying the name of the linearized variable.}
}


\value{
  A list with two objects are returned by the function:
  
  \item{value}{A \code{data.frame} containing the estimated income quintile share ratio (in percentage). The estimates are done by domains if \code{Dom} is not \code{NULL}.}
  
  \item{lin}{A \code{data.frame} containing the linearized variables of the income quintile share ratio (in percentage). The linearized variables are computed by domains if \code{Dom} is not \code{NULL}.}
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

  Jean-Claude Deville (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dati <- data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"
dd<-linqsr("eqIncome", id="IDd", weight="rb050", Dom = "db040",
            dataset= dati, alpha=20, na.rm = FALSE)
dd$value
}

\seealso{
\code{\link{incPercentile}}, \code{\link{varpoord}}}
\keyword{Linearization}

