\name{lingini}

\alias{lingini}

\title{Linearization of the GINI coefficient I}

\description{
  Estimate the Gini coefficient, which is a measure for inequality, and its linearization.
}

\usage{
  lingini(Y, ID_level2 = NULL, weight = NULL,
          sort = NULL, Dom = NULL, period = NULL,
          dataset = NULL, var_name = "lin_gini")
}

\arguments{

  \item{Y}{Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

  \item{ID_level2}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

  \item{weight}{Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the GINI is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

  \item{period}{Optional variable for survey period. If supplied, linearization of the GINI is done for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.}

  \item{dataset}{Optional survey data object convertible to \code{data.table}.}

  \item{var_name}{A character specifying the name of the linearized variable.}
}

\value{

  A list with two objects are returned by the function:

  \item{value}{A \code{data.table} containing the estimated Gini coefficients (in percentage) by G. Osier and Eurostat.}
  
  \item{lin}{A \code{data.table} containing the linearized variables of the Gini coefficients (in percentage) by G. Osier.}

}

\references{
Working group on Statistics on Income and Living  Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.\cr
  
Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dati <- data.table(IDd = 1 : nrow(eusilc), eusilc)[1 : 3,]

# Full population
dat1 <- lingini(Y = "eqIncome", ID_level2 = "IDd", weight = "rb050", dataset = dati)
dat1$value

\dontrun{
# By domains
dat2 <- lingini(Y = "eqIncome", ID_level2 = "IDd", weight = "rb050", Dom = c("db040"), dataset = dati)
dat2$value}
}


\seealso{
\code{\link{lingini2}}, \code{\link{linqsr}},  \code{\link{varpoord}} , \code{\link{vardcrospoor}}, \code{\link{vardchangespoor}}}

\keyword{Linearization}

