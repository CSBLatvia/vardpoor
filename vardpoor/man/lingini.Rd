\name{lingini}

\alias{lingini}

\title{Linearization of the GINI coefficient I}

\description{
  Estimate the Gini coefficient, which is a measure for inequality, and its linearization.
}

\usage{
  lingini(inc, id = NULL, weight = NULL,
          sort = NULL, Dom = NULL, period=NULL,
          dataset = NULL, var_name="lin_gini")
}

\arguments{

  \item{inc}{Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{weight}{Optional weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the GINI is done for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{period}{Optional variable for survey period. If supplied, linearization of the GINI is done for each time period. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{dataset}{Optional survey data object convertible to \code{data.frame}.}

  \item{var_name}{A character specifying the name of the linearized variable.}
}

\value{

  A list with two objects are returned by the function:

  \item{value}{A \code{data.frame} containing the estimated GINI coefficients (in percentage) by G. Osier and Eurostat papers.}
  
  \item{lin}{A \code{data.frame} containing the linearized variables of the GINI coefficients (in percentage) by G. Osier paper.}

}

\references{
Working group on Statistics on Income and Living  Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.\cr
  
Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dati <- data.frame(1:nrow(eusilc),eusilc)[1:3,]
colnames(dati)[1] <- "IDd"

# Full population
dat1 <- lingini(inc="eqIncome", id="IDd", weight="rb050", dataset=dati)
dat1$value

\dontrun{
# By domains
dat2 <- lingini(inc="eqIncome", id="IDd", weight="rb050", Dom=c("db040"), dataset=dati)
dat2$value}
}

\seealso{
\code{\link{lingini2}}, \code{\link{linqsr}}, \code{\link{varpoord}}}
\keyword{Linearization}

