\name{linpoormed}

\alias{linpoormed}

\title{Linearization of the median income below the at-risk-of-poverty threshold}


\description{

Estimation of the median income below at-risk-of-poverty threshold and computation of linearized variable for variance estimation. The at-risk-of-poverty threshold is estimated for the whole population always. The median income is estimated for the whole population or for each domain.
}

\usage{
linpoormed(inc, id, weight=NULL, sort=NULL,
           Dom=NULL, period=NULL, dataset = NULL,
           percentage=60, order_quant=50,
           var_name="lin_poormed")
}

\arguments{

  \item{inc}{Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{weight}{Optional weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the median income of persons below a poverty threshold is done for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{period}{Optional variable for survey period. If supplied, linearization of the median income of persons below a poverty threshold is done for each time period. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{dataset}{Optional survey data object convertible to \code{data.frame}.}

  \item{percentage}{A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
  \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
  For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.}
  
  \item{order_quant}{A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
  \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
  For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.}

  \item{var_name}{A character specifying the name of the linearized variable.}
}

\value{
  A list with two objects are returned by the function:

  \item{value}{A \code{data.frame} containing the estimated median income below the at-risk-of-poverty gap.}
  
  \item{lin}{A \code{data.frame} containing the linearized variables of the median income below the at-risk-of-poverty gap.}
}

\references{
Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators
based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.\cr
  
Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"

# Full population
d <- linpoormed(inc="eqIncome", id="IDd", weight = "rb050", Dom=NULL,
                dataset=dati, percentage=60, order_quant=50)

\dontrun{
# Domains by location of houshold
dd <- linpoormed(inc="eqIncome", id="IDd", weight="rb050", Dom="db040",
                dataset=dati, percentage=60, order_quant=50)

}
}

\seealso{
\code{\link{linarpt}}, \code{\link{linrmpg}}, \code{\link{varpoord}}}

\keyword{Linearization}
