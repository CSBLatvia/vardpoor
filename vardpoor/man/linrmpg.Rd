\name{linrmpg}

\alias{linrmpg}

\title{Linearization of the relative median at-risk-of-poverty gap}

\description{
  Estimate the relative median at-risk-of-poverty gap, which is defined as the relative difference between the median equalized disposable income of persons below the at-risk-of-poverty threshold and the at-risk-of-poverty threshold itself (expressed as a percentage of the at-risk-of-poverty threshold) and its linearization.
}

\usage{
  linrmpg(inc, id, weight=NULL, sort=NULL, Dom=NULL,
          period=NULL, dataset = NULL, percentage=60,
          order_quant=50, var_name="lin_rmpg")

}

\arguments{
  \item{inc}{Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{weight}{Optional weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{Dom}{Optional variables used to define population domains. If supplied, linearization of the relative median at-risk-of-poverty gap is done for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

  \item{period}{Optional variable for survey period. If supplied, linearization of the relative median at-risk-of-poverty gap is done for each time period. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

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

\item{value}{A \code{data.frame} containing the estimated relative median at-risk-of-poverty gap (in percentage).}
  
\item{lin}{A \code{data.frame} containing the linearized variables of the relative median at-risk-of-poverty gap (in precentage).}
}


\references{
Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.\cr
  
Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"

# Full population
d<-linrmpg(inc="eqIncome", id="IDd", weight="rb050", Dom=NULL,
           dataset=dati, percentage=60, order_quant=50)
d$value
d$threshold

\dontrun{
# By domains
dd<-linrmpg(inc="eqIncome", id="IDd", weight = "rb050", Dom="db040",
            dataset=dati, percentage=60, order_quant=50)
dd$value}
}

\seealso{
\code{\link{linarpt}}, \code{\link{linarpr}},  \code{\link{linpoormed}}, \code{\link{varpoord}}}
\keyword{Linearization}

