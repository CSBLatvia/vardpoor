\name{vardcrospoor}

\alias{vardcrospoor}

\title{Variance estimation for cross-sectional, longitudinal measures for indicators on social exclusion and poverty.}

\description{Computes the variance estimation for cross-sectional and longitudinal measures for indicators on social exclusion and poverty.}

\usage{
vardcrospoor(Y, 
                     age=NULL,
                     pl085 = NULL,
                     month_at_work = NULL,
                     Y_den=NULL,
                     Y_thres = NULL,
                     wght_thres = NULL, 
                     H, PSU, w_final, id,
                     Dom = NULL,
                     country, periods,
                     sort=NULL,
                     gender = NULL,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     dataset = NULL,
                     use.estVar = FALSE,
                     withperiod = TRUE,
                     netchanges = TRUE,
                     confidence = .95,
                     type="linrmpg")
}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{age}{Age variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. }

\item{pl085}{Retirement variable (Number of months spent in retirement or early retirement). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{month_at_work}{Variable for total number of month at work (sum of the number of months spent at full-time work as employee, number of months spent at part-time work as employee, number of months spent at full-time work as self-employed (including family worker), number of months spent at part-time work as self-employed (including family worker)).  One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{Y_den}{Denominator variable (for example gross individual earnings). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
}

\item{Y_thres}{Variable (for example equalized disposable income) used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). Variable specified for \code{inc} is used as \code{income_thres} if \code{income_thres} is  not defined.}

\item{wght_thres}{Weight variable used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. Variable specified for \code{weight} is used as \code{wght_thres} if \code{wght_thres} is not defined.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{id}{variable for unit ID codes (for household surveys - secondary unit id number). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{country}{Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{periods}{Variable for the survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{gender}{Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{percentage}{A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
\deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.}

\item{order_quant}{A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
\deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.}

\item{alpha}{a numeric value in range \eqn{[0,100]} for the order of the income quantile share ratio (in percentage).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{use.estVar}{Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the estimation of covariance matrix of the residuals.}
 
\item{withperiod}{Logical value. If \code{TRUE} is value, the results is with period, if \code{FALSE}, without period.}

\item{netchanges}{Logical value. If value is TRUE, then produce two objects: the first object is aggregation of weighted data by period (if available), country, strata and PSU, the second object is an estimation for Y, the variance, gradient for numerator and denominator by country and period (if available). If value is FALSE, then both objects containing \code{NULL}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}

\item{type}{a character vector (of length one unless several.ok is TRUE), example "linarpr","linarpt", "lingpg", "linpoormed", "linrmpg",
   "lingini", "lingini2", "linqsr", "linarr", "linrmir", "all_choices".}
}

\value{

  A list with two objects are returned by the function:

  \item{data_net_changes}{ A \code{data.table} containing aggregation of weighted data by period (if available), country, strata, PSU.}

  \item{results}{ A \code{data.table} containing
       \code{period} -  survey periods, \cr
       \code{country} - survey countries, \cr
       \code{Dom} - optional variable of the population domains, \cr
       \code{type} - type variable, \cr
       \code{count_respondents} - the count of respondents, \cr
       \code{pop_size} - the population size (in numbers of individuals), \cr
       \code{estim} - the estimated value, \cr
       \code{se} - the estimated standard error, \cr
       \code{var} - the estimated variance, \cr
       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage.} 
}

\references{
Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{http://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC,
URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr

Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{http://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr

}

\seealso{
\code{\link{linrmir}}, \code{\link{linarr}}, \code{\link{vardchanges}}}


\examples{
data(eusilc)
set.seed(1)

data <- data.table(rbind(eusilc, eusilc),
                   year=c(rep(2010, nrow(eusilc)),
                          rep(2011, nrow(eusilc))),
                   country=c(rep("AT", nrow(eusilc)),
                             rep("AT", nrow(eusilc))))
data[age<0, age:=0]
PSU <- data[,.N, keyby="db030"]
PSU[, N:=NULL]
PSU[, PSU:=trunc(runif(nrow(PSU), 0, 100))]
PSU$inc <- runif(nrow(PSU), 20, 100000)
data <- merge(data, PSU, all=TRUE, by="db030")
PSU <- eusilc <- NULL
data[, strata:=c("XXXX")]
data[, strata:=as.character(strata)]
data$pl085 <- 12*trunc(runif(nrow(data),0,2))
data$month_at_work <- 12*trunc(runif(nrow(data),0,2))

result <- vardcrospoor(Y="inc", age="age",
                     pl085="pl085", 
                     month_at_work="month_at_work",
                     Y_den="inc", Y_thres="inc",
                     wght_thres="rb050",
                     H="strata", PSU="PSU", 
                     w_final="rb050",  id=NULL,
                     Dom = c("rb090", "db040"),
                     country="country", periods="year",
                     sort=NULL,
                     gender = NULL,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     dataset = data,
                     use.estVar = FALSE,
                     confidence=0.95,
                     type="linrmpg")

\dontrun{
result2 <- vardcrospoor(Y="inc",  age="age",
                     pl085="pl085", 
                     month_at_work="month_at_work",
                     Y_den="inc", Y_thres="inc",
                     wght_thres="rb050",
                     H="strata", PSU="PSU", 
                     w_final="rb050", id=NULL,
                     Dom = c("rb090", "db040"),
                     country="country", periods="year",
                     sort=NULL,
                     gender = NULL,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     dataset = data,
                     use.estVar = FALSE,
                     confidence=0.95,
                     type="linrmpg")}
}


\keyword{vardcros}
