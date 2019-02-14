\name{vardchangstrs}

\alias{vardchangstrs}

\title{Variance estimation for measures of change for stratified simple random sampling}

\description{Computes the variance estimation for measures of change for stratified simple random sampling.}

\usage{
vardchangstrs(Y, H, PSU, w_final,
              Dom = NULL, periods = NULL,
              dataset, periods1, periods2,
              in_sample, in_frame,
              confidence = 0.95,
              percentratio = 1)
}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{periods}{Variable for the all survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{periods1}{The vector of periods from variable \code{periods} describes the first period for measures of change.}

\item{periods2}{The vector of periods from variable \code{periods} describes the second period for measures of change.}

\item{in_sample}{Sample variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{in_frame}{Frame variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{percentratio}{Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.}

\item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95.}

}

\value{
  
  A list with objects are returned by the function:

 \item{annual_sum}{ A \code{data.table} containing
        \code{year} - survey years, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{totalY} - the estimated value of variables of interest for period1, \cr
        \code{totalZ} - optional the estimated value of denominator for period2, \cr
        \code{year} -  survey years of \code{years} for measures of annual, \cr
        \code{year_1} -  survey years of \code{years1} for measures of annual net change, \cr
        \code{year_2} -  survey years of \code{years2} for measures of annual net change, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{estim_1} - the estimated value for period1 for measures of annual net change, \cr
        \code{estim_2} - the estimated value for period2 for measures of annual net change, \cr
        \code{estim} - the estimated value, \cr
        \code{var} - the estimated variance, \cr
        \code{se} - the estimated standard error, \cr
        \code{rse} - the estimated relative standard error (coefficient of variation), \cr
        \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
        \code{absolute_margin_of_error} - the estimated absolute margin of error for period1 for measures of annual, \cr
        \code{relative_margin_of_error} - the estimated relative margin of error in percentage for measures of annual, \cr
        \code{CI_lower} - the estimated confidence interval lower bound, \cr
        \code{CI_upper} - the estimated confidence interval upper bound, \cr
        \code{confidence_level} - the positive value for confidence interval, \cr 
        \code{significant} - is the the difference significant}

}

\references{
Nordberg, L. ((2000), On Variance Estimation for Measures of Change When Samples are Coordinated by the Use of Permanent Random Numbers. Journal of Official Statistics 16, 363?378

}

\examples{

### Example 
data("eusilc")
set.seed(1)
eusilc1 <- eusilc[1 : 100,]
set.seed(1)
datas <- data.table(rbind(eusilc1, eusilc1),
                     year = c(rep(2010, nrow(eusilc1)),
                              rep(2011, nrow(eusilc1))))

PSU <- datas[, .N, keyby = "db030"][, N:= NULL]
PSU[, PSU:= trunc(runif(nrow(PSU), 0, 5))]
datas <- merge(datas, PSU, all = TRUE, by = "db030")
datas[, strata := c("XXXX")]

datas[, calc := trunc(runif(nrow(datas), 0, 2))]
datas[, id_lv2 := paste0("V", .I)]

\dontrun{
result <- vardchangstrs(Y = "calc", H = "strata",
                        PSU = "PSU", w_final = "rb050",
                        ID_level1 = "db030", ID_level1 = "id_lv2",
                        periods = "year", dataset = datas,
                        periods1 = 2010, periods2 = 2011,
                        percentratio = 100, confidence = 0.95) }
}

\seealso{
\code{\link{domain}}}
\keyword{vardchangstrs}
