\name{vardbootstr}

\alias{vardbootstr}

\title{Variance estimation for measures of annual net change or annual for single stratified sampling designs}

\description{Computes the variance estimation for measures of annual net change or annual for single stratified sampling designs.}

\usage{
vardbootstr(boots_count = 500, Y, H, PSU,
            w_final, ID_level1, Z = NULL,
            Dom = NULL, dh = 1, fpc, dataset = NULL,
            years, subperiods = NULL, year1 = NULL,
            year2 = NULL, percentratio = 100,
            confidence = 0.95, method = "cros")
}

\arguments{

\item{boots_count}{Positive numeric value. Number of replicates, by default - 100}

\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{ID_level1}{Variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Z}{Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.table} or variable names as character, column numbers. This variable is \code{NULL} by default.}

\item{Dom}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{dh}{n_h-m_h, where n_h is the stratum size and m_h the number of units sampled with replacement. By default, dh=1 (HFCN recommendation)}

\item{fpc}{Variable for the finite population correction (sampling rate = n_h/N_h). Default = 0.}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{years}{Variable for the all survey years. The values for each year are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{subperiods}{Variable for the all survey subperiods. The values for each subperiod are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{year1}{The vector of years from variable \code{years} describes the first year for measures of annual net change.}

\item{year2}{The vector of years from variable \code{periods} describes the second year for measures of annual net change.}

\item{percentratio}{Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.}

\item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95.}

\item{method}{character value; value 'cros' is for measures of annual or value 'netchanges' is for measures of annual net change. This variable by default is netchanges.}

}

\value{
  
 A list with objects are returned by the function:

  \item{crossectional_results}{ A \code{data.table} containing:
       \code{year} -  survey years, \cr
       \code{subperiods} -  survey subperiods, \cr
       \code{variable} - names of variables of interest, \cr
       \code{Dom} - optional variable of the population domains, \cr
       \code{estim} - the estimated value, \cr
       \code{var} - the estimated variance of cross-sectional and longitudinal measures, \cr
       \code{sd_w} - the estimated weighted variance of simple random sample, \cr
       \code{se} - the estimated standard error of cross-sectional or longitudinal, \cr
       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
       \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
       \code{relative_margin_of_error} - the estimated relative margin of error, \cr
       \code{CI_lower} - the estimated confidence interval lower bound, \cr
       \code{CI_upper} - the estimated confidence interval upper bound, \cr 
       \code{confidence_level} - the positive value for confidence interval. }

 \item{annual_results}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1} for measures of annual net change, \cr
        \code{year_2} -  survey years of \code{years2} for measures of annual net change, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{variable} - names of variables of interest, \cr
        \code{estim_2} - the estimated value for period2 for measures of annual net change, \cr
        \code{estim_1} - the estimated value for period1 for measures of annual net change, \cr
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
Guillaume OSIER, Virginie RAYMOND, (2015), Development of methodology for the estimate of variance of annual net changes for LFS-based indicators. Deliverable 1 - Short document with derivation of the methodology. \cr
}

\examples{

### Example 
data("eusilc")
set.seed(1)
eusilc1 <- eusilc[1 : 20,]
set.seed(1)
require(data.table)
data <- data.table(rbind(eusilc1, eusilc1),
                   year = c(rep(2010, nrow(eusilc1)),
                            rep(2011, nrow(eusilc1))))
data[, half:= .I - 2 * trunc((.I - 1) / 2)]
data[, quarter:= .I - 4 * trunc((.I - 1) / 4)]
data[age < 0, age:= 0]
PSU <- data[, .N, keyby = "db030"][, N:= NULL]
PSU[, PSU:= trunc(runif(nrow(PSU), 0, 5))]
data <- merge(data, PSU, all = TRUE, by = "db030")
PSU <- eusilc <- NULL
data[, strata := c("XXXX")]

data[, employed := trunc(runif(nrow(data), 0, 2))]
data[, id_lv2 := paste0("V", .I)]
data[, fpc := 0]

\dontrun{
result <- vardbootstr(boots_count = 500, = "employed", H = "strata",
                      PSU = "PSU", w_final = "rb050", ID_level1 = "ids",
                      Z = NULL, Dom = NULL, dh = 1, fpc = "fpc",
                      dataset = data, years = "year",
                      subperiods = "half", year1 = 2010,
                      year = 2011, percentratio = 100,
                      confidence = 0.95, method = "netchanges") }
} 

\seealso{
\code{\link{vardchanges}}, \code{\link{vardannual}}}
\keyword{vardannual}