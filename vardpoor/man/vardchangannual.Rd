\name{vardchangannual}

\alias{vardchangannual}

\title{Variance estimation for measures of annual net change for single and multistage stage cluster sampling designs}

\description{Computes the variance estimation for measures of annual net change for single and multistage stage cluster sampling designs.}

\usage{
vardchangannual(Y, H, PSU, w_final, id,
            Dom = NULL, Z = NULL, 
            country, years, subperiods,
            dataset = NULL,
            year1, year2,
            percentratio = 1,
            use.estVar = FALSE,
            confidence = 0.95) }

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{id}{variable for unit ID codes (for household surveys - secondary unit id number). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{Z}{Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.table} or variable names as character, column numbers. This variable is \code{NULL} by default.}

\item{country}{Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{years}{Variable for the all survey years. The values for each year are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{subperiods}{Variable for the all survey subperiods. The values for each subperiod are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{year1}{The vector of years from variable \code{years} describes the first year.}

\item{year2}{The vector of years from variable \code{periods} describes the second year.}

\item{percentratio}{Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.}

\item{use.estVar}{Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the estimation of covariance matrix of the residuals.}
 
\item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95 .}

}

\value{
  
  A list with objects are returned by the function:

  \item{crossectional_results}{ A \code{data.table} containing:
       \code{year} -  survey years, \cr
       \code{subperiods} -  survey subperiods, \cr
       \code{country} - survey countries, \cr
       \code{Dom} - optional variable of the population domains, \cr
       \code{namesY} - variable with names of variables of interest, \cr
       \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
       \code{sample_size} - the sample size (in numbers of individuals), \cr
       \code{pop_size} - the population size (in numbers of individuals), \cr
       \code{total} - the estimated totals, \cr
       \code{variance} - the estimated variance of cross-sectional or longitudinal measures, \cr
       \code{sd_w} - the estimated weighted variance of simple random sample, \cr
       \code{sd_nw} - the estimated variance estimation of simple random sample, \cr
       \code{pop} - the population size (in numbers of households), \cr
       \code{sampl_siz} - the sample size (in numbers of households), \cr
       \code{stderr_w} - the estimated weighted standard error of simple random sample, \cr
       \code{stderr_nw} - the estimated standard error of simple random sample, \cr
       \code{se} - the estimated standard error of cross-sectional or longitudinal, \cr
       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
       \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
       \code{relative_margin_of_error} - the estimated relative margin of error, \cr
       \code{CI_lower} - the estimated confidence interval lower bound, \cr
       \code{CI_upper} - the estimated confidence interval upper bound.}
 
 \item{crossectional_var_grad}{ A \code{data.table} containing
        \code{year} -  survey years, \cr
        \code{subperiods} -  survey subperiods, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{grad} - the estimated gradient, \cr
        \code{var} - the estimated a design-based variance.}

 \item{vardchanges_grad_var}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{subperiods_1} -  survey subperiods of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{subperiods_2} -  survey subperiods of \code{years2}, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{nams} - gradient names, numenator (num) and denominator (den), for each year, \cr 
        \code{grad} - the estimated gradient, \cr
        \code{cros_var} - the estimated a design-based variance.}

  \item{vardchanges_rho}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{subperiods_1} -  survey subperiods of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{subperiods_2} -  survey subperiods of \code{years2}, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{nams} - gradient names, numenator (num) and denominator (den), for each year, \cr
        \code{rho} - the estimated correlation matrix.}

  \item{vardchanges_var_tau}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{subperiods_1} -  survey subperiods of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{subperiods_2} -  survey subperiods of \code{years2}, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{nams} - gradient names, numenator (num) and denominator (den), for each year, \cr
        \code{var_tau} - the estimated covariance matrix.}

  \item{vardchanges_results}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{subperiods_1} -  survey subperiods of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{subperiods_2} -  survey subperiods of \code{years2}, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{estim_1} - the estimated value for period1, \cr
        \code{estim_2} - the estimated value for period2, \cr
        \code{estim} - the estimated value, \cr
        \code{var} - the estimated variance, \cr
        \code{se} - the estimated standard error, \cr
        \code{CI_lower} - the estimated confidence interval lower bound, \cr
        \code{CI_upper} - the estimated confidence interval upper bound, \cr
        \code{significant} - is the the difference significant}

 \item{X_annual}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{year} -  survey years, \cr
        \code{country} - survey countries, \cr
        \code{period} - period1 and period2 together, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{cros_se} - the estimated cross-sectional standard error.}

 \item{A_matrix}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{cols} - the estimated matrix_A columns, \cr
        \code{matrix_A} - the estimated matrix A.}

 \item{annual_sum}{ A \code{data.table} containing
        \code{year} - survey years, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{totalY} - the estimated value of variables of interest for period1, \cr
        \code{totalZ} - optional the estimated value of denominator for period2, \cr
        \code{estim} - the estimated value for year .}

 \item{annual_changes}{ A \code{data.table} containing
        \code{year_1} -  survey years of \code{years1}, \cr
        \code{year_2} -  survey years of \code{years2}, \cr
        \code{country} - survey countries, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{namesY} - variable with names of variables of interest, \cr
        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
        \code{estim_1} - the estimated value for period1, \cr
        \code{estim_2} - the estimated value for period2, \cr
        \code{estim} - the estimated value, \cr
        \code{var} - the estimated variance, \cr
        \code{se} - the estimated standard error, \cr
        \code{CI_lower} - the estimated confidence interval lower bound, \cr
        \code{CI_upper} - the estimated confidence interval upper bound, \cr
        \code{significant} - is the the difference significant}

}

\references{
Guillaume OSIER, Virginie RAYMOND, (2015), Development of methodology for the estimate of variance of annual net changes for LFS-based indicators. Deliverable 1 - Short document with derivation of the methodology. \cr

Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{http://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr

Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{http://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC,
URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
}

\examples{

### Example 
data("eusilc")
set.seed(1)
eusilc1 <- eusilc[1 : 100,]
set.seed(1)
data <- data.table(rbind(eusilc1, eusilc1),
                   year = c(rep(2010, nrow(eusilc1)),
                            rep(2011, nrow(eusilc1))),
                   country = c(rep("AT", nrow(eusilc1)),
                               rep("AT", nrow(eusilc1))))
data[, quarter:= .I - 4 * trunc((.I - 1) / 4)]
data[age<0, age:= 0]
PSU <- data[, .N, keyby = "db030"]
PSU[, N:= NULL]
PSU[, PSU:= trunc(runif(nrow(PSU), 0, 5))]
data <- merge(data, PSU, all=TRUE, by="db030")
PSU <- eusilc <- NULL
data[, strata:= c("XXXX")]
data[, strata:= as.character(strata)]

data[, employed:= trunc(runif(nrow(data), 0, 2))]
data[, unemployed:= trunc(runif(nrow(data), 0, 2))]
data[, labour_force:= employed + unemployed]

result <- vardchangannual(Y = "employed", H = "strata",
                          PSU = "PSU", w_final = "rb050",                          
                          ID_level2 = "db030", Dom = NULL,
                          Z = NULL, country = "country",
                          years = "year", subperiods = "quarter",
                          dataset = data, year1 = 2010, year2 = 2011,
                          percentratio = 100, confidence = 0.95)

\dontrun{
result <- vardchangannual(Y = "unemployed", H = "strata",
                          PSU = "PSU", w_final = "rb050",                          
                          ID_level2 = "db030", Dom = NULL,
                          Z = "labour_force", country = "country",
                          years = "year", subperiods = "quarter",
                          dataset = data, year1 = 2010, year2 = 2011,
                          percentratio = 100, confidence = 0.95) }
}

\seealso{
\code{\link{domain}},   \code{\link{vardcros}},  \code{\link{vardchanges}}}
\keyword{vardchangannual}
