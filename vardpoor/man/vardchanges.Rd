\name{vardchanges}

\alias{vardchanges}

\title{Variance estimation for measures of change for single and multistage stage cluster sampling designs}

\description{Computes the variance estimation for measures of change for single and multistage stage cluster sampling designs.}

\usage{
vardchanges(Y, H, PSU, w_final, id,
            Dom = NULL, Z = NULL,
            country, periods, dataset = NULL,
            period1, period2,
            linratio = FALSE,
            use.estVar = FALSE,
            confidence=0.95)}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{id}{variable for unit ID codes (for household surveys - secondary unit id number). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Dom}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Z}{Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}). This variable is \code{NULL} by default.}

\item{country}{Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{periods}{Variable for the all survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{period1}{The vector of the one row from variable \code{periods} describes the first period.}

\item{period2}{The vector of the one row from variable \code{periods} describes the second period.}

\item{linratio}{Logical value. If value is \code{TRUE}, then the linearized variables for the ratio estimator is used for variance estimation. If value is \code{FALSE}, then the gradients is used for variance estimation.}

\item{use.estVar}{Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the  estimation of covariance matrix of the residuals.}

\item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95 .}
}

\value{
  
  A list with objects are returned by the function:

  \item{crossectional_results}{ A \code{data.table} containing:
    
       \code{period} -  survey periods, \cr
       \code{country} - survey countries, \cr
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

  \item{changes_results}{ A \code{data.table} containing

        \code{estim} - the estimated value, \cr
        \code{var} - the estimated variance, \cr
        \code{se} - the estimated standard error, \cr
        \code{rse} - the estimated relative standard error (coefficient of variation), \cr
        \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
        \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
        \code{relative_margin_of_error} - the estimated relative margin of error, \cr
        \code{CI_lower} - the estimated confidence interval lower bound, \cr
        \code{CI_upper} - the estimated confidence interval upper bound.}
}

\references{
Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{http://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr

Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{http://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC,
URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr
}

\examples{

### Example 

data("eusilc")
set.seed(1)
eusilc1 <- eusilc[1:40,]

set.seed(1)
data <- data.table(rbind(eusilc1, eusilc1),
                   year=c(rep(2010, nrow(eusilc1)),
                          rep(2011, nrow(eusilc1))),
                   country=c(rep("AT", nrow(eusilc1)),
                             rep("AT", nrow(eusilc1))))
data[age<0, age:=0]
PSU <- data[,.N, keyby="db030"]
PSU[, N:=NULL]
PSU[, PSU:=trunc(runif(nrow(PSU), 0, 5))]
setkeyv(PSU, "db030")
setkeyv(data, "db030")
data <- merge(data, PSU, all=TRUE)
PSU <- eusilc <- NULL
data[, strata:=c("XXXX")]
data[, strata:=as.character(strata)]

data[, t_pov:=trunc(runif(nrow(data), 0, 2))]
data[, exp:= 1]

# At-risk-of-poverty (AROP)
data[, pov:= ifelse (t_pov == 1, 1, 0)]
 
result <- vardchanges(Y="pov",
                   H="strata", PSU="PSU", w_final="rb050",
                   id="db030", Dom=NULL, Z=NULL,
                   country="country", periods="year",
                   dataset=data,
                   period1=2010, period2=2011,
                   use.estVar=FALSE)
 
\dontrun{
data("eusilc")
data <- data.table(rbind(eusilc, eusilc),
                   year=c(rep(2010, nrow(eusilc)),
                          rep(2011, nrow(eusilc))),
                   country=c(rep("AT", nrow(eusilc)),
                             rep("AT", nrow(eusilc))))
data[age<0, age:=0]
PSU <- data[,.N, keyby="db030"]
PSU[, N:=NULL]
PSU[, PSU:=trunc(runif(nrow(PSU), 0, 100))]
setkeyv(PSU, "db030")
setkeyv(data, "db030")
data <- merge(data, PSU, all=TRUE)
PSU <- eusilc <- NULL
data[, strata:=c("XXXX")]
data[, strata:=as.character(strata)]

data[, t_pov:=trunc(runif(nrow(data), 0, 2))]
data[, t_dep:=trunc(runif(nrow(data), 0, 2))]
data[, t_lwi:=trunc(runif(nrow(data), 0, 2))]
data[, exp:= 1]
data[, exp2:= 1 * (age < 60)]

# At-risk-of-poverty (AROP)
data[, pov:= ifelse (t_pov == 1, 1, 0)]
 
# Severe material deprivation (DEP)
data[, dep:= ifelse (t_dep == 1, 1, 0)]

# Low work intensity (LWI)
data[, lwi:= ifelse (t_lwi == 1 & exp2 == 1, 1, 0)]

# At-risk-of-poverty or social exclusion (AROPE)
data[, arope:= ifelse (pov == 1 | dep == 1 | lwi == 1, 1, 0)]
data[, dom:=1]

result <- vardchanges(Y=c("pov", "dep", "lwi", "arope"),
                   H="strata", PSU="PSU", w_final="rb050",
                   id="db030", Dom="rb090", Z=NULL,
                   country="country", periods="year",
                   dataset=data,
                   period1=2010, period2=2011,
                   use.estVar=FALSE)
}
}

\seealso{
\code{\link{domain}},   \code{\link{vardcros}},  \code{\link{vardchangespoor}}}
\keyword{vardchanges}
