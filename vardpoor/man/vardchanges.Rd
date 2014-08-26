\name{vardchanges}

\alias{vardchanges}

\title{Variance estimation for measures of change for single and multistage stage cluster sampling designs}

\description{Computes the variance estimation for measures of change for single and multistage stage cluster sampling designs.}

\usage{
vardchanges(Y1, H1, PSU1, w_final1, id1,
            Dom1 = NULL, Z1 = NULL,
            country1, period1, dataset1 = NULL,
            Y2, H2, PSU2, w_final2, id2,
            Dom2 = NULL, Z2 = NULL, country2,
            period2, dataset2 = NULL, confidence=0.95)}

\arguments{

\item{Y1}{Variables of interest. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{H1}{The unit stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{PSU1}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{w_final1}{Weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{id1}{variable for unit ID codes (for household surveys - secondary unit id number). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{Dom1}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{Z1}{Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset2}). This variable is \code{NULL} by default.}

\item{country1}{Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{period1}{Variable for the survey periods. The values for each period are computed independently. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset1}).}

\item{dataset1}{Optional survey data object convertible to \code{data.frame}.}


\item{Y2}{Variables of interest. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{H2}{The unit stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{PSU2}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{w_final2}{Weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{id2}{variable for unit ID codes (for household surveys - secondary unit id number). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{Dom2}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{Z2}{Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset2}). This variable is \code{NULL} by default.}

\item{country2}{Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{period2}{Variable for the survey periods. The values for each period are computed independently. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset2}).}

\item{dataset2}{Optional survey data object convertible to \code{data.frame}.}

\item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95 .}
}

\value{
  
  A \code{data.table} is returned by the function and containing: 

        \code{estim} - the estimated value,

        \code{var} - the estimated variance,

        \code{se} - the estimated standard error,

        \code{rse} - the estimated relative standard error (coefficient of variation),

        \code{cv} - the estimated relative standard error (coefficient of variation) in percentage,

        \code{absolute_margin_of_error} - the estimated absolute margin of error,

        \code{relative_margin_of_error} - the estimated relative margin of error,

        \code{CI_lower} - the estimated confidence interval lower bound,

        \code{CI_upper} - the estimated confidence interval upper bound.
}

\references{
Eurostat Methodologies and Working papers, Standard error estimation for the EU-SILC indicators of poverty and social exclusion, 2013, URL \url{http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-RA-13-024/EN/KS-RA-13-024-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC,
URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr
}

\examples{

### Example 

data("eusilc")

set.seed(1)

data("eusilc")
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
 
dataset1 <- data[year==2010]
dataset2 <- data[year==2011]

result <- vardchanges(Y1="pov",
                   H1="strata", PSU1="PSU", w_final1="rb050",
                   id1="db030", Dom1=NULL, Z1=NULL,
                   country1="country", period1="year",
                   dataset1=dataset1,
                   Y2="pov",
                   H2="strata", PSU2="PSU", w_final2="rb050",
                   id2="db030", Dom2=NULL, Z2=NULL,
                   country2="country", period2="year",
                   dataset2=dataset2) 

\dontrun{
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
dataset1 <- data[year==2010]
dataset2 <- data[year==2011]

result <- vardchanges(Y1=c("pov", "dep", "lwi", "arope"),
                   H1="strata", PSU1="PSU", w_final1="rb050",
                   id1="db030", Dom1="rb090", Z1=NULL,
                   country1="country", period1="year",
                   dataset1=dataset1,
                   Y2=c("pov", "dep", "lwi", "arope"),
                   H2="strata", PSU2="PSU", w_final2="rb050",
                   id2="db030", Dom2="rb090", Z2=NULL,
                   country2="country", period2="year",
                   dataset2=dataset2)}
}


\seealso{
\code{\link{domain}},   \code{\link{vardcros}}}
\keyword{vardchanges}
