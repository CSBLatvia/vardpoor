\name{vardcros}

\alias{vardcros}

\title{Variance estimation for cross-sectional, longitudinal measures for single and multistage stage cluster sampling designs}

\description{Computes the variance estimation for cross-sectional and longitudinal measures for any stage cluster sampling designs.}

\usage{
vardcros(Y, H, PSU, w_final, id, Dom = NULL,
         Z = NULL, country, period,
         dataset = NULL, linratio=FALSE,
         household_level_max=TRUE,
         withperiod=TRUE, netchanges=TRUE,
         confidence = .95)}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{id}{variable for unit ID codes (for household surveys - secondary unit id number). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Dom}{Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Z}{Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}). This variable is \code{NULL} by default.}

\item{country}{Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{period}{Variable for the survey periods. The values for each period are computed independently. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.frame}.}

\item{linratio}{Logical value. If value is \code{TRUE}, then the linearized variables for the ratio estimator is used for variance estimation. If value is \code{FALSE}, then the gradients is used for variance , but if f variables of interest.}

\item{household_level_max}{Logical value.}

\item{withperiod}{Logical value. If \code{TRUE} is value, the results is with period, if \code{FALSE}, without period.}

\item{netchanges}{Logical value. If value is TRUE, then produce two objects: the first object is aggregation of weighted data by period (if available), country, strata and PSU, the second object is an estimation for Y, the variance, gradient for numerator and denominator by country and period (if available). If value is FALSE, then both objects containing \code{NULL}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}
}

\value{

  A list with three objects are returned by the function:

  \item{data_net_changes}{ A \code{data.table} containing aggregation of weighted data by period (if available), country, strata, PSU.}

  \item{var_grad}{ A \code{data.table} containing estimation for Y, the variance, gradient for numerator and denominator by country and period (if available).}

  \item{results}{ A \code{data.table} containing
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
} 

\references{
Eurostat Methodologies and Working papers, Standard error estimation for the EU-SILC indicators of poverty and social exclusion, 2013, URL \url{http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-RA-13-024/EN/KS-RA-13-024-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC,
URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr
}



\examples{
# Example 1
data(eusilc)
set.seed(1)
data <- data.table(eusilc,
                   year=rep(2010, nrow(eusilc)),
                   country=rep("AT", nrow(eusilc)))
data[age<0, age:=0]
PSU <- data[,.N, keyby="db030"]
PSU[, N:=NULL]
PSU[, PSU:=trunc(runif(nrow(PSU), 0, 100))]
data <- merge(data, PSU, by="db030", all=TRUE)
PSU <- eusilc <- 0

data[, strata:="XXXX"]
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

result11 <- vardcros(Y="arope",
                    H="strata", PSU="PSU", w_final="rb050",
                    id="db030", Dom="rb090", Z=NULL,
                    country="country", period="year",
                    dataset=data,
                    linratio=FALSE, 
                    withperiod=TRUE,
                    netchanges=TRUE,
                    confidence = .95)
\dontrun{
# Example 2
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
data <- merge(data, PSU, by="db030", all=TRUE)
PSU <- eusilc <- 0
data[, strata:="XXXX"]
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

result11 <- vardcros(Y=c("pov", "dep", "arope"),
                    H="strata", PSU="PSU", w_final="rb050",
                    id="db030", Dom="rb090", Z=NULL,
                    country="country", period="year",
                    dataset=data,
                    linratio=FALSE, 
                    withperiod=TRUE,
                    netchanges=TRUE,
                    confidence = .95)

data2 <- data[exp2==1]
result12 <- vardcros(Y=c("lwi"),
                    H="strata", PSU="PSU", w_final="rb050",
                    id="db030", Dom="rb090", Z=NULL,
                    country="country", period="year",
                    dataset=data2,
                    linratio=FALSE, 
                    withperiod=TRUE,
                    netchanges=TRUE,
                    confidence = .95)

### Example 2
data(eusilc)
set.seed(1)
year <- 2011
data <- data.table(rbind(eusilc, eusilc, eusilc, eusilc),
                   rb010=c(rep(2008, nrow(eusilc)),
                           rep(2009, nrow(eusilc)),
                           rep(2010, nrow(eusilc)),
                           rep(2011, nrow(eusilc))),
                   rb020=c(rep("AT", nrow(eusilc)),
                           rep("AT", nrow(eusilc)),
                           rep("AT", nrow(eusilc)),
                           rep("AT", nrow(eusilc))))
data[, u:=1]
data[age<0, age:=0]
data[, strata:="XXXX"]
PSU <- data[,.N, keyby="db030"]
PSU[, N:=NULL]
PSU[, PSU:=trunc(runif(nrow(PSU), 0, 100))]
data <- merge(data, PSU, by="db030", all=TRUE)
thres <- data.table(rb020=rep("AT",4),
                    thres= c(11406, 11931, 12371, 12791),
                    rb010=2008:2011)
setnames(thres, names(thres), tolower(names(thres)))
setkeyv(data, c("rb010", "rb020"))
setkeyv(thres, c("rb010", "rb020"))
data <- merge(data, thres, all.x=TRUE)
data[is.na(u), u:=0]
data <- data[u==1]
setkeyv(data, c("rb020", "rb030"))

#############
# T3        #
#############

T3 <- data[rb010==year-3]
T3[, strata1:=strata]
T3[, PSU1:=PSU]
T3[, w1:=rb050]
T3[, inc1:=eqIncome]
T3[, rb110_1:=db030]
setnames(T3, "thres", "thres1")
T3[, pov1:=inc1<=thres1]
T3 <- T3[, c("rb020", "rb030", "strata", "PSU", "inc1", "pov1"), with=FALSE]

#############
# T2        #
#############
T2 <- data[rb010==year-2]
T2[, strata2:=strata]
T2[, PSU2:=PSU]
T2[, w2:=rb050]
T2[, inc2:=eqIncome]
T2[, rb110_2:=db030]
setnames(T2, "thres", "thres2")
T2[, pov2:=inc2<=thres2]
T2 <- T2[, c("rb020", "rb030","strata2","PSU2","inc2","pov2"), with=FALSE]
#############
# T1 #
#############
T1 <- data[rb010==year-1]
T1[, strata3:=strata]
T1[, PSU3:=PSU]
T1[, w3:=rb050]
T1[, inc3:=eqIncome]
T1[, rb110_3:=db030]
setnames(T1, "thres", "thres3")
T1[, pov3:=inc3<=thres3]
T1 <- T1[, c("rb020", "rb030", "strata3", "PSU3", "inc3", "pov3"), with=FALSE]
#############
# T0 #
#############
T0 <- data[rb010==year]
T0[, PSU4:=PSU]
T0[, strata4:=strata]
T0[, w4:=rb050]
T0[, inc4:=eqIncome]
T0[, rb110_4:=db030]
setnames(T0, "thres", "thres4")
T0[, pov4:=inc4<=thres4]
T0 <- T0[, c("rb020", "rb030", "strata4", "PSU4", "w4", "inc4", "pov4"), with=FALSE]
apv <- merge(T3, T2, all=TRUE)
apv <- merge(apv, T1, all=TRUE)
apv <- merge(apv, T0, all=TRUE)
apv <- apv[(!is.na(inc1)) & (!is.na(inc2)) & (!is.na(inc3)) & (!is.na(inc4))]
apv[, ppr:=ifelse(((pov4==1)&((pov1==1&pov2==1&pov3==1)|(pov1==1&pov2==1&
pov3==0)|(pov1==1&pov2==0&pov3==1)|(pov1==0&pov2==1&pov3==1))),1,0)]

result20 <- vardcros(Y="ppr", H="strata", PSU="PSU",
                    w_final="w4", id="rb030",
                    Dom = NULL, Z=NULL,
                    country="rb020", period=NULL,
                    dataset=apv,
                    linratio=FALSE, 
                    withperiod=FALSE,
                    netchanges=FALSE,
                    confidence = .95)}
}


\seealso{\code{\link{domain}}, \code{\link{lin.ratio}}}
\keyword{vardcros}
