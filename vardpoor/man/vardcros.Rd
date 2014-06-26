\name{vardcros}
\alias{vardcros}
\title{Variance estimation for cros-sectional and longitudinal measures for multistage stage cluster sampling designs}
\description{Computes the variance estimation for cros-sectional and longitudinal measures for multistage stage cluster sampling designs.}
\usage{
          vardcros(Y, H, PSU, w_final, id,
                   Dom = NULL,
                 Z = NULL,
country, period,
dataset = NULL,
meanY=TRUE,
withoutperiod=TRUE,
netchanges=TRUE,
confidence = .95)}

\arguments{
\item{Y}{either a numeric data.frame, matrix, data.table with column names giving the variables of interest, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{H}{either 1 column data.frame, matrix, data.table with column name giving elements indicating the unit stratum, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{PSU}{either 1 column data.frame, matrix, data.table giving primary sampling unit, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{w_final}{either a numeric vector, 1 column data.frame, matrix, data.table giving the final weights, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{id}{optional; either 1 column data.frame, matrix, data.table with column names giving the IDs, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{Dom}{optional; either a data.frame, matrix, data.table with column names giving different domains, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}. This variable is \code{NULL} by default. If supplied, the values for each domain are computed.}

\item{Z}{optional; either a data.frame, matrix, data.table with column names giving variables of denominator, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}. This variable is \code{NULL} by default. If supplied, the ratio estimation is computed.}

\item{country}{optional; either a data.frame, matrix, data.table with column names giving different countries, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset} .}

\item{period}{optional; either a data.frame, matrix, data.table with column names giving different periods, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset} .}
 
\item{dataset}{an optional; name of the individual dataset \code{data.frame}.}

\item{meanY}{logical value; is valid for checking variables and determine which kind it is, }

\item{withoutperiod}{logical value which determines is results with or without period .}

\item{netchanges}{logical value to produce aggregation matrix at period, country, strata and PSU for gradient .}

\item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95 .}
}

\value{
The function returns values:

\item{sample_size}{a \code{data.table} containing the values of the sample size (in numbers of individuals).}

\item{pop_size}{a \code{data.table} containing the values of the population size (in numbers of individuals).}

\item{total}{a \code{data.table} containing the estimations of totals.}

\item{variance}{a \code{data.table} containing the values of the variance estimation of cross-sectional or longitudinal measures.}

\item{sd_w}{a \code{data.table} containing the values of the weighted variance estimation of simple random sample.}

\item{sd_nw}{a \code{data.table} containing the values of the variance estimation of simple random sample.}

\item{pop}{a \code{data.table} containing the values of the population size (in numbers of households).}

\item{sampl_siz}{a \code{data.table} containing the values of the sample size (in numbers of households).}

\item{stderr_w}{a \code{data.table} containing the values of the weighted standard deviation estimation of simple random sample.}

\item{stderr_nw}{a \code{data.table} containing the values of the standard deviation estimation of simple random sample.}

\item{sd}{a \code{data.table} containing the values of the standard deviation estimation of cross-sectional or longitudinal.}

\item{cv}{a \code{data.table} containing the values of the relative standart error (coefficient of variation).}
}

\references{
Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC,
URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr
}



\examples{
# Example 1
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

result1 <- vardcros(Y=c("pov", "dep", "arope"),
                    H="strata", PSU="PSU", w_final="rb050",
                    id="db030", Dom="rb090", Z=NULL,
                    country="country", period="year",
                    dataset=data,
                    meanY=TRUE, 
                    withoutperiod=FALSE,
                    netchanges=TRUE,
                    confidence = .95)

data2 <- data[exp2==1]
result2 <- vardcros(Y=c("lwi"),
                    H="strata", PSU="PSU", w_final="rb050",
                    id="db030", Dom="rb090", Z=NULL,
                    country="country", period="year",
                    dataset=data2,
                    meanY=TRUE,
                    withoutperiod=FALSE,
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

result2 <- vardcros(Y="ppr", H="strata", PSU="PSU",
                    w_final="w4", id="rb030",
                    Dom = NULL, Z=NULL,
                    country="rb020", period=NULL,
                    dataset=apv,
                    meanY=TRUE,
                    withoutperiod=TRUE,
                    netchanges=FALSE,
                    confidence = .95)

}


\seealso{\code{\link{domain}}, \code{\link{lin.ratio}}}
\keyword{vardcros}
