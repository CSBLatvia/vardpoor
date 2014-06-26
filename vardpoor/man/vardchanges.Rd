\name{vardchanges}

\alias{vardchanges}

\title{Variance estimation for measures of change for multistage stage cluster sampling designs}

\description{Computes the variance estimation for measures of change for multistage stage cluster sampling designs.}

\usage{
vardchanges(Y1, H1, PSU1, w_final1, id1,
                     Dom1 = NULL,
                     Z1 = NULL,
                     country1, period1,
                     dataset1 = NULL,
                     Y2, H2, PSU2, w_final2, id2,
                     Dom2 = NULL,
                     Z2 = NULL,
                     country2, period2,
                     dataset2 = NULL,
                     confidence=0.95)}

\arguments{
\item{Y1}{either a numeric data.frame, matrix, data.table with column names giving the variables of interest, or (if \code{dataset}  is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{H1}{either 1 column data.frame, matrix, data.table with  column name giving elements  indicating the unit stratum, or (if \code{dataset} is not \code{NULL}) a character string,  an integer or a logical vector (length is the same as 'dataset'  column count) specifying the corresponding column of \code{dataset}.}

\item{PSU1}{either 1 column data.frame, matrix, data.table giving  primary sampling unit, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{w_final1}{either a numeric vector, 1 column data.frame, matrix, data.table giving the final weights, or (if \code{dataset} is not \code{NULL}) a character string,  an integer or a logical vector
(length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{id1}{optional; either 1 column data.frame, matrix, data.table with column names giving the IDs, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{Dom1}{optional; either a data.frame, matrix, data.table with column names giving different domains, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}. This variable is \code{NULL} by default. If supplied, the values for each domain are computed.}

\item{Z1}{optional; either a data.frame, matrix, data.table with column names giving variables of denominator, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}. This variable is \code{NULL} by default. If supplied, the ratio estimation is computed.}

\item{country1}{optional; either a data.frame, matrix, data.table with column names giving different countries, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}.}

\item{period1}{optional; either a data.frame, matrix, data.table with column names giving different periods, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}.}

\item{dataset1}{an optional; name of the individual dataset \code{data.frame}.}

\item{Y2}{either a numeric data.frame, matrix, data.table with column names giving the variables of interest, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{H2}{either 1 column data.frame, matrix, data.table with column name giving elements  indicating the unit stratum, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

  \item{PSU2}{either 1 column data.frame, matrix, data.table giving primary sampling unit, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{w_final2}{either a numeric vector, 1 column data.frame, matrix, data.table giving the final weights, or (if \code{dataset} is not \code{NULL}) a character string,  an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

  \item{id2}{optional; either 1 column data.frame, matrix, data.table with column names giving the IDs, or (if \code{dataset} is not \code{NULL}) a character string, an integer or a logical vector (length is the same as 'dataset' column count) specifying the corresponding column of \code{dataset}.}

\item{Dom2}{optional; either a data.frame, matrix, data.table with column names giving different domains, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}. This variable is \code{NULL} by default. If supplied, the values for each domain are computed.}

\item{Z2}{optional; either a data.frame, matrix, data.table with column names giving variables of denominator, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}. This variable is \code{NULL} by default. If supplied, the ratio estimation is computed.}

\item{country2}{optional; either a data.frame, matrix, data.table with column names giving different countries, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}.}

\item{period2}{optional; either a data.frame, matrix, data.table with column names giving different periods, or (if \code{dataset} is not \code{NULL}) character strings, integers or a logical vectors (length is the same as 'dataset' column count) specifying the corresponding columns of \code{dataset}.}

\item{dataset2}{an optional; name of the individual dataset \code{data.frame}.}

  \item{confidence}{optional; either a positive value for confidence interval. This variable by default is 0.95 .}
}

\value{
  The function returns values: 
  \item{var}{a \code{data.table} containing the values of the
  variance estimation for measures of change.}
}

\references{
  Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013} \cr
}

\examples{

### Example 

data("eusilc")

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
                   dataset2=dataset2) 
}

\seealso{
\code{\link{domain}},   \code{\link{vardcros}}}
\keyword{vardchanges}
