\name{vardom_othstr}
\alias{vardom_othstr}
\title{Estimation of the variance in domain using two stratification}
\description{Computes the estimation of the variance in domain.}
\usage{vardom_othstr(Y, H, H2, PSU, w_final, id=NULL,
       Dom = NULL, N_h = NULL, N_h2, s2g=FALSE,  Z = NULL,
       X = NULL, g = NULL, dataset = NULL, 
       q = rep(1, if (is.null(dataset)) nrow(data.frame(X)) else nrow(dataset)),
       confidence = .95, outp_lin=FALSE, outp_res=FALSE)}
\arguments{
  \item{Y}{either a numeric data.frame, matrix, data.table with
  column names giving the variables of interest, or (if \code{dataset} 
  is not \code{NULL}) a character string, an integer or a
  logical vector (length is the same as 'dataset' column count)
  specifying the corresponding column of \code{dataset}.}

  \item{H}{either 1 column data.frame, matrix, data.table with
  column name giving elements  indicating the unit stratum,
  or (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as 'dataset'
  column count) specifying the corresponding column of \code{dataset}.}

  \item{H2}{either 1 column data.frame, matrix, data.table with
  column name giving elements  indicating the unit new stratum,
  or (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as 'dataset'
  column count) specifying the corresponding column of \code{dataset}.}

  \item{PSU}{either 1 column data.frame, matrix, data.table giving
  primary sampling unit, or (if \code{dataset} is not \code{NULL})
  a character string, an integer or a logical vector (length is the
  same as 'dataset' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{w_final}{either a numeric vector, 1 column data.frame, matrix,
  data.table giving the final weights, or (if \code{dataset} is not
  \code{NULL}) a character string,  an integer or a logical vector
  (length is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.}

  \item{id}{optional; either 1 column data.frame, matrix,
  data.table with column names giving the IDs, or
  (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as
  'dataset' column count) specifying the corresponding
  column of \code{dataset}. if \code{id} is \code{NULL},
  then it will be \code{PSU}.}

  \item{Dom}{optional; either a data.frame, matrix, data.table with
  column names giving different domains, or (if \code{dataset} is
  not \code{NULL}) character strings, integers or a logical vectors
  (length is the same as 'dataset' column count) specifying the
  corresponding columns of \code{dataset}. This variable is \code{NULL}
  by default. If supplied, the values for each domain are computed.}

  \item{N_h}{optional; either a matrix giving the first column - stratum, but
  the second column - the total of the population in each stratum.}

  \item{N_h2}{optional; either a matrix giving the first column - new stratum,
  but the second column - the total of the population in each new stratum.}

  \item{s2g}{by default is FALSE; calculate variance, but if s2g is TRUE, 
  then variance estimation is taken as S^2g value.}

  \item{Z}{optional; either a data.frame, matrix, data.table with
  column names giving variables of denominator, or (if \code{dataset}
  is not \code{NULL}) character strings, integers or a logical vectors
  (length is the same as 'dataset' column count) specifying the
  corresponding columns of \code{dataset}. This variable is \code{NULL}
  by default. If supplied, the ratio estimation is computed.}

  \item{X}{optional; either a data.frame, matrix, data.table giving
  auxiliary variables, or (if \code{dataset} is not \code{NULL})
  character strings, integers or a logical vectors (length is the
  same as 'dataset' column count) specifying the corresponding
  columns of \code{dataset}. This variable is \code{NULL} by default.}

  \item{g}{optional; either a numeric vector, 1 column data.frame,
  matrix, data.table giving the g weights, or (if \code{dataset} is not
  \code{NULL}) a character string,  an integer or a logical vector
  (length is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}. This variable is \code{NULL}
  by default.}

  \item{q}{optional; either a numeric vector, 1 column data.frame,
  matrix, data.table giving the positive values accounting for
  heteroscedasticity, or (if \code{dataset} is not \code{NULL}) a
  character string,  an integer or a logical vector (length is the
  same as 'dataset' column count) specifying the corresponding
  column of \code{dataset}. This variable is \code{NULL} by default.}

  \item{dataset}{an optional; name of the individual dataset 
  \code{data.frame}.}

  \item{confidence}{optional; either a positive value for confidence
  interval. This variable by default is 0.95 .}

  \item{outp_lin}{logical. if \code{TRUE} linearized values of 
  the ratio estimator will be printed out}

  \item{outp_res}{logical. if \code{TRUE} estimated residuals
  of calibration will be printed out}
}
\value{
  The function returns values: 
  \item{estim}{a \code{data.table} containing the estimation(s)
  by domain, or (if \code{Dom} is \code{NULL}) totals.}

  \item{var}{a \code{data.table} containing the values of the
  variance estimation by domains or (if \code{Dom} is \code{NULL})
  totals.}

  \item{se}{a \code{data.table} containing the values of the
  standart error by domains or (if \code{Dom} is \code{NULL})
  totals.}

  \item{rse}{a \code{data.table} containing the values of the
  relative standart error (coefficient of variation) by domains
  or (if \code{Dom} is \code{NULL}) totals in percentage.}

  \item{cv}{a \code{data.table} containing the values of the
  relative standart error (coefficient of variation) by domains
  or (if \code{Dom} is \code{NULL}) totals.}

  \item{absolute_margin_of_error}{a \code{data.table} containing
  the values of the absolute margin of error by domains or
  (if \code{Dom} is \code{NULL}) totals.}

  \item{relative_margin_of_error}{a \code{data.table} containing
  the values of the relative margin of error by domains or
  (if \code{Dom} is \code{NULL}) totals.}

  \item{CI_lower}{a \code{data.table} containing the values of
  the confidence interval lower bound by domains or (if \code{Dom}
  is \code{NULL}) totals.}

  \item{CI_upper}{a \code{data.table} containing the values of
  the confidence interval upper bound by domains or (if \code{Dom}
  is \code{NULL}) totals.}

  \item{var_srs_HT}{a \code{matrix} containing the values of
  the variance estimation of HT estimator under SRS by domains
  or (if \code{Dom} is \code{NULL}) totals.}

  \item{var_cur_HT}{a \code{matrix} containing the values of the
  variance estimation of HT estimator under HT estimator under
  current design by domains or (if \code{Dom} is \code{NULL}) totals.}

  \item{var_srs_ca}{a \code{matrix} containing the values of
  the variance estimation of calibrated estimator under SRS
  by domains or (if \code{Dom} is \code{NULL}) totals.}
  
  \item{deff_sam}{a \code{matrix} containing the values of
  the estimation of the design effect of sample design by
  domains or (if \code{Dom} is \code{NULL}) totals.}

  \item{deff_est}{a \code{matrix} containing the values of
  the estimation of the design effect of estimator by domains
  or (if \code{Dom} is \code{NULL}) totals.}

  \item{deff}{a \code{matrix} containing the values of
  the estimation of the overall design effect of sample
  design and estimator by domains or (if \code{Dom} is
  \code{NULL}) totals.}

  \item{lin_out}{a \code{data.table} containing the linearized
  values of the ratio estimator with id and PSU.}

  \item{res_out}{a \code{data.table} containing the estimated
  residuals of calibration with id and PSU.}

  \item{all_result}{a \code{data.frame} containing all previosly definited
  values together by domains or (if \code{Dom} is \code{NULL}) totals.}
}
\details{
Calculate variance estimation on domains based on G. Osier documents.
}
\references{

  Deville, J. C. (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}
\examples{

data(eusilc)
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"
Nh <- data.table(db040=eusilc$db040, rb050=eusilc$rb050)
setkeyv(Nh, names(Nh)[1])
N_h2 <- Nh[, sum(rb050, na.rm = FALSE), keyby="db040"]

aa<-vardom_othstr("eqIncome", H="db040",H2="db040", PSU="db030", w_final="rb050",
           id="db030", Dom = "db040", N_h=NULL, N_h2, Z = NULL,
           X = NULL, g = NULL, dataset = dataset,
           q = rep(1, if (is.null(dataset)) 
                       nrow(as.data.frame(H)) else nrow(dataset)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)

} \seealso{
  \code{\link{domain}},   \code{\link{lin.ratio}},  \code{\link{residual_est}},
  \code{\link{vardomh}},  \code{\link{var_srs}},    \code{\link{variance_est}},
  \code{\link{variance_othstr}}
}
\keyword{vardpoor}
