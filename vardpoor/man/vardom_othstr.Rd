\name{vardom_othstr}
\alias{vardom_othstr}

\title{Variance estimation for sample surveys in domain by the two stratification}

\description{Computes the variance estimation for sample surveys in domain by the two stratification.}

\usage{vardom_othstr(Y, H, H2, PSU, w_final, id=NULL,
       Dom = NULL, period=NULL, N_h = NULL,
       N_h2, s2g=FALSE, Z = NULL,
       X = NULL, g = NULL, dataset = NULL, 
       q = rep(1, if (is.null(dataset)) nrow(data.frame(X)) else nrow(dataset)),
       confidence = .95, outp_lin=FALSE, outp_res=FALSE)}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H2}{The unit new stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). If \code{id} is \code{NULL}, then it will be \code{PSU}.}

\item{Dom}{Optional variables used to define population domains. If supplied, linearization of the at-risk-of-poverty rate is done for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{period}{Optional variable for survey period. If supplied, residual estimation of calibration is done independently for each time period. One dimensional object convertible to one-column \code{data.frame}.}

\item{N_h}{optional; either a \code{data.frame} giving the first column - stratum, but the second column - the total of the population in each stratum.}

\item{N_h2}{optional; either a \code{data.frame} giving the first column - new stratum, but the second column - the total of the population in each new stratum.}

\item{s2g}{by default is FALSE; calculate variance, but if s2g is TRUE, then variance estimation is taken as S^2g value.}

\item{Z}{optional variables of denominator for ratio estimation. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{X}{Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{g}{Optional variable of the g weights. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{q}{Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.frame}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}

\item{outp_lin}{Logical value. if \code{TRUE} linearized values of the ratio estimator will be printed out.}

\item{outp_res}{Logical value. if \code{TRUE} estimated residuals of calibration will be printed out.}

}

\value{

  A list with objects are returned by the function:

  \item{lin_out}{A \code{data.table} containing the linearized values of the ratio estimator with id and PSU.}

  \item{res_out}{A \code{data.table} containing the estimated residuals of calibration with id and PSU.}

  \item{all_result}{A \code{data.table}, which containing variables:

       \code{estim} - the estimated value,

       \code{var} - the estimated variance,

       \code{se} - the estimated standard error,

       \code{rse} - the estimated relative standard error (coefficient of variation),

       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage,

       \code{absolute_margin_of_error} - the estimated absolute margin of error,

       \code{relative_margin_of_error} - the estimated relative margin of error,

       \code{CI_lower} - the estimated confidence interval lower bound,
 
       \code{CI_upper} - the estimated confidence interval upper bound,

       \code{var_srs_HT} - the estimated variance of the HT estimator under SRS,

       \code{var_cur_HT} - the estimated variance of the HT estimator under current design,

       \code{var_srs_ca} - the estimated variance of the calibrated estimator under SRS,
  
       \code{deff_sam} - the estimated design effect of sample design,

       \code{deff_est} - the estimated design effect of estimator,

       \code{deff} - the overall estimated design effect of sample design and estimator.}
}

\references{
  Jean-Claude Deville (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr

  M. Liberts. (2004) Non-response Analysis and Bias Estimation in a Survey on Transportation of Goods by Road.  \cr
}

\examples{

data(eusilc)

# Example 1
eusilc1 <- eusilc[1:1000, ]
dataset <- data.frame(1:nrow(eusilc1),eusilc1)
colnames(dataset)[1] <- "IDd"
Nh <- data.table(db040=eusilc$db040, rb050=eusilc$rb050)
setkeyv(Nh, names(Nh)[1])
N_h2 <- Nh[, sum(rb050, na.rm = FALSE), keyby="db040"]

aa<-vardom_othstr(Y="eqIncome", H="db040",H2="db040", PSU="db030", w_final="rb050",
           id="rb030", Dom = "db040", period=NULL, N_h=NULL, N_h2=N_h2, Z = NULL,
           X = NULL, g = NULL, dataset = dataset,
           q = rep(1, if (is.null(dataset)) 
                       nrow(as.data.frame(H)) else nrow(dataset)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)


\dontrun{
# Example 2
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"
Nh <- data.table(db040=eusilc$db040, rb050=eusilc$rb050)
setkeyv(Nh, names(Nh)[1])
N_h2 <- Nh[, sum(rb050, na.rm = FALSE), keyby="db040"]

aa<-vardom_othstr(Y="eqIncome", H="db040",H2="db040", PSU="db030", w_final="rb050",
           id="rb030", Dom = "db040", period=NULL, N_h=NULL, N_h2=N_h2, Z = NULL,
           X = NULL, g = NULL, dataset = dataset,
           q = rep(1, if (is.null(dataset)) 
                       nrow(as.data.frame(H)) else nrow(dataset)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)
}

}
\seealso{
\code{\link{domain}},   \code{\link{lin.ratio}},  \code{\link{residual_est}},
\code{\link{vardomh}},  \code{\link{var_srs}},    \code{\link{variance_est}},
\code{\link{variance_othstr}}}
\keyword{vardpoor}
