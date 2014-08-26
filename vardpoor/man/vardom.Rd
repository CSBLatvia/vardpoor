\name{vardom}

\alias{vardom}

\title{Variance estimation of the sample surveys in domain by the ultimate cluster method}

\description{Computes the variance estimation of the sample surveys in domain by the ultimate cluster method.}

\usage{
vardom(Y, H, PSU, w_final, id=NULL,
       Dom = NULL, period = NULL, 
       N_h = NULL, fh_zero=FALSE,
       PSU_level = TRUE, Z = NULL,
       X = NULL, ind_gr = NULL, g = NULL,
       dataset = NULL, 
       q = rep(1, if (is.null(dataset)) 
             nrow(data.frame(X)) else nrow(dataset)),
       confidence = .95, outp_lin=FALSE,
       outp_res=FALSE)}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Dom}{Optional variables used to define population domains. If supplied, variables of interest are calculated for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{period}{Optional variable for survey period. If supplied, residual estimation of calibration is done independently for each time period. One dimensional object convertible to one-column \code{data.frame}.}

\item{N_h}{optional; either a matrix giving the first column - stratum, but the second column - the total of the population in each stratum.}

\item{fh_zero}{by default FALSE; fh is calculated as division of n_h and N_h in each strata, if true, fh value is zero in each strata.}

\item{PSU_level}{by default TRUE; if PSU_level is true, in each strata fh is calculated as division of count of PSU in sample (n_h) and count of PSU in frame(N_h). if PSU_level is false, in each strata fh is calculated as division of count of units in sample (n_h) and count of units in frame(N_h), which calculated as sum of weights.}

\item{Z}{optional variables of denominator for ratio estimation. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{X}{Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{ind_gr}{Optional variable by which divided independently auxiliary variables. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{g}{Optional variable of the g weights. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{q}{Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.frame}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}

\item{outp_lin}{Logical value. if \code{TRUE} linearized values of the ratio estimator will be printed out.}

\item{outp_res}{Logical value. if \code{TRUE} estimated residuals of calibration will be printed out.}
}


\value{

  A list with objects is returned by the function:

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

\details{
Calculate variance estimation in domains based on book of Hansen, Hurwitz and Madow.

}
\references{
Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr

Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second wave onwards? 2012 \cr

Eurostat Methodologies and Working papers, Standard error estimation for the EU-SILC indicators of poverty and social exclusion, 2013, URL \url{http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-RA-13-024/EN/KS-RA-13-024-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013}    \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"

aa<-vardom(Y="eqIncome", H="db040", PSU="db030",
           w_final="rb050", id="rb030", Dom = "db040",
           period = NULL, N_h=NULL, Z = NULL,
           X = NULL, g = NULL, dataset = dataset,
           q = rep(1, if (is.null(dataset)) 
                       nrow(as.data.frame(H)) else nrow(dataset)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)

}
\seealso{
\code{\link{domain}},   \code{\link{lin.ratio}},  \code{\link{residual_est}},
\code{\link{vardomh}},  \code{\link{var_srs}},    \code{\link{variance_est}},
\code{\link{variance_othstr}}}
\keyword{vardpoor}
