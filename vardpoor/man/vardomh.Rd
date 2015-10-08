\name{vardomh}

\alias{vardomh}

\title{Variance estimation for sample surveys in domain for household surveys by the ultimate
                        cluster method}

\description{Computes the variance estimation in domain for household.}

\usage{vardomh(Y, H, PSU, w_final, ID_household,
               id = NULL, Dom = NULL, period = NULL,
               N_h = NULL, fh_zero = FALSE, PSU_level = TRUE,
               Z = NULL, dataset = NULL,
               X = NULL, periodX = NULL, X_ID_household = NULL,
               ind_gr = NULL, g = NULL, q = NULL, datasetX = NULL,
              confidence = .95,  outp_lin = FALSE, outp_res = FALSE)}

\arguments{

\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{ID_household}{Variable for household ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{period}{Optional variable for the survey periods. If supplied, the values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Dom}{Optional variables used to define population domains. If supplied, values are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{N_h}{optional; either a matrix giving the first column - stratum, but the second column - the total of the population in each stratum.}

\item{fh_zero}{by default FALSE; fh is calculated as division of n_h and N_h in each strata, if true, fh value is zero in each strata.}

\item{PSU_level}{by default TRUE; if PSU_level is true, in each strata fh is calculated as division of count of PSU in sample (n_h) and count of PSU in frame (N_h). if PSU_level is false, in each strata fh is calculated as division of count of units in sample (n_h) and count of units in frame (N_h), which calculated as sum of weights.}

\item{Z}{Optional variables of denominator for ratio estimation. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{X}{Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{periodX}{Optional variable ofthe survey periods. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{X_ID_household}{Variable for household ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{ind_gr}{Optional variable by which divided independently auxiliary variables. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{g}{Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{q}{Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{datasetX}{Optional survey data object in household level convertible to \code{data.table}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}

\item{outp_lin}{Logical value. If \code{TRUE} linearized values of the ratio estimator will be printed out.}

\item{outp_res}{Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.}

}


\value{

  A list with objects are returned by the function:

  \item{lin_out}{A \code{data.table} containing the linearized values of the ratio estimator with id and PSU.}

  \item{res_out}{A \code{data.table} containing the estimated residuals of calibration with id and PSU.}

  \item{all_result}{A \code{data.table}, which containing variables:

       \code{respondent_count} - the count of respondents, \cr
       \code{pop_size} - the estimated size of population, \cr
       \code{n_nonzero} - the count of respondents, who answers are larger than zero, \cr
       \code{estim} - the estimated value, \cr
       \code{var} - the estimated variance, \cr
       \code{se} - the estimated standard error, \cr
       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
       \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
       \code{relative_margin_of_error} - the estimated relative margin of error, \cr
       \code{CI_lower} - the estimated confidence interval lower bound, \cr
       \code{CI_upper} - the estimated confidence interval upper bound, \cr
       \code{var_srs_HT} - the estimated variance of the HT estimator under SRS, \cr
       \code{var_cur_HT} - the estimated variance of the HT estimator under current design, \cr
       \code{var_srs_ca} - the estimated variance of the calibrated estimator under SRS, \cr
       \code{deff_sam} - the estimated design effect of sample design, \cr
       \code{deff_est} - the estimated design effect of estimator, \cr
       \code{deff} - the overall estimated design effect of sample design and estimator, \cr
       \code{n_eff} - the effective sample size.}

}

\details{
Calculate variance estimation in domains for household surveys based on book of Hansen, Hurwitz and Madow.
}

\references{
Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr

Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second wave onwards? 2012 \cr

Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{http://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr

Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{http://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013}    \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}


\examples{

data(eusilc)
dataset <- data.table(IDd=1:nrow(eusilc),eusilc)

aa<-vardomh(Y="eqIncome", H="db040", PSU="db030", w_final="rb050",
           ID_household="db030", id="rb030", Dom = "db040",
           period = NULL, N_h = NULL, Z = NULL, dataset = dataset, X = NULL,
           X_ID_household = NULL, g = NULL, datasetX = NULL,
           q = rep(1, if (is.null(datasetX)) 
                       nrow(as.data.frame(H)) else nrow(datasetX)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)

\dontrun{
dataset2 <- copy(dataset)
dataset$period <- 1
dataset2$period <- 2
dataset <-data.table(rbind(dataset, dataset2))

# by default without using fh_zero (finite population correction)
aa2<-vardomh(Y="eqIncome", H="db040", PSU="db030", w_final="rb050",
           ID_household="db030", id="rb030", Dom = "db040",
           period = "period", N_h = NULL, Z = NULL, dataset = dataset,
           X=NULL, X_ID_household=NULL, 
           g=NULL, q=NULL, datasetX=NULL,
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)
aa2

# without using fh_zero (finite population correction)
aa3<-vardomh(Y="eqIncome", H="db040", PSU="db030", w_final="rb050",
           ID_household="db030", id="rb030", Dom = "db040",
           period = "period", N_h = NULL, fh_zero=FALSE, 
           Z = NULL, dataset = dataset,
           X = NULL, X_ID_household = NULL, 
           g = NULL, datasetX = NULL,
           q = rep(1, if (is.null(datasetX)) 
                       nrow(as.data.frame(H)) else nrow(datasetX)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)
aa3

# with using fh_zero (finite population correction)
aa4<-vardomh(Y="eqIncome", H="db040", PSU="db030", w_final="rb050",
           ID_household="db030", id="rb030", Dom = "db040",
           period = "period", N_h = NULL, fh_zero=TRUE, 
           Z = NULL, dataset = dataset,
           X = NULL, X_ID_household = NULL, 
           g = NULL, datasetX = NULL,
           q = rep(1, if (is.null(datasetX)) 
                       nrow(as.data.frame(H)) else nrow(datasetX)),
           confidence = .95, outp_lin=TRUE, outp_res=TRUE)
aa4
}

}

\seealso{
\code{\link{domain}},   \code{\link{lin.ratio}},  \code{\link{residual_est}},
\code{\link{var_srs}},  \code{\link{variance_est}}
}
\keyword{vardpoor}
