% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vardomh.R
\name{vardomh}
\alias{vardomh}
\title{Variance estimation for sample surveys in domain for one or two stage surveys by the ultimate cluster method}
\usage{
vardomh(
  Y,
  H,
  PSU,
  w_final,
  ID_level1,
  ID_level2,
  Dom = NULL,
  period = NULL,
  N_h = NULL,
  PSU_sort = NULL,
  fh_zero = FALSE,
  PSU_level = TRUE,
  Z = NULL,
  dataset = NULL,
  X = NULL,
  periodX = NULL,
  X_ID_level1 = NULL,
  ind_gr = NULL,
  g = NULL,
  q = NULL,
  datasetX = NULL,
  confidence = 0.95,
  percentratio = 1,
  outp_lin = FALSE,
  outp_res = FALSE
)
}
\arguments{
\item{Y}{Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{ID_level1}{Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{ID_level2}{Variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{Dom}{Optional variables used to define population domains. If supplied, values are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.}

\item{period}{Optional variable for the survey periods. If supplied, the values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{N_h}{Number of primary sampling units in population for each stratum (and period if \code{period} is not \code{NULL}). If \code{N_h = NULL} and \code{fh_zero = FALSE} (default), \code{N_h} is estimated from sample data as sum of weights (\code{w_final}) in each stratum (and period if \code{period} is not \code{NULL})
Optional for single-stage sampling design as it will be estimated from sample data. Recommended for multi-stage sampling design as \code{N_h} can not be correctly estimated from the sample data in this case. If \code{N_h} is not used in case of multi-stage sampling design (for example, because this information is not available), it is advisable to set \code{fh_zero = TRUE}.
If \code{period} \bold{is} \code{NULL}. A two-column data object convertible to \code{data.table} with rows for each stratum. The first column should contain stratum code. The second column - the number of primary sampling units in the population of each stratum.
If \code{period} \bold{is not} \code{NULL}. A three-column data object convertible to \code{data.table} with rows for each intersection of strata and period. The first column should contain period. The second column should contain stratum code. The third column - the number of primary sampling units in the population of each stratum and period.}

\item{PSU_sort}{optional; if PSU_sort is defined, then variance is calculated for systematic sample.}

\item{fh_zero}{by default FALSE; \code{fh} is calculated as division of n_h and N_h in each strata, if TRUE, \code{fh} value is zero in each strata.}

\item{PSU_level}{by default TRUE; if PSU_level is TRUE, in each strata \code{fh} is calculated as division of count of PSU in sample (n_h) and count of PSU in frame (N_h). if PSU_level is FALSE, in each strata \code{fh} is calculated as division of count of units in sample (n_h) and count of units in frame (N_h), which calculated as sum of weights.}

\item{Z}{Optional variables of denominator for ratio estimation. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.table}.}

\item{X}{Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{periodX}{Optional variable of the survey periods. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.}

\item{X_ID_level1}{Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{ind_gr}{Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{g}{Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{q}{Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.}

\item{datasetX}{Optional survey data object in level1 convertible to \code{data.table}.}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}

\item{percentratio}{Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.}

\item{outp_lin}{Logical value. If \code{TRUE} linearized values of the ratio estimator will be printed out.}

\item{outp_res}{Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.}
}
\value{
A list with objects are returned by the function:
  \itemize{
     \item lin_out A \code{data.table} containing the linearized values of the ratio estimator with ID_level2 and PSU.
     \item res_out A \code{data.table} containing the estimated residuals of calibration with ID_level1 and PSU.
     \item betas A numeric \code{data.table} containing the estimated coefficients of calibration.
     \item all_result A \code{data.table}, which containing variables:
        \code{variable} - names of variables of interest, \cr
        \code{Dom} - optional variable of the population domains, \cr
        \code{period} - optional variable of the survey periods, \cr
        \code{respondent_count} - the count of respondents, \cr
        \code{pop_size} - the estimated size of population, \cr
        \code{n_nonzero} - the count of respondents, who answers are larger than zero, \cr
        \code{estim} - the estimated value, \cr
        \code{var} - the estimated variance, \cr
        \code{se} - the estimated standard error, \cr
        \code{rse} - the estimated relative standard error (coefficient of variation), \cr
        \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
        \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
        \code{relative_margin_of_error} - the estimated relative margin of error in percentage, \cr
        \code{CI_lower} - the estimated confidence interval lower bound, \cr
        \code{CI_upper} - the estimated confidence interval upper bound, \cr
        \code{confidence_level} - the positive value for confidence interval, \cr
        \code{S2_y_HT} - the estimated variance of the y variable in case of total or the estimated variance of the linearised variable in case of the ratio of two totals using non-calibrated weights, \cr
        \code{S2_y_ca} - the estimated variance of the y variable in case of total or the estimated variance of the linearised variable in case of the ratio of two totals using calibrated weights, \cr       \code{S2_res} - the estimated variance of the regression residuals, \cr
        \code{S2_res} - the estimated variance of the regression residuals, \cr
        \code{var_srs_HT} - the estimated variance of the HT estimator under SRS for household, \cr
        \code{var_cur_HT} - the estimated variance of the HT estimator under current design for household, \cr
        \code{var_srs_ca} - the estimated variance of the calibrated estimator under SRS for household, \cr
        \code{deff_sam} - the estimated design effect of sample design for household, \cr
        \code{deff_est} - the estimated design effect of estimator for household, \cr
        \code{deff} - the overall estimated design effect of sample design and estimator for household
   }
}
\description{
Computes the variance estimation in domain for ID_level1.
}
\details{
Calculate variance estimation in domains for household surveys based on book of Hansen, Hurwitz and Madow.
}
\examples{
library("data.table")
library("laeken")
data("eusilc")
dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
aa <- vardomh(Y = "eqIncome", H = "db040", PSU = "db030",
             w_final = "rb050", ID_level1 = "db030",
             ID_level2 = "rb030", Dom = "db040", period = NULL,
             N_h = NULL, Z = NULL, dataset = dataset1, X = NULL,
             X_ID_level1 = NULL, g = NULL, q = NULL, 
             datasetX = NULL, confidence = 0.95, percentratio = 1,
             outp_lin = TRUE, outp_res = TRUE)

\dontrun{
dataset2 <- copy(dataset1)
dataset1$period <- 1
dataset2$period <- 2
dataset1 <- data.table(rbind(dataset1, dataset2))

# by default without using fh_zero (finite population correction)
aa2 <- vardomh(Y = "eqIncome", H = "db040", PSU = "db030",
               w_final = "rb050", ID_level1 = "db030",
               ID_level2 = "rb030", Dom = "db040", period = "period",
               N_h = NULL, Z = NULL, dataset = dataset1,
               X = NULL, X_ID_level1 = NULL,  
               g = NULL, q = NULL, datasetX = NULL,
               confidence = .95, percentratio = 1,
               outp_lin = TRUE, outp_res = TRUE)
aa2

# without using fh_zero (finite population correction)
aa3 <- vardomh(Y = "eqIncome", H = "db040", PSU = "db030",
               w_final = "rb050", ID_level1 = "db030", 
               ID_level2 = "rb030", Dom = "db040",
               period = "period", N_h = NULL, fh_zero = FALSE, 
               Z = NULL, dataset = dataset1, X = NULL,
               X_ID_level1 = NULL, g = NULL, q = NULL,
               datasetX = NULL, confidence = .95,
               percentratio = 1, outp_lin = TRUE,
               outp_res = TRUE)
aa3

# with using fh_zero (finite population correction)
aa4 <- vardomh(Y = "eqIncome", H = "db040", PSU = "db030",
               w_final = "rb050", ID_level1 = "db030",
               ID_level2 = "rb030", Dom = "db040",
               period = "period", N_h = NULL, fh_zero = TRUE, 
               Z = NULL, dataset = dataset1,
               X = NULL, X_ID_level1 = NULL, 
               g = NULL, q = NULL, datasetX = NULL,
               confidence = .95, percentratio = 1,
               outp_lin = TRUE, outp_res = TRUE)
aa4}


}
\references{
Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr
Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second wave onwards? 2012 \cr
Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{https://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr
Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}. \cr
}
\seealso{
\code{\link{domain}},
         \code{\link{lin.ratio}},
         \code{\link{residual_est}},
         \code{\link{var_srs}},
         \code{\link{variance_est}}
}
\keyword{vardpoor}
