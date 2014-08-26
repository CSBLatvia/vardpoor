\name{varpoord}

\alias{varpoord}

\title{Estimation of the variance and deff for sample surveys for indicators on social exclusion and poverty}

\description{Computes the estimation of the variance for indicators on social exclusion and poverty.}

\usage{ varpoord(inc, w_final, income_thres = NULL, wght_thres = NULL,
                 ID_household, id = NULL, H, PSU, N_h, fh_zero = FALSE,
                 PSU_level=TRUE, sort = NULL, Dom = NULL, period = NULL,
                 gender = NULL, dataset = NULL, X = NULL, periodX = NULL,
                 X_ID_household = NULL, ind_gr = NULL, g = NULL, datasetX = NULL,
                 q, percentage = 60, order_quant = 50, alpha = 20,
                 confidence = 0.95, outp_lin = FALSE, outp_res = FALSE,
                 several.ok = FALSE, type = "lin_rmpg") }
                     

\arguments{

\item{inc}{Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{w_final}{Weight variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{income_thres}{Variable (for example equalized disposable income) used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). Variable specified for \code{inc} is used as \code{income_thres} if \code{income_thres} is  not defined.}

\item{wght_thres}{Weight variable used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). Variable specified for \code{weight} is used as \code{wght_thres} if \code{wght_thres} is not defined.}

\item{ID_household}{Variable for household ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{id}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{H}{The unit stratum variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{PSU}{Primary sampling unit variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{N_h}{optional; either a matrix giving the first column - stratum, but the second column - the total of the population in each stratum.}

\item{fh_zero}{by default FALSE; fh is calculated as division of n_h and N_h in each strata, if true, fh value is zero in each strata.}

\item{PSU_level}{by default TRUE; if PSU_level is true, in each strata fh is calculated as division of count of PSU in sample (n_h) and count of PSU in frame(N_h). if PSU_level is false, in each strata fh is calculated as division of count of units in sample (n_h) and count of units in frame(N_h), which calculated as sum of weights.}

\item{sort}{Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{Dom}{Optional variables used to define population domains. If supplied, variables is calculated for each domain. An object convertible to \code{data.frame} or variable names as character vector, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{period}{Optional variable for survey period. If supplied, variables is calculated for each time period. Object convertable to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{dataset}).}

\item{gender}{Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).}

\item{dataset}{Optional survey data object convertible to \code{data.frame}.}

\item{X}{Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{periodX}{Optional variable of the survey periods. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.frame} or variable names as character, column numbers or logical vector (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{X_ID_household}{Variable for household ID codes. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{ind_gr}{Optional variable by which divided independently auxiliary variables. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{g}{Optional variable of the g weights. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{datasetX}{Optional survey data object in household level convertible to \code{data.frame}.}

\item{q}{Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{datasetX}).}

\item{percentage}{A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
\deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.}

\item{order_quant}{A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
\deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.}

\item{alpha}{a numeric value in range \eqn{[0,100]} for the order of the income quantile share ratio (in percentage).}

\item{confidence}{Optional positive value for confidence interval. This variable by default is 0.95.}

\item{outp_lin}{Logical value. if \code{TRUE} linearized values of the ratio estimator will be printed out.}

\item{outp_res}{Logical value. if \code{TRUE} estimated residuals of calibration will be printed out.}

\item{several.ok}{logical specifying if \code{type} should be allowed to have more than one.}

\item{type}{a character vector (of length one unless several.ok is TRUE), example "linarpr","linarpt", "lingpg", "linpoormed", "linrmpg",
   "lingini", "lingini2", "linqsr", "all_choises".}

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
Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr

Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second wave onwards? 2012 \cr

Eurostat Methodologies and Working papers, Standard error estimation for the EU-SILC indicators of poverty and social exclusion, 2013, URL \url{http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-RA-13-024/EN/KS-RA-13-024-EN.PDF}. \cr

Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013}    \cr

Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.\cr

Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

MATTI LANGEL - YVES TILLE, Corrado Gini, a pioneer in balanced sampling and inequality theory. \emph{METRON - International Journal of Statistics}, 2011, vol. LXIX, n. 1, pp. 45-65, URL \url{ftp://metron.sta.uniroma1.it/RePEc/articoli/2011-1-3.pdf}. \cr

Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}

\examples{
data(eusilc)
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"
dataset1 <- dataset[1:1000,]

aa<-varpoord(inc = "eqIncome", w_final = "rb050",
             income_thres = NULL, wght_thres = NULL,
             ID_household = "db030", id = "IDd", 
             H = "db040", PSU = "rb030", N_h = NULL,
             sort = NULL, Dom = "db040",
             gender = NULL, X = NULL,
             X_ID_household = NULL, g = NULL,
             datasetX = NULL,
             q = rep(1, if (is.null(datasetX)) 
                        nrow(as.data.frame(H)) else nrow(datasetX)),
             dataset =  dataset1, percentage=60, order_quant=50,
             alpha = 20, confidence = .95, outp_lin = FALSE,
             outp_res = FALSE, several.ok=FALSE, type="linarpt")
aa


\dontrun{
aa<-varpoord(inc = "eqIncome", w_final = "rb050",
             income_thres = NULL, wght_thres = NULL,
             ID_household = "db030", id = "IDd", 
             H = "db040", PSU = "rb030", N_h = NULL,
             sort = NULL, Dom = "db040",
             gender = NULL, X = NULL,
             X_ID_household = NULL, g = NULL,
             datasetX = NULL,
             q = rep(1, if (is.null(datasetX)) 
                        nrow(as.data.frame(H)) else nrow(datasetX)),
             dataset =  dataset, percentage=60, order_quant=50,
             alpha = 20, confidence = .95, outp_lin = TRUE,
             outp_res = TRUE, several.ok=FALSE, type="all_choises")
aa$lin_out[20:40]
aa$res_out[20:40]
}


} 

\seealso{
\code{\link{vardom}}, \code{\link{vardomh}}, \code{\link{linarpt}}} 
\keyword{varpoord}

  
