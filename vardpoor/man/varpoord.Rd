\name{varpoord}
\alias{varpoord}
\title{Estimation of the variance and deff for sample surveys for indicators on social exclusion and poverty}
\description{Computes the estimation of the variance for indicators on social 
exclusion and poverty.}
\usage{ varpoord(inc, w_final, income_thres = NULL, wght_thres = NULL,
                 ID_household, id = NULL, H, PSU, N_h, fh_zero = FALSE,
                 PSU_level=TRUE, sort = NULL, Dom = NULL, period = NULL,
                 gender = NULL, dataset = NULL, X = NULL, periodX = NULL,
                 X_ID_household = NULL, ind_gr = NULL, g = NULL, datasetX = NULL,
                 q, percentage = 60, order_quant = 50, alpha = 20,
                 confidence = 0.95, outp_lin = FALSE, outp_res = FALSE,
                 na.rm = FALSE, several.ok = FALSE, type = "lin_rmpg") }
                     

\arguments{
  \item{inc}{either a numeric vector, 1 column data.frame,
  matrix, data.table giving the equivalized disposable
  income, or (if \code{dataset} is not \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.}

  \item{w_final}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal sample
  weights, or (if \code{dataset} is not  \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}.}

  \item{income_thres}{either a numeric vector, 1 column
  data.frame, matrix, data.table giving the equivalized
  disposable income for computation and linearization
  of the poverty threshold, or (if \code{dataset} is not
  \code{NULL}) a character string, an integer or a logical
  vector (length is the same as 'dataset' column count)
  specifying the corresponding column of \code{dataset}.
  If \code{income_thres} is \code{NULL}, it is \code{inc}.}

  \item{wght_thres}{either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal sample
  weights for computation and linearization of the poverty
  threshold, or (if \code{dataset} is not \code{NULL}) a
  character string, an integer or a logical vector (length
  is the same as 'dataset' column count) specifying the
  corresponding column of \code{dataset}. If \code{wght_thres}
  is \code{NULL}, it is \code{weight}.}

  \item{ID_household}{either 1 column data.frame, matrix, data.table
   with column names giving the household IDs, or
  (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as
  'dataset' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{id}{optional; either 1 column data.frame, matrix,
  data.table with column names giving the personal IDs, or
  (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as
  'dataset' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{H}{either 1 column data.frame, matrix, data.table with
  column name giving elements  indicating the unit stratum,
  or (if \code{dataset} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as 'dataset'
  column count) specifying the corresponding column of \code{dataset}.}

  \item{PSU}{either 1 column data.frame, matrix, data.table giving
  primary sampling unit, or (if \code{dataset} is not \code{NULL})
  a character string, an integer or a logical vector (length is the
  same as 'dataset' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{N_h}{either a matrix giving the first column - stratum, but
  the second column - the total of the population in each stratum.}

  \item{fh_zero}{by default FALSE; fh is calculated as division of n_h and N_h 
  in each strata, if true, fh value is zero in each strata.}

  \item{PSU_level}{by default TRUE; if PSU_level is true, in each strata fh
  is calculated as division of count of PSU in sample (n_h) and count of PSU in frame(N_h). 
  if PSU_level is false, in each strata fh is calculated as division of count of units in
  sample (n_h) and count of units in frame(N_h), which calculated as sum of weights. }

  \item{sort}{optional; either a numeric vector, 1 column
  data.frame, matrix, data.table giving the personal IDs to
  be used as tie-breakers for sorting, or (if \code{dataset}
  is not \code{NULL}) a character string, an integer or a
  logical vector (length is the same as 'dataset' column count)
  specifying the corresponding column of \code{dataset}.}

  \item{Dom}{optional; either a data.frame, matrix, data.table with
  column names giving different domains, or (if \code{dataset} is
  not \code{NULL}) character strings, integers or a logical vectors
  (length is the same as 'dataset' column count) specifying the
  corresponding columns of \code{dataset}. This variable is \code{NULL}
  by default. If supplied, the values for each domain are computed.}

  \item{period}{optional; either a data.frame, matrix, data.table
  with column names giving different periods, or (if 
  \code{dataset} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{dataset}. If supplied, the values for
  each period are computed independently.
  }

  \item{gender}{either a factor giving the gender, or (if
  \code{dataset} is not \code{NULL}) a character string, an
  integer or a logical vector (length is the same as 'dataset'
  column count) specifying the corresponding column of
  \code{dataset}.}

  \item{dataset}{an optional; name of the individual dataset 
  \code{data.frame}.}

  \item{X}{optional; either a data.frame, matrix, data.table giving
  auxiliary variables, or (if \code{datasetX} is not \code{NULL})
  character strings, integers or a logical vectors (length is the
  same as 'dataset' column count) specifying the corresponding
  columns of \code{dataset}. This variable is \code{NULL} by default.}

  \item{periodX}{optional; either a data.frame, matrix, data.table
  with column names giving different periods for data X, or (if 
  \code{datasetX} is not \code{NULL}) character strings,
  integers or a logical vectors (length is the same as 
  'dataset' column count) specifying the corresponding
  columns of \code{datasetX}. If supplied, the values for
  each periodX are computed independently.
  }

  \item{X_ID_household}{either 1 column data.frame, matrix, data.table
  with column name giving the household IDs for auxiliary variables,
  or (if \code{datasetX} is not \code{NULL}) a character string,
  an integer or a logical vector (length is the same as
  'datasetX' column count) specifying the corresponding
  column of \code{dataset}.}

  \item{ind_gr}{optional; either a vector, 1 column data.frame,
  matrix, data.table giving the variable by which divided independently
  auxiliary variables, or (if \code{datasetX} is not
  \code{NULL}) a character string,  an integer or a logical vector
  (length is the same as 'datasetX' column count) specifying the
  corresponding column of \code{datasetX}. This variable is \code{NULL}
  by default.}

  \item{g}{optional; either a numeric vector, 1 column data.frame,
  matrix, data.table giving the g weights, or (if \code{datasetX}
  is not \code{NULL}) a character string,  an integer or a logical
  vector (length is the same as 'dataset' column count) specifying
  the corresponding column of \code{datasetX}. This variable is
  \code{NULL} by default.}

  \item{datasetX}{an optional; name of the individual dataset
  \code{data.frame}.}

  \item{q}{optional; either a numeric vector, 1 column data.frame,
  matrix, data.table giving the positive values accounting for
  heteroscedasticity, or (if \code{datasetX} is not \code{NULL}) a
  character string,  an integer or a logical vector (length is the
  same as 'dataset' column count) specifying the corresponding
  column of \code{datasetX}. This variable is \code{NULL} by default.}

  \item{percentage}{a numeric value in \eqn{[0,100]} giving
  the percentage of the income quantile to be used for the
  at-risk-of-poverty threshold (see \code{\link{linarpt}}).}

  \item{order_quant}{a numeric value in \eqn{[0,100]} giving the
  order of the income quintale (in percentage) to be used for
  the at-risk-of-poverty threshold (see \code{\link{linarpt}}).}

  \item{alpha}{a numeric value in \eqn{[0,100]} giving
  the Order of the income quantile share ratio (in percentage).}

  \item{confidence}{optional; either a positive value for confidence
  interval. This variable by default is 0.95.}

  \item{outp_lin}{logical. if \code{TRUE} linearized
  values will be printed out}

  \item{outp_res}{logical. if \code{TRUE} estimated residuals
  of calibration will be printed out}

  \item{na.rm}{a logical indicating whether missing values
  should be removed.}

  \item{several.ok}{logical specifying if \code{type} should be
   allowed to have more than one.}

  \item{type}{a character vector (of length one unless several.ok is TRUE),
   example "linarpr","linarpt", "lingpg", "linpoormed", "linrmpg",
   "lingini", "lingini2", "linqsr", "all_choises".}

}
\value{
  The function returns values: 
  \item{estim}{a \code{data.frame} containing the estimation(s)
  by domain, or (if \code{Dom} is \code{NULL}) totals.}

  \item{var}{a \code{matrix} containing the values of the
  variance estimation by domains or (if \code{Dom} is \code{NULL})
  totals.}

  \item{se}{a \code{matrix} containing the values of the
  standart error by domains or (if \code{Dom} is \code{NULL})
  totals.}

  \item{rse}{a \code{data.frame} containing the values of the
  relative standart error (coefficient of variation) by domains
  or (if \code{Dom} is \code{NULL}) totals in percentage.}

  \item{cv}{a \code{data.frame} containing the values of the
  relative standart error (coefficient of variation) by domains
  or (if \code{Dom} is \code{NULL}) totals.}

  \item{absolute_margin_of_error}{a \code{matrix} containing
  the values of the absolute margin of error by domains or
  (if \code{Dom} is \code{NULL}) totals.}

  \item{relative_margin_of_error}{a \code{matrix} containing
  the values of the relative margin of error by domains or
  (if \code{Dom} is \code{NULL}) totals.}

  \item{CI_lower}{a \code{data.frame} containing the values of
  the confidence interval lower bound by domains or (if \code{Dom}
  is \code{NULL}) totals.}

  \item{CI_upper}{a \code{data.frame} containing the values of
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

  \item{lin_out}{a \code{data.table} containing the
  linearized values with ID_household and id.}

  \item{res_out}{a \code{data.table} containing the estimated
  residuals of calibration with id and PSU.}

  \item{all_result}{a \code{data.frame} containing all previosly definited
  values together by domains or (if \code{Dom} is \code{NULL}) totals.}
}
\references{ 
  Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard
  error estimation and other related sampling issues in EU-SILC,
  URL \url{http://www.cros-portal.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013}    \cr

  Working group on Statistics on Income and Living
  Conditions (2004) Common cross-sectional EU indicators
  based on EU-SILC; the gender pay gap.  \emph{EU-SILC
  131-rev/04}, Eurostat.\cr
  
  Guillaume Osier (2009). Variance estimation for complex
  indicators of poverty and inequality. \emph{Journal of
  the European Survey Research Association}, Vol.3, No.3,
  pp. 167-195, ISSN 1864-3361,  
  URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr

  MATTI LANGEL - YVES TILLE, Corrado Gini, a pioneer in balanced sampling
  and inequality theory. \emph{METRON - International Journal of Statistics},
  2011, vol. LXIX, n. 1, pp. 45-65, 
  URL \url{ftp://metron.sta.uniroma1.it/RePEc/articoli/2011-1-3.pdf}. \cr

  Jean-Claude Deville (1999). Variance estimation for complex statistics
  and estimators: linearization and residual techniques. Survey
  Methodology, 25, 193-203,
  URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}. \cr
}
\examples{
data(eusilc)
dataset <- data.frame(1:nrow(eusilc),eusilc)
colnames(dataset)[1] <- "IDd"

aa<-varpoord("eqIncome", "rb050", income_thres = NULL,
             wght_thres = NULL, ID_household = "db030",
             id = NULL, H="db040",
             PSU="rb030", N_h=NULL, sort = NULL,
             Dom = "db040", gender = NULL, X = NULL,
             X_ID_household = NULL,
             g = NULL,
             datasetX = NULL,
             q = rep(1, if (is.null(datasetX)) 
                        nrow(as.data.frame(H)) else nrow(datasetX)),
             dataset =  dataset, percentage=60, order_quant=50,
             alpha = 20, confidence = .95, outp_lin = TRUE,
             outp_res = TRUE, na.rm=FALSE,
             several.ok=FALSE, type="lingini")
aa$lin_out[20:40]
aa$res_out[20:40]
} 

\seealso{
\code{\link{vardom}}, \code{\link{vardomh}}, \code{\link{linarpt}}} 
\keyword{varpoord}

  