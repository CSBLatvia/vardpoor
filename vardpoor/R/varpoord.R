#' Estimation of the variance and deff for sample surveys for indicators on social exclusion and poverty
#' 
#' @description Computes the estimation of the variance for indicators on social exclusion and poverty.
#' 
#' @param Y Study variable (for example equalized disposable income or gross pension income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param age Age variable. One dimensional object convertible to one-column \code{data.frame} or variable name as character, column number.
#' @param pl085 Retirement variable (Number of months spent in retirement or early retirement). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param month_at_work}{Variable for total number of month at work (sum of the number of months spent at full-time work as employee, number of months spent at part-time work as employee, number of months spent at full-time work as self-employed (including family worker), number of months spent at part-time work as self-employed (including family worker)).  One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y_den Denominator variable (for example gross individual earnings). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y_thres Variable (for example equalized disposable income) used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. Variable specified for \code{inc} is used as \code{income_thres} if \code{income_thres} is  not defined.
#' @param wght_thres Weight variable used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. Variable specified for \code{weight} is used as \code{wght_thres} if \code{wght_thres} is not defined.
#' @param  ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level2 Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param N_h Number of primary sampling units in population for each stratum (and period if \code{period} is not \code{NULL}). If \code{N_h = NULL} and \code{fh_zero = FALSE} (default), \code{N_h} is estimated from sample data as sum of weights (\code{w_final}) in each stratum (and period if \code{period} is not \code{NULL}).
#' Optional for single-stage sampling design as it will be estimated from sample data. Recommended for multi-stage sampling design as \code{N_h} can not be correctly estimated from the sample data in this case. If \code{N_h} is not used in case of multi-stage sampling design (for example, because this information is not available), it is advisable to set \code{fh_zero = TRUE}.
#' If \code{period} \bold{is} \code{NULL}. A two-column data object convertible to \code{data.table} with rows for each stratum. The first column should contain stratum code. The second column - the number of primary sampling units in the population of each stratum.
#' If \code{period} \bold{is not} \code{NULL}. A three-column data object convertible to \code{data.table} with rows for each intersection of strata and period. The first column should contain period. The second column should contain stratum code. The third column - the number of primary sampling units in the population of each stratum and period.
#' @param PSU_sort optional; if PSU_sort is defined, then variance is calculated for systematic sample.
#' @param fh_zero by default FALSE; \code{fh} is calculated as division of n_h and N_h in each strata, if TRUE, \code{fh} value is zero in each strata.
#' @param PSU_level by default TRUE; if PSU_level is TRUE, in each strata \code{fh} is calculated as division of count of PSU in sample (n_h) and count of PSU in frame(N_h). if PSU_level is FALSE, in each strata \code{fh} is calculated as division of count of units in sample (n_h) and count of units in frame(N_h), which calculated as sum of weights.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables is calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, variables is calculated for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param gender Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param dataset Optional survey data object convertible to \code{data.frame}.
#' @param X Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param periodX Optional variable of the survey periods. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param X_ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ind_gr Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param g Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param datasetX Optional survey data object in household level convertible to \code{data.table}.
#' @param percentage A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
#'  \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'  For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.
#' @param order_quant A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
#' \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#' For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param alpha a numeric value in range \eqn{[0,100]} for the order of the income quantile share ratio (in percentage).
#' @param confidence Optional positive value for confidence interval. This variable by default is 0.95.
#' @param outp_lin Logical value. If \code{TRUE} linearized values of the ratio estimator will be printed out.
#' @param outp_res Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.
#' @param type a character vector (of length one unless several.ok is TRUE), example "linarpr","linarpt", "lingpg", "linpoormed", "linrmpg", "lingini", "lingini2", "linqsr", "linarr", "linrmir".
#' 
#' @return A list with objects are returned by the function:
#' \itemize{
#'    \item \code{lin_out} - a \code{data.table} containing the linearized values of the ratio estimator with ID_level2 and PSU.
#'    \item \code{res_out} - a \code{data.table} containing the estimated residuals of calibration with ID_level1 and PSU. 
#'    \item \code{betas} - a numeric \code{data.table} containing the estimated coefficients of calibration.
#'    \item \code{all_result} - a \code{data.table}, which containing variables: \cr
#'       \code{respondent_count} - the count of respondents, \cr
#'       \code{pop_size} - the estimated size of population, \cr
#'       \code{n_nonzero} - the count of respondents, who answers are larger than zero, \cr
#'       \code{value} - the estimated value, \cr
#'       \code{var} - the estimated variance, \cr
#'       \code{se} - the estimated standard error, \cr
#'       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'       \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
#'       \code{relative_margin_of_error} - the estimated relative margin of error in percentage, \cr
#'       \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'       \code{CI_upper} - the estimated confidence interval upper bound, \cr
#'       \code{confidence_level} - the positive value for confidence interval, \cr 
#'       \code{S2_y_HT} - the estimated variance of the y variable in case of total or the estimated variance of the linearised variable in case of the ratio of two totals using non-calibrated weights, \cr
#'       \code{S2_y_ca} - the estimated variance of the y variable in case of total or the estimated variance of the linearised variable in case of the ratio of two totals using calibrated weights, \cr
#'       \code{S2_res} - the estimated variance of the regression residuals, \cr
#'       \code{var_srs_HT} - the estimated variance of the HT estimator under SRS for household, \cr
#'       \code{var_cur_HT} - the estimated variance of the HT estimator under current design for household, \cr
#'       \code{var_srs_ca} - the estimated variance of the calibrated estimator under SRS  for household, \cr
#'       \code{deff_sam} - the estimated design effect of sample design for household, \cr
#'       \code{deff_est} - the estimated design effect of estimator for household, \cr
#'       \code{deff} - the overall estimated design effect of sample design and estimator for household
#'   }
#'  
#'      
#' @references
#' Eric Graf and Yves Tille, Variance Estimation Using Linearization for Poverty and Social Exclusion Indicators,  Survey Methodology, June 2014 61 Vol. 40, No. 1, pp. 61-79, Statistics Canada, Catalogue no. 12-001-X, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/12-001-x2014001-eng.pdf} \cr
#' Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second wave onwards? 2012 \cr
#' Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}. \cr
#' Eurostat Methodologies and Working papers, Standard error estimation for the EU-SILC indicators of poverty and social exclusion, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}. \cr
#' Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
#' Matti Langel, Yves Tille, Corrado Gini, a pioneer in balanced sampling and inequality theory. \emph{Metron - International Journal of Statistics}, 2011, vol. LXIX, n. 1, pp. 45-65, URL \doi{10.1007/BF03263549}. \cr
#' Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr
#' Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
#' Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.\cr
#' 
#' 
#' @seealso \code{\link{vardom}}, \code{\link{vardomh}}, \code{\link{linarpt}}
#' 
#' @keywords varpoord
#' 
#' 
#' @examples
#' library("data.table")
#' library("laeken")
#' data("eusilc")
#' dataset <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#' dataset1 <- dataset[1 : 1000]
#'  
#' #use dataset1 by default without using fh_zero (finite population correction)
#' aa <- varpoord(Y = "eqIncome", w_final = "rb050",
#'                Y_thres = NULL, wght_thres = NULL,
#'                ID_level1 = "db030", ID_level2 = "IDd", 
#'                H = "db040", PSU = "rb030", N_h = NULL,
#'                sort = NULL, Dom = NULL,
#'                gender = NULL, X = NULL,
#'                X_ID_level1 = NULL, g = NULL,
#'                q = NULL, datasetX = NULL,             
#'                dataset = dataset1, percentage = 60,
#'                order_quant = 50L, alpha = 20, 
#'                confidence = .95, outp_lin = FALSE,
#'                outp_res = FALSE, type = "linarpt")
#' aa
#'  
#' \dontrun{
#'  # use dataset1 by default with using fh_zero (finite population correction)
#'  aa2 <- varpoord(Y = "eqIncome", w_final = "rb050",
#'                  Y_thres = NULL, wght_thres = NULL,
#'                  ID_level1 = "db030", ID_level2 = "IDd", 
#'                  H = "db040", PSU = "rb030", N_h = NULL,
#'                  fh_zero = TRUE, sort = NULL, Dom = "db040",
#'                  gender = NULL, X = NULL, X_ID_level1 = NULL,
#'                  g = NULL, datasetX = NULL, dataset =  dataset1,
#'                  percentage = 60, order_quant = 50L,
#'                  alpha = 20, confidence = .95, outp_lin = FALSE,
#'                  outp_res = FALSE, type = "linarpt")
#'  aa2
#'  aa2$all_result
#'  
#'  
#'  # using dataset1
#'  aa4 <- varpoord(Y = "eqIncome", w_final = "rb050",
#'                  Y_thres = NULL, wght_thres = NULL,
#'                  ID_level1 = "db030", ID_level2 = "IDd", 
#'                  H = "db040", PSU = "rb030", N_h = NULL,
#'                  sort = NULL, Dom = "db040",
#'                  gender = NULL, X = NULL,
#'                  X_ID_level1 = NULL, g = NULL,
#'                  datasetX = NULL, dataset =  dataset,
#'                  percentage = 60, order_quant = 50L,
#'                  alpha = 20, confidence = .95,
#'                  outp_lin = TRUE, outp_res = TRUE,
#'                  type = "linarpt")
#'  aa4$lin_out[20 : 40]}
#'  
#' 
#' @import data.table
#' 
#' @export varpoord


varpoord <- function(Y, w_final,
                     age = NULL,
                     pl085 = NULL,
                     month_at_work=NULL,
                     Y_den = NULL,
                     Y_thres = NULL,
                     wght_thres = NULL,
                     ID_level1,
                     ID_level2 = NULL,
                     H, PSU, N_h,
                     PSU_sort = NULL,
                     fh_zero = FALSE,
                     PSU_level=TRUE,
                     sort = NULL,
                     Dom = NULL,
                     period = NULL,
                     gender = NULL,
                     dataset = NULL,
                     X = NULL,
                     periodX = NULL,
                     X_ID_level1 = NULL,
                     ind_gr = NULL,
                     g = NULL,
                     q = NULL,
                     datasetX = NULL,
                     percentage = 60,
                     order_quant = 50,
                     alpha = 20,
                     confidence = .95,
                     outp_lin = FALSE,
                     outp_res = FALSE,
                     type="linrmpg") {

  ### Checking

  all_choices <- c("linarpr","linarpt","lingpg","linpoormed",
                   "linrmpg","lingini","lingini2", "linqsr", "linrmir", "linarr")
  type <- tolower(type)
  type <- match.arg(type, all_choices, length(type) > 1)

  fh_zero <- check_var(vars = fh_zero, varn = "fh_zero", varntype = "logical")
  PSU_level <- check_var(vars = PSU_level, varn = "PSU_level", varntype = "logical")
  outp_lin <- check_var(vars = outp_lin, varn = "outp_lin", varntype = "logical")
  outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical")

  percentage <- check_var(vars = percentage, varn = "percentage", varntype = "numeric0100")
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "numeric0100")
  alpha <- check_var(vars = alpha, varn = "alpha", varntype = "numeric0100")
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")

  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 ncols = 1, isnumeric = TRUE,
                 isvector = TRUE, grepls = "__")
  Ynrow <- length(Y)

  w_final <- check_var(vars = w_final, varn = "weight",
                       dataset = dataset, ncols = 1,
                       Ynrow = Ynrow, isnumeric = TRUE,
                       isvector = TRUE)

  age <- check_var(vars = age, varn = "age", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE,
                   mustbedefined = any(c("linarr", "linrmir") %in% type))

  pl085 <- check_var(vars = pl085, varn = "pl085", dataset = dataset,
                     ncols = 1, Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE,
                     mustbedefined = any(type == "linarr"))

  month_at_work <- check_var(vars = month_at_work, varn = "month_at_work",
                             dataset = dataset, ncols = 1, Ynrow = Ynrow,
                             isnumeric = TRUE, isvector = TRUE,
                             mustbedefined = any(type == "linarr"))

  gender <- check_var(vars = gender, varn = "gender", dataset = dataset,
                      ncols = 1, Ynrow = Ynrow, isnumeric = TRUE,
                      isvector = TRUE, mustbedefined = any(type == "lingpg"))

  Y_den <- check_var(vars = Y_den, varn = "Y_den", dataset = dataset,
                     ncols = 1, Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE,
                     mustbedefined = any("linarr" == type))

  Y_thres <- check_var(vars = Y_thres, varn = "Y_thres",
                       dataset = dataset, ncols = 1,
                       Ynrow = Ynrow, mustbedefined = FALSE,
                       isnumeric = TRUE, isvector = TRUE)

  wght_thres <- check_var(vars = wght_thres, varn = "wght_thres",
                          dataset = dataset, ncols = 1,
                          Ynrow = Ynrow, mustbedefined = FALSE,
                          isnumeric = TRUE, isvector = TRUE)

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, ischaracter = TRUE)

  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, ischaracter = TRUE,
                         namesID1 = names(ID_level1), periods = period)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 ischaracter = TRUE, namesID1 = names(ID_level1),
                 dif_name = "dataH_stratas")

  sort <- check_var(vars = sort, varn = "sort",
                    dataset = dataset, ncols = 1,
                    Ynrow = Ynrow, mustbedefined = FALSE,
                    isnumeric = TRUE, isvector = TRUE)

  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, mustbedefined = FALSE,
                      duplicatednames = TRUE)

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   grepls = "__")

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, namesID1 = names(ID_level1))

  PSU_sort <- check_var(vars = PSU_sort, varn = "PSU_sort", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        isvector = TRUE, mustbedefined = FALSE, PSUs = PSU)

  if(!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) |
      !is.null(periodX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
       X <- check_var(vars = X, varn = "X", dataset = datasetX,
                      check.names = TRUE, isnumeric = TRUE,
                      dif_name = c(names(period) , "g", "q"), dX = "X")
       Xnrow <- nrow(X)

       ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                           dataset = datasetX, ncols = 1, Xnrow = Xnrow, dX = "X",
                           ischaracter = TRUE, dif_name = c(names(period) , "g", "q"))

       g <- check_var(vars = g, varn = "g", dataset = datasetX,
                      ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                      isvector = TRUE, dX = "X")

       q <- check_var(vars = q, varn = "q", dataset = datasetX,
                      ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                      isvector = TRUE, dX = "X")

       periodX <- check_var(vars = periodX, varn = "periodX",
                            dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                            ischaracter = TRUE, mustbedefined = !is.null(period),
                            duplicatednames = TRUE, varnout = "period",
                            varname = names(period), dX = "X")

       X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                ischaracter = TRUE, varnout = "ID_level1",
                                varname = names(ID_level1), periods = period,
                                periodsX = periodX, ID_level1 = ID_level1, dX = "X")
   }

 # N_h
 np <- sum(ncol(period))
 if (!is.null(N_h)) {
   N_h <- data.table(N_h)
   if (anyNA(N_h)) stop("'N_h' has missing values")
   if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2, " columns"))
   if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numeric")

   nams <- c(names(period), names(H))
   if (all(nams %in% names(N_h))) {N_h[, (nams) := lapply(.SD, as.character), .SDcols = nams]
       } else stop(paste0("All strata titles of 'H'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h'"))

   if (is.null(period)) {
     if (any(is.na(merge(unique(H), N_h, by = names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
     if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                pH <- NULL }
     setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
 }

  N <- dataset <- datasetX <- NULL

  if (is.null(Y_thres)) Y_thres <- Y
  if (is.null(wght_thres)) wght_thres <- w_final
  psusn <- as.integer(!is.null(PSU_sort))


  # Design weights
  if (!is.null(X)) {
             ID_level1h <- data.table(ID_level1)
             if (!is.null(period)) { ID_level1h <- data.table(period, ID_level1h)
                                     X_ID_level1 <- data.table(period, X_ID_level1)
                              }
             idhx <- data.table(X_ID_level1, g)
             setnames(idhx, names(idhx)[c(1 : (ncol(idhx) - 1))], names(ID_level1h))
             idg <- merge(ID_level1h, idhx, by = names(ID_level1h), sort = FALSE)
             w_design <- w_final / idg[[ncol(idg)]]
             idg <- data.table(idg, w_design = w_design)
             idh <- idg[, .N, keyby = c(names(ID_level1h), "w_design")]
             if (nrow(X) != nrow(idh))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- ID_level1h <- NULL
      } else w_design <- w_final

  ### Calculation
  sar_nr <- respondent_count <- pop_size <- n_nonzero <- NULL
  nhs <- data.table(respondent_count = 1, pop_size = w_final,
                    n_nonzero = as.integer(abs(Y) > .Machine$double.eps))
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                       keyby = eval(names(nhs)[0:2-ncol(nhs)]),
                                                      .SDcols = c("respondent_count", "pop_size", "n_nonzero")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm=TRUE),
                                                     .SDcols=c("respondent_count", "pop_size", "n_nonzero")]

  estim <- c()
  aH <- names(H)
  idper <- copy(ID_level2)
  Y1sort <- Y1asort <- NULL
  aPSU <- names(PSU)
  if (!is.null(period)) idper <- data.table(idper, period)

  Y1 <- data.table(idper, ID_level1, H, PSU, check.names = TRUE)
  if (!is.null(PSU_sort)) Y1 <- data.table(Y1, PSU_sort, check.names = TRUE)
  Y1 <- data.table(Y1, w_design, w_final)


  Y1[, Y1sort := .I]
  setkeyv(Y1, names(idper))
  value <- NULL

  if ("linarpt" %in% type) {
       varpt <- linarpt(Y = Y, id = ID_level2, weight = w_final,
                        sort = sort, Dom = Dom, period = period,
                        dataset = NULL, percentage = percentage,
                        order_quant = order_quant, var_name = "lin_arpt",
                        checking = FALSE)
       Y1 <- merge(Y1, varpt$lin, all.x = TRUE)
       esti <- data.table("ARPT", varpt$value, NA)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varpt <- esti <- NULL
     }
  if ("linarpr" %in% type) {
       varpr <- linarpr(Y = Y, id = ID_level2, weight = w_final,
                        Y_thres = Y_thres,
                        wght_thres = wght_thres, sort = sort,
                        Dom = Dom, period = period, dataset = NULL,
                        percentage = percentage,
                        order_quant = order_quant,
                        var_name = "lin_arpr",
                        checking = FALSE)

       Y1 <- merge(Y1, varpr$lin, all.x = TRUE)

       esti <- data.table("ARPR", varpr$value, NA)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varpr <- esti <- NULL
     }
  if (("lingpg" %in% type) & all(!is.null(gender))) {
        vgpg <- lingpg(Y = Y, gender = gender, id = ID_level2,
                       weight = w_final, sort = sort,
                       Dom = Dom, period = period, dataset = NULL,
                       var_name = "lin_gpg", checking = FALSE)

        Y1 <- merge(Y1, vgpg$lin, all.x = TRUE)

        esti <- data.table("GPG", vgpg$value, NA)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgpg <- esti <- NULL
     }
  if ("linpoormed" %in% type) {
        vporm <- linpoormed(Y = Y, id = ID_level2, weight = w_final,
                            sort = sort, Dom = Dom, period = period,
                            dataset = NULL, percentage = percentage,
                            order_quant = order_quant, var_name = "lin_poormed",
                            checking = FALSE)
        Y1 <- merge(Y1, vporm$lin, all.x = TRUE)

        esti <- data.table("POORMED", vporm$value, NA)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vporm <- esti <- NULL
     }
  if ("linrmpg" %in% type) {
        vrmpg <- linrmpg(Y = Y, id = ID_level2, weight = w_final,
                         sort = sort, Dom = Dom, period = period,
                         dataset = NULL, percentage = percentage,
                         order_quant = order_quant, var_name = "lin_rmpg",
                         checking = FALSE)
        Y1 <- merge(Y1, vrmpg$lin, all.x = TRUE)

        esti <- data.table("RMPG", vrmpg$value, NA)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vrmpg <- esti <- NULL
      }
  if ("linqsr" %in% type) {
        vqsr <- linqsr(Y = Y, id = ID_level2, weight = w_final,
                       sort = sort, Dom = Dom, period = period,
                       dataset = NULL, alpha = alpha, var_name = "lin_qsr",
                       checking = FALSE)
        Y1 <- merge(Y1, vqsr$lin, all.x = TRUE)

        esti <- data.table("QSR", vqsr$value)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vqsr <- esti <- NULL
     }
  if ("lingini" %in% type) {
        vgini <- lingini(Y = Y, id = ID_level2, weight = w_final,
                         sort = sort, Dom=Dom, period = period,
                         dataset = NULL, var_name = "lin_gini",
                         checking = FALSE)
        Y1 <- merge(Y1, vgini$lin, all.x = TRUE)

        esti <- data.table("GINI", vgini$value)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgini <- esti <- NULL
     }
  if ("lingini2" %in% type) {
       vgini2 <- lingini2(Y = Y, id = ID_level2, weight = w_final,
                          sort = sort, Dom = Dom, period = period,
                          dataset = NULL, var_name = "lin_gini2",
                          checking = FALSE)
       Y1 <- merge(Y1, vgini2$lin, all.x = TRUE)

       esti <- data.table("GINI2", vgini2$value)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       vgini2 <- esti <- NULL
     }
  if (("linrmir" %in% type) & all(!is.null(age))) {
       vrmir <- linrmir(Y = Y, id = ID_level2, age = age,
                        weight = w_final, sort = sort, Dom = Dom,
                        period = period, dataset = NULL,
                        order_quant = order_quant,
                        var_name = "lin_rmir",
                        checking = FALSE)
       Y1 <- merge(Y1, vrmir$lin, all.x = TRUE)

       esti <- data.table("RMIR", vrmir$value, NA)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       vrmir <- esti <- NULL
    }
  if (("linarr" %in% type) & all(!is.null(age)
                & !is.null(pl085) & !is.null(month_at_work))) {

       varr <- linarr(Y = Y, Y_den = Y_den, id = ID_level2, age = age,
                      pl085 = pl085, month_at_work = month_at_work,
                      weight = w_final, sort = sort, Dom = Dom,
                      period = period, dataset = NULL,
                      order_quant = order_quant,
                      var_name = "lin_arr",
                      checking = FALSE)
       Y1 <- merge(Y1, varr$lin, all.x = TRUE)

       esti <- data.table("ARR", varr$value, NA)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varr <- esti <- NULL
    }


  estim[, variable := paste0("lin_", tolower(type))]
  nDom <- names(Dom)
  if (!is.null(nDom)) estim[, (paste0(nDom, "at1at")) := lapply(nDom, function(x) paste(x, get(x), sep = "."))]

  Dom <- estim[, "variable"]
  if (!is.null(nDom)) Dom <- estim[, c("variable", paste0(nDom, "at1at")), with = FALSE]

  estim$variable <- do.call("paste", c(as.list(Dom), sep = "__"))
  estim[, variable := str_replace_all(variable, "[ ]", ".")]
  if (!is.null(nDom)) estim[, (paste0(nDom, "at1at")) := NULL]
  all_result <- estim


  setkey(Y1, Y1sort)
  Y1[, Y1sort := NULL]

  estim <- .SD <- lin_outp <- NULL
  if (outp_lin) lin_outp <- Y1[, c(-(3 : 5) - np), with = FALSE]

  Y2 <- Y1[, lapply(.SD, sum, na.rm = TRUE), by = c(names(Y1)[c(2 : (6 + np + psusn))]), .SDcols = names(Y1)[- (1 : (6 + np + psusn))]]
  Y3 <- Y2[, c(-(1 : (5 + np + psusn))), with = FALSE]

  idper <- period <- NULL
  if (np > 0) period <- Y2[, c(1 : np), with = FALSE]

  ID_level1h <- Y2[, np + 1, with = FALSE]
  H <- Y2[, np + 2, with = FALSE]
  setnames(H, names(H), aH)

  PSU <- Y2[, np + 3, with = FALSE]
  setnames(PSU, names(PSU), aPSU)

  if (!is.null(PSU_sort)) PSU_sort <- Y2[[np + 4]]

  w_design2 <- Y2[[np + 4 + psusn]]
  w_final2 <- Y2[[np + 5 + psusn]]

  Y1 <- Y2 <- NULL

  # Calibration

  res_outp <- betas <- variable <- NULL
  if (!is.null(X)) {
       if (np > 0) ID_level1h <- data.table(period, ID_level1h)
       setnames(ID_level1h, names(ID_level1h), names(X_ID_level1))
       X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
       D1 <- merge(ID_level1h, X0, by = names(ID_level1h), sort = FALSE)
       ind_gr <- D1[, np + 2, with = FALSE]
       if (!is.null(period)) ind_gr <- data.table(D1[, names(periodX), with = FALSE], ind_gr)
       ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))

       lin1 <- lapply(split(Y3[, .I], ind_period), function(i) {
                            resid <- residual_est(Y = Y3[i],
                                                  X = D1[i, (np + 5) : ncol(D1), with = FALSE],
                                                  weight = w_design2[i],
                                                  q = D1[i][["q"]],
                                                  dataset = NULL,
                                                  checking = FALSE)
                            pers0 <- ind_gr[i, .N, keyby = c(names(ind_gr))]
                            list(data.table(sar_nr = i, resid$residuals),
                                 data.table(pers0[, N := NULL], resid$betas))
                                    })
       Y4 <- rbindlist(lapply(lin1, function(x) x[[1]]))
       betas <- rbindlist(lapply(lin1, function(x) x[[2]]))
       setkeyv(Y4, "sar_nr")
       Y4[, sar_nr := NULL]
       if (outp_res) res_outp <- data.table(ID_level1h, PSU, w_final2, Y4)
   } else Y4 <- Y3
   lin1 <- X0 <- D1 <- ind_gr <- ID_level1h <- X_ID_level1 <- q <- g <- NULL

  var_est <- variance_est(Y = Y4, H = H, PSU = PSU, w_final = w_final2,
                          N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                          PSU_sort = PSU_sort, period = period, dataset = NULL,
                          msg = "Current variance estimation",
                          checking = FALSE)
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- merge(var_est, all_result, all = TRUE, by = c(names(period), "variable"))

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y3, H = H, PSU = PSU, w_final = w_design2,
                             N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                             PSU_sort = PSU_sort, period = period, dataset = NULL,
                             msg = "Variance of HT estimator under current design",
                             checking = FALSE)
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT, by = c(names(period), "variable"))
  H <- PSU <- PSU_sort <-  N_h <- var_est <- var_cur_HT <- NULL

  # Variance of HT estimator under SRS
  if (is.null(period)) {
           varsrs <- var_srs(Y = Y3, w = w_design2)
           S2_y_HT <- varsrs$S2p
           S2_y_ca <- var_srs(Y = Y3, w = w_final2)$S2p
           var_srs_HT <- varsrs$varsrs
       } else {
           period_agg <- unique(period)
           lin1 <- lapply(1 : nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y3)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          varsrs <- var_srs(Y = Y3[ind], w = w_design2[ind])
                          S2_y_ca <- var_srs(Y = Y3[ind], w = w_final2[ind])$S2p
                          list(S2p = data.table(period_agg[i,], varsrs$S2p),
                               varsrs = data.table(period_agg[i,], varsrs$varsrs),
                               S2_y_ca = data.table(period_agg[i,], S2_y_ca))
                        })
           S2_y_HT <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_HT <- rbindlist(lapply(lin1, function(x) x[[2]]))
           S2_y_ca <- rbindlist(lapply(lin1, function(x) x[[3]]))
      }
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_HT <- transpos(S2_y_HT, is.null(period), "S2_y_HT", names(period))
  all_result <- merge(all_result, S2_y_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_ca <- transpos(S2_y_ca, is.null(period), "S2_y_ca", names(period))
  all_result <- merge(all_result, S2_y_ca, all = TRUE, by = c(names(period), "variable"))

  Y3 <- w_design2 <- var_srs_HT <- S2_y_HT <- S2_y_ca <- NULL

  # Variance of calibrated estimator under SRS
   if (is.null(period)) {
           varsres <- var_srs(Y = Y4, w = w_final2)
           S2_res <- varsres$S2p
           var_srs_ca <- varsres$varsrs
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y4)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          varsres <- var_srs(Y = Y4[ind], w = w_final2[ind])
                          list(S2p = data.table(period_agg[i,], varsres$S2p),
                               varsrs = data.table(period_agg[i,], varsres$varsrs))
                        })
           S2_res <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_ca <- rbindlist(lapply(lin1, function(x) x[[2]]))
        }
  var_srs_ca <- transpos(var_srs_ca, is.null(period), "var_srs_ca", names(period), "variable")
  all_result <- merge(all_result, var_srs_ca, all = TRUE, by = c(names(period), "variable"))
  S2_res <- transpos(S2_res, is.null(period), "S2_res", names(period), "variable")
  all_result <- merge(all_result, S2_res, all = TRUE, by = c(names(period), "variable"))
  var_srs_ca <- S2_res <- Y4 <- w_final2 <- NULL

  all_result[, variable := NULL]
  deff_sam <- deff_est <- deff <- n_eff <- var_est2 <- NULL
  se <- rse <- cv <- absolute_margin_of_error <- NULL
  relative_margin_of_error <- CI_lower <- CI_upper <- NULL

  if (nrow(all_result[var_est < 0]) > 0) stop("Estimation of variance are negative!")

  # Design effect of sample design
  all_result[, deff_sam := var_cur_HT / var_srs_HT]

  # Design effect of estimator
  all_result[, deff_est := var_est / var_cur_HT]

  # Overall effect of sample design and estimator
  all_result[, deff := deff_sam * deff_est]

  all_result[, var_est2 := var_est]
  all_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := 0]
  all_result[, se := sqrt(var_est2)]
  all_result[xor(is.na(var_est2), var_est2 < 0), se := NA]
  all_result[(value != 0) & (!is.nan(value)), rse := se / value]
  all_result[value == 0 | is.nan(value), rse := NA]
  all_result[, cv := rse * 100]

  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, absolute_margin_of_error := tsad * se]
  all_result[, relative_margin_of_error := tsad * cv]
  all_result[, CI_lower := value - absolute_margin_of_error]
  all_result[, CI_upper := value + absolute_margin_of_error]

  setnames(all_result, "var_est", "var")

  if (!is.null(c(nDom, period))) { all_result <- merge(all_result, nhs,
                                                       all = TRUE, by = c(nDom, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]
                                  all_result[, n_nonzero := nhs$n_nonzero]}

  variabl <- c("respondent_count", "n_nonzero", "pop_size",
               "value", "value_eu", "var", "se", "rse", "cv",
               "absolute_margin_of_error", "relative_margin_of_error",
               "CI_lower", "CI_upper")

  if (is.null(nDom))  variabl <- c(variabl, "S2_y_HT", "S2_y_ca", "S2_res")
  variabl <- c(variabl, "var_srs_HT",  "var_cur_HT", "var_srs_ca",
               "deff_sam", "deff_est", "deff")

  type <- "type"
  if (!is.null(period)) type <- c(type, names(period))
  setkeyv(all_result, c(type, nDom))
  list(lin_out = lin_outp,
       res_out = res_outp,
       betas = betas,
       all_result = all_result[, c(type, nDom, variabl), with = FALSE])
}
