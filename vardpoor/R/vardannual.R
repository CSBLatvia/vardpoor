#' Variance estimation for measures of annual net change or annual for single and multistage stage cluster sampling designs
#'
#' @description Computes the variance estimation for measures of annual net change or annual for single and multistage stage cluster sampling designs.
#'
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level2}{Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param Z Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.table} or variable names as character, column numbers. This variable is \code{NULL} by default.
#' @param gender Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param country Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param years Variable for the all survey years. The values for each year are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param subperiods Variable for the all survey sub-periods. The values for each sub-period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset}{Optional survey data object convertible to \code{data.table}.
#' @param year1 The vector of years from variable \code{years} describes the first year for measures of annual net change.
#' @param year2 The vector of years from variable \code{periods} describes the second year for measures of annual net change.
#' @param X Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param countryX Optional variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param yearsX Variable of the all survey years. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param subperiodsX Variable for the all survey sub-periods. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param X_ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ind_gr Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param g Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param datasetX Optional survey data object in household level convertible to \code{data.table}.
#' @param frate Positive numeric value. Sampling rate in percentage, by default - 0.
#' @param percentratio Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.
#' @param use.estVar Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the estimation of covariance matrix of the residuals.
#' @param use.gender Logical value. If value is \code{TRUE}, then \code{subperiods} is defined together with \code{gender}.
#' @param confidence optional; either a positive value for confidence interval. This variable by default is 0.95.
#' @param method character value; value 'cros' is for measures of annual or value 'netchanges' is for measures of annual net change. This variable by default is netchanges.
#'
#' @return A list with objects are returned by the function:
#' \itemize{
#'    \item \code{crossectional_results} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years,
#'      \item \code{subperiods} - survey sub-periods,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{sample_size} - the sample size (in numbers of individuals),
#'      \item \code{pop_size} - the population size (in numbers of individuals),
#'      \item \code{total} - the estimated totals,
#'      \item \code{variance} - the estimated variance of cross-sectional or longitudinal measures,
#'      \item \code{sd_w} - the estimated weighted variance of simple random sample,
#'      \item \code{sd_nw} - the estimated variance estimation of simple random sample,
#'      \item \code{pop} - the population size (in numbers of households),
#'      \item \code{sampl_siz} - the sample size (in numbers of households),
#'      \item \code{stderr_w} - the estimated weighted standard error of simple random sample,
#'      \item \code{stderr_nw} - the estimated standard error of simple random sample,
#'      \item \code{se} - the estimated standard error of cross-sectional or longitudinal,
#'      \item \code{rse} - the estimated relative standard error (coefficient of variation),
#'      \item \code{cv} - the estimated relative standard error (coefficient of variation) in percentage,
#'      \item \code{absolute_margin_of_error} - the estimated absolute margin of error,
#'      \item \code{relative_margin_of_error} - the estimated relative margin of error,
#'      \item \code{CI_lower} - the estimated confidence interval lower bound,
#'      \item \code{CI_upper} - the estimated confidence interval upper bound, 
#'      \item \code{confidence_level} - the positive value for confidence interval.
#'    }
#'    \item \code{crossectional_var_grad} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years,
#'      \item \code{subperiods} - survey sub-periods,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{grad} - the estimated gradient,
#'      \item \code{var} - the estimated a design-based variance.
#'    }
#'    \item \code{vardchanges_grad_var} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year_1} - survey years of \code{years1},
#'      \item \code{subperiods_1} - survey sub-periods of \code{years1},
#'      \item \code{year_2} - survey years of \code{years2},
#'      \item \code{subperiods_2} - survey sub-periods of \code{years2},
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{nams} - gradient names, numerator (num) and denominator (den), for each year, 
#'      \item \code{grad} - the estimated gradient,
#'      \item \code{cros_var} - the estimated a design-based variance.
#'    }
#'    \item \code{vardchanges_rho} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years of \code{years} for cross-sectional estimates,
#'      \item \code{subperiods} - survey sub-periods of \code{years} for cross-sectional estimates,
#'      \item \code{year_1} - survey years of \code{years1},
#'      \item \code{subperiods_1} - survey sub-periods of \code{years1},
#'      \item \code{year_2} - survey years of \code{years2},
#'      \item \code{subperiods_2} - survey sub-periods of \code{years2},
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{nams} - gradient names, numerator (num) and denominator (den), for each year,
#'      \item \code{rho} - the estimated correlation matrix.
#'    }
#'    \item \code{vardchanges_var_tau} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year_1} - survey years of \code{years1},
#'      \item \code{subperiods_1} - survey sub-periods of \code{years1},
#'      \item \code{year_2} - survey years of \code{years2},
#'      \item \code{subperiods_2} - survey sub-periods of \code{years2},
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{nams} - gradient names, numerator (num) and denominator (den), for each year,
#'      \item \code{var_tau} - the estimated covariance matrix.
#'    }
#'    \item \code{vardchanges_results} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years of \code{years} for measures of annual,
#'      \item \code{subperiods} - survey sub-periods of \code{years} for measures of annual,
#'      \item \code{year_1} - survey years of \code{years1} for measures of annual net change,
#'      \item \code{subperiods_1} - survey sub-periods of \code{years1} for measures of annual net change,
#'      \item \code{year_2} - survey years of \code{years2} for measures of annual net change,
#'      \item \code{subperiods_2} - survey sub-periods of \code{years2} for measures of annual net change,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{estim_1} - the estimated value for period1,
#'      \item \code{estim_2} - the estimated value for period2,
#'      \item \code{estim} - the estimated value,
#'      \item \code{var} - the estimated variance,
#'      \item \code{se} - the estimated standard error,
#'      \item \code{CI_lower} - the estimated confidence interval lower bound,
#'      \item \code{CI_upper} - the estimated confidence interval upper bound,
#'      \item \code{confidence_level} - the positive value for confidence interval, 
#'      \item \code{significant} - is the the difference significant
#'    }
#'    \item \code{X_annual} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years of \code{years} for measures of annual,
#'      \item \code{year_1} - survey years of \code{years1} for measures of annual net change,
#'      \item \code{year_2} - survey years of \code{years2} for measures of annual net change,
#'      \item \code{period} - period1 and period2 together,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{cros_se} - the estimated cross-sectional standard error.
#'    }
#'    \item \code{A_matrix} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years of \code{years1} for measures of annual,
#'      \item \code{year_1} - survey years of \code{years1} for measures of annual net change,
#'      \item \code{year_2} - survey years of \code{years2} for measures of annual net change,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{cols} - the estimated matrix_A columns,
#'      \item \code{matrix_A} - the estimated matrix A.
#'    }
#'    \item \code{annual_sum} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{totalY} - the estimated value of variables of interest for period1,
#'      \item \code{totalZ} - optional the estimated value of denominator for period2,
#'      \item \code{estim} - the estimated value for year.
#'    }
#'    \item \code{annual_results} - a \code{data.table} containing:
#'    \itemize{
#'      \item \code{year} - survey years of \code{years} for measures of annual,
#'      \item \code{year_1} - survey years of \code{years1} for measures of annual net change,
#'      \item \code{year_2} - survey years of \code{years2} for measures of annual net change,
#'      \item \code{country} - survey countries,
#'      \item \code{Dom} - optional variable of the population domains,
#'      \item \code{namesY} - variable with names of variables of interest,
#'      \item \code{namesZ} - optional variable with names of denominator for ratio estimation,
#'      \item \code{estim_1} - the estimated value for period1 for measures of annual net change,
#'      \item \code{estim_2} - the estimated value for period2 for measures of annual net change,
#'      \item \code{estim} - the estimated value,
#'      \item \code{var} - the estimated variance,
#'      \item \code{se} - the estimated standard error,
#'      \item \code{rse} - the estimated relative standard error (coefficient of variation),
#'      \item \code{cv} - the estimated relative standard error (coefficient of variation) in percentage,
#'      \item \code{absolute_margin_of_error} - the estimated absolute margin of error for period1 for measures of annual,
#'      \item \code{relative_margin_of_error} - the estimated relative margin of error in percentage for measures of annual,
#'      \item \code{CI_lower} - the estimated confidence interval lower bound,
#'      \item \code{CI_upper} - the estimated confidence interval upper bound,
#'      \item \code{confidence_level} - the positive value for confidence interval, 
#'      \item \code{significant} - is the the difference significant
#'    }
#' }
#'
#'
#' @references
#' \itemize{
#'   \item Guillaume Osier, Virginie Raymond, (2015), Development of methodology for the estimate of variance of annual net changes for LFS-based indicators. Deliverable 1 - Short document with derivation of the methodology.
#'   \item Guillaume Osier, Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{https://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}.
#'   \item Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}.
#'   \item Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en}.
#' }
#'
#' @seealso \code{\link{domain}},
#'          \code{\link{vardcros}},
#'          \code{\link{vardchanges}}
#' 
#' @keywords vardannual
#' 
#' @examples
#' 
#' ### Example
#' library("data.table")
#' data("eusilc", package = "laeken")
#' 
#' set.seed(1)
#' eusilc1 <- eusilc[1:20, ]
#' 
#' dataset1 <- data.table(rbind(eusilc1, eusilc1),
#'                        year = c(rep(2010, nrow(eusilc1)),
#'                                 rep(2011, nrow(eusilc1))))
#' 
#' dataset1[, country := "AT"]
#' dataset1[, half := .I - 2 * trunc((.I - 1) / 2)]
#' dataset1[, quarter := .I - 4 * trunc((.I - 1) / 4)]
#' dataset1[age < 0, age := 0]
#' 
#' PSU <- dataset1[, .N, keyby = "db030"][, N := NULL]
#' PSU[, PSU := trunc(runif(.N, 0, 5))]
#' 
#' dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
#' 
#' dataset1[, strata := "XXXX"]
#' dataset1[, employed := trunc(runif(.N, 0, 2))]
#' dataset1[, unemployed := trunc(runif(.N, 0, 2))]
#' dataset1[, labour_force := employed + unemployed]
#' dataset1[, id_lv2 := paste0("V", .I)]
#' 
#' vardannual(Y = "employed", H = "strata",
#'            PSU = "PSU", w_final = "rb050",
#'            ID_level1 = "db030", ID_level2 = "id_lv2",
#'            Dom = NULL, Z = NULL, years = "year",
#'            subperiods = "half", dataset = dataset1,
#'            percentratio = 100, confidence = 0.95,
#'            method = "cros")
#'   
#' vardannual(Y = "employed", H = "strata",
#'            PSU = "PSU", w_final = "rb050",
#'            ID_level1 = "db030", ID_level2 = "id_lv2",
#'            Dom = NULL, Z = NULL, country = "country",
#'            years = "year", subperiods = "quarter",
#'            dataset = dataset1, year1 = 2010, year2 = 2011,
#'            percentratio = 100, confidence = 0.95,
#'            method = "netchanges")
#'     
#' vardannual(Y = "unemployed", H = "strata",
#'            PSU = "PSU", w_final = "rb050",
#'            ID_level1 = "db030", ID_level2 = "id_lv2", 
#'            Dom = NULL, Z = "labour_force",
#'            country = "country", years = "year",
#'            subperiods = "quarter", dataset = dataset1,
#'            year1 = 2010, year2 = 2011,
#'            percentratio = 100, confidence = 0.95,
#'            method = "netchanges")
#' 
#' @import data.table
#' @import MASS
#' @import laeken
#' 
#' @export vardannual


# # Development ####
# # For production this section should be disabled
# 
# # Load helper functions
# source(file = "vardpoor/R/domain.R", echo = FALSE)
# source(file = "vardpoor/R/vardcros.R", echo = FALSE)
# source(file = "vardpoor/R/vardchanges.R", echo = FALSE)
# 
# # Generate example data
# library("data.table")
# 
# set.seed(1)
# 
# data("eusilc", package = "laeken")
# eusilc1 <- eusilc[1:20, ]
# rm(eusilc)
# 
# dataset1 <- data.table(rbind(eusilc1, eusilc1),
#                        year = c(rep(2010, nrow(eusilc1)),
#                                 rep(2011, nrow(eusilc1))))
# rm(eusilc1)
# 
# dataset1[, country := "AT"]
# dataset1[, half := .I - 2 * trunc((.I - 1) / 2)]
# dataset1[, quarter := .I - 4 * trunc((.I - 1) / 4)]
# dataset1[age < 0, age := 0]
# 
# PSU <- dataset1[, .N, keyby = "db030"][, N := NULL][]
# PSU[, PSU := trunc(runif(.N, 0, 5))]
# 
# dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
# rm(PSU)
# 
# dataset1[, strata := "XXXX"]
# dataset1[, employed := trunc(runif(.N, 0, 2))]
# dataset1[, unemployed := trunc(runif(.N, 0, 2))]
# dataset1[, labour_force := employed + unemployed]
# dataset1[, id_lv2 := paste0("V", .I)]
# 
# 
# # Define function arguments for testing
# Y <- "employed"
# H <- "strata"
# PSU <- "PSU"
# w_final <- "rb050"
# ID_level1 <- "db030"
# ID_level2 <- "id_lv2"
# Dom <- NULL
# Z <- NULL
# gender <- NULL
# country <- NULL
# years <- "year"
# subperiods <- "half"
# dataset <- copy(dataset1)
# year1 <- NULL
# year2 <- NULL
# X <- NULL
# countryX <- NULL
# yearsX <- NULL
# subperiodsX <- NULL
# X_ID_level1 <- NULL
# ind_gr <- NULL
# g <- NULL
# q <- NULL
# datasetX <- NULL
# frate <- 0
# percentratio <- 100
# use.estVar <- FALSE
# use.gender <- FALSE
# confidence <- 0.95
# method <- "cros"


# Function definition ####
vardannual <- function(Y, H, PSU, w_final, ID_level1,
                       ID_level2, Dom = NULL, Z = NULL,
                       gender = NULL, country = NULL,
                       years, subperiods, dataset = NULL,
                       year1 = NULL, year2 = NULL, X = NULL,
                       countryX = NULL, yearsX = NULL,
                       subperiodsX = NULL, X_ID_level1 = NULL,
                       ind_gr = NULL, g = NULL, q = NULL,
                       datasetX = NULL, frate = 0, percentratio = 1,
                       use.estVar = FALSE, use.gender = FALSE,
                       confidence = 0.95, method = "cros") {
  
  ### Checking
  . <- NULL
  outp_res <- FALSE
  method <- check_var(vars = method, varn = "method", varntype = "method")
  percentratio <- check_var(vars = percentratio, varn = "percentratio",
                            varntype = "pinteger")
  use.estVar <- check_var(vars = use.estVar, varn = "use.estVar",
                          varntype = "logical")
  use.gender <- check_var(vars = use.gender, varn = "use.gender",
                          varntype = "logical")
  confidence <- check_var(vars = confidence, varn = "confidence",
                          varntype = "numeric01")
  frate <- check_var(vars = frate, varn = "frate", varntype = "numeric0100")
  
  if (!is.null(X)) {
    if (is.null(datasetX)) datasetX <- copy(dataset)
    equal_dataset <- identical(dataset, datasetX) & !is.null(dataset)
    if (equal_dataset) {
      X_ID_level1 <- ID_level1
      countryX <- country
    }
  }
  
  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)
  
  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                 dif_name = "dataH_stratas")
  
  w_final <- check_var(vars = w_final, varn = "w_final",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       isnumeric = TRUE, isvector = TRUE)
  
  gender <- check_var(vars = gender, varn = "gender",
                      dataset = dataset, ncols = 1, Ynrow = Ynrow,
                      isnumeric = TRUE, isvector = TRUE,
                      mustbedefined = FALSE)
  
  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustbedefined = FALSE)
  
  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   ncols = 0, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, mustbedefined = FALSE,
                   duplicatednames = TRUE, grepls = "__")
  
  country <- check_var(vars = country, varn = "country",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       ischaracter = TRUE, mustbedefined = FALSE,
                       dif_name = c("percoun", "period_country", "Nrs"))
  
  years <- check_var(vars = years, varn = "years", dataset = dataset,
                     ncols = 1, Yncol = 0, Ynrow = Ynrow, ischaracter = TRUE,
                     dif_name = c("percoun", "period_country", names(country),
                                  "yearg", "Nrs"),
                     use.gender = use.gender)
  yearg <- NULL
  years[, yearg := substr(get(names(years)), 1,
                          nchar(get(names(years))) - ifelse(use.gender, 2, 0))]
  yearm <- names(years)[1 + use.gender]
  
  if (method != "cros") {
    year1 <- check_var(vars = year1, varn = "year1", dataset = NULL, ncols = 1,
                       ischaracter = TRUE,
                       years = years[, 1 + use.gender, with = FALSE])
    year2 <- check_var(vars = year2, varn = "year2", dataset = NULL, ncols = 1, 
                       ischaracter = TRUE,
                       years = years[, 1 + use.gender, with = FALSE])
  } else {
    if (!missing(year1)) if (!is.null(year1)) stop("'year1' must be NULL")
    if (!missing(year2)) if (!is.null(year2)) stop("'year2' must be NULL")
    year1 <- years[, .N, by = yearm][, N := NULL]
    year2 <- years[, .N, by = yearm][, N := NULL]
  }
  
  subperiods <- check_var(vars = subperiods, varn = "subperiods",
                          dataset = dataset, ncols = 1, Ynrow = Ynrow,
                          ischaracter = TRUE, years = years, Domen = Dom,
                          dif_name = c("percoun", "period_country",
                                       names(country), "yearg", "Nrs"))
  subpm <- names(subperiods)
  
  subn <- data.table(years, subperiods, Dom)
  subn <- subn[, .N, by = c(names(subn))]
  subn <- max(subn[, .N, by = names(years)][["N"]])
  
  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE)
  
  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE, namesID1 = names(ID_level1),
                         country = country, years = years, periods = subperiods)
  
  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   namesID1 = names(ID_level1))
  
  if (!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) |
      !is.null(countryX) | !is.null(yearsX) | !is.null(subperiodsX) |
      !is.null(X_ID_level1) | !is.null(datasetX)) {
    X <- check_var(vars = X, varn = "X", dataset = datasetX,
                   isnumeric = TRUE,
                   dif_name = c(names(years), names(subperiods),
                                names(country), names(H), names(PSU),
                                names(ID_level1), "w_final", names(Y),
                                "w_design", "g", "q"), dX = "X")
    Xnrow <- nrow(X)
    
    ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                        dataset = datasetX, ncols = 1,
                        Xnrow = Xnrow, ischaracter = TRUE,
                        dif_name = c(names(years), names(subperiods),
                                     names(country), names(H), names(PSU),
                                     names(ID_level1), "w_final", names(Y),
                                     names(X), "w_design", "g", "q"), dX = "X")
    
    g <- check_var(vars = g, varn = "g", dataset = datasetX,
                   ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                   isvector = TRUE, dX = "X")
    
    q <- check_var(vars = q, varn = "q", dataset = datasetX,
                   ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                   isvector = TRUE, dX = "X")
    
    countryX <- check_var(vars = countryX, varn = "countryX",
                          dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                          ischaracter = TRUE, mustbedefined = !is.null(country),
                          varnout = "country", varname = names(country),
                          country = country, dX = "X")
    
    yearsX <- check_var(vars = yearsX, varn = "yearsX", dataset = datasetX,
                        ncols = 1, Xnrow = Xnrow, ischaracter = TRUE,
                        mustbedefined = !is.null(years), varnout = "years",
                        varname = names(years)[1], country = country,
                        countryX = countryX, years = years[, 1, with = FALSE],
                        use.gender = use.gender, dX = "X")
    
    subperiodsX <- check_var(vars = subperiodsX, varn = "subperiodsX",
                             dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                             ischaracter = TRUE,
                             mustbedefined = !is.null(subperiods),
                             varnout = "subperiods",
                             varname = names(subperiods),
                             country = country, countryX = countryX,
                             years = years[, 1, with = FALSE], dX = "X",
                             yearsX = yearsX, periods = subperiods)
    
    X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                             dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                             ischaracter = TRUE, varnout = "ID_level1",
                             varname = names(ID_level1), country = country,
                             countryX = countryX,
                             years = years[, 1, with = FALSE],
                             yearsX = yearsX, periods = subperiods, dX = "X",
                             periodsX = subperiodsX, ID_level1 = ID_level1)
    
  }
  
  dataset <- datasetX <- NULL
  ids <- nams <- cros_se <- num1 <- totalY <- totalZ <- NULL
  estim_1 <- estim_2 <- avar <- N <- estim <- NULL
  var_est2 <- se <- rse <- cv <- CI_lower <- CI_upper <- NULL
  Nr_sar <- cols <- Nrs <- percoun <- totalY_male <- NULL
  totalZ_male <- totalY_female <- totalZ_female <- confidence_level <- NULL  
  
  pers <- data.table(years, subperiods,
                     pers = paste0(years[[1]], "__", subperiods[[1]]))
  
  # Not necessary as `yearg` is allready available
  # pers[, yearg := substr(get(names(years)[1]), 1,
  #                        nchar(get(names(years)[1])) -
  #                          ifelse(use.gender, 2, 0))]
  
  if (!is.null(X)) {
    persX <- data.table(yearsX, subperiodsX,
                        pers = paste0(yearsX[[names(yearsX)]], "__",
                                      subperiodsX[[names(subperiodsX)]]))
  }
  
  sarak <- pers[, .N, keyby = names(pers)][, N := NULL][]
  
  cros_calc <- vardcros(Y = Y, H = H, PSU = PSU, w_final = w_final,
                        ID_level1 = ID_level1, ID_level2 = ID_level2,
                        Dom = Dom, Z = Z, gender = gender, 
                        country = country, period = pers[, "pers"],
                        dataset = NULL, X = X, countryX = countryX,
                        periodX = persX[, "pers"], X_ID_level1 = X_ID_level1,
                        ind_gr = ind_gr, g = g, q = q, datasetX = NULL,
                        linratio = !is.null(Z),
                        percentratio = percentratio,
                        use.estVar = use.estVar,
                        ID_level1_max = is.null(X),
                        outp_res = outp_res,
                        withperiod = TRUE,
                        netchanges = TRUE,
                        confidence = confidence,
                        checking = FALSE)
  
  countryX <- periodX <- yearX <- NULL
  X_ID_level1 <- ind_gr <- g <- q <- ID_level2 <- NULL
  subperiods <- w_final <- NULL   
  
  years <- years[, .N, by = c(names(years)[1])][, N := NULL][]
  
  pers12 <- paste("pers", 1:2, sep = "_")
  if (method == "cros") {
    period12 <- c(yearm, paste(subpm, 1:2, sep = "_"))
    years12 <- yearm
  } else {
    period12 <- paste(rep(c(yearm, subpm), 2), c(1, 1, 2, 2), sep = "_")
    years12 <- paste(yearm, 1:2, sep = "_")
  }
  
  atsyear <- rbindlist(list(data.table(Nrs = 1:nrow(year1), yrs = 1, year1),
                            data.table(Nrs = 1:nrow(year2), yrs = 2, year2)))
  if (method == "cros") atsyear <- data.table(Nrs = 1:nrow(year1),
                                              yrs = 1, year1)
  
  atsyear <- merge(atsyear, sarak, all.x = TRUE, by = yearm,
                   sort = FALSE, allow.cartesian = TRUE)
  
  atsyear[, ids := 1:.N, by = "Nrs"]
  
  nr1 <- max(atsyear[["ids"]])
  
  yrs <- rbindlist(lapply(1:(nr1 - 1), function(j) {
    atsy1 <- atsyear[ids == j]
    atsy2 <- atsyear[ids %in% c((j + 1):nr1)]
    if (method == "cros") {
      atsy2[, (yearm) := NULL]
      setnames(atsy1, names(atsy1)[-c(1:2)],
               paste0(names(atsy1)[-c(1:2)], "_1"))
      setnames(atsy2, names(atsy2)[-1],
               paste0(names(atsy2)[-1], "_2"))
    } else {
      setnames(atsy1, names(atsy1)[-2], paste0(names(atsy1)[-2], "_1"))
      setnames(atsy2, names(atsy2)[-2], paste0(names(atsy2)[-2], "_2"))
    }
    merge(atsy1, atsy2, all = TRUE, by = "Nrs")
  }))
  
  if (method != "cros") {  
    yr12 <- rbind(data.table(Nrs = 1:nrow(year1), yearg = year1[[1]]),
                  data.table(Nrs = 1:nrow(year1), yearg = year2[[1]]))
  } else yr12 <- data.table(Nrs = 1:nrow(year1), yearg = year1[[1]])
  
  if (!is.null(Dom)) {
    Y1 <- namesD(Y, Dom, uniqueD = TRUE)
    Z1 <- NULL
    if (!is.null(Z)) Z1 <- namesD(Z, Dom, uniqueD = TRUE)
  } else {
    Y1 <- names(Y)
    Z1 <- names(Z)
  }
  
  Y <- names(Y)
  Z <- names(Z)
  names_country <- names(country)
  PSU <- names(PSU)
  H <- names(H)
  Dom <- names(Dom)
  yrs_without <- yrs[, .N, by = c("pers_1", "pers_2")]
  
  data <- cros_calc$data_net_changes
  changes_calc <- vardchanges_calculation(Y1 = Y1, Z1 = Z1, Dom = Dom,
                    names_country = names_country,
                    per = "pers", PSU = PSU, H = H,
                    period1 = yrs_without[, .(pers = get("pers_1"))],
                    period2 = yrs_without[, .(pers = get("pers_2"))],
                    cros_var_grad = cros_calc$var_grad,
                    change_type = "absolute",
                    data = data, linratio = !is.null(Z),
                    annual = TRUE,
                    percentratio = percentratio,
                    use.estVar = use.estVar,
                    confidence = confidence, poor = FALSE)
  
  pers <- pers[, .N, keyby = names(pers)][, N := NULL][]
  crossectional_results <- merge(pers, cros_calc$results,
                                 all = TRUE, by = "pers")
  crossectional_results[, (c("pers", "yearg")) := NULL]
  if (is.null(names(country))) crossectional_results[, percoun := NULL]
  gender <- data <- yrs_without <- cros_calc <- NULL
  
  cros_var_grad <- merge(sarak, changes_calc$cros_var_grad,
                         all.y = TRUE, by = c("pers"))
  
  rho <- merge(yrs, changes_calc$rho_matrix,
               all.y = TRUE,
               by = c("pers_1", "pers_2"),
               allow.cartesian = TRUE)
  
  sar <- c("Nrs", names_country, "namesY", "namesZ", Dom)
  sar <- sar[sar %in% names(rho)]
  rho[, Nr_sar := .GRP, by = sar]
  
  rho1 <- rho[nams == "num2"]
  rho1[, ids := 1:.N, by = sar]
  
  rhoj <- rho[,.N, keyby = sar][, N := NULL][]
  max_ids <- max(atsyear[["ids"]])
  
  yr12cros <- merge(yr12, cros_var_grad, by = "yearg",
                    allow.cartesian = TRUE, sort = FALSE)

  apstr <- lapply(1:max(rho[["Nr_sar"]]), function(j) {
    rho2 <- rho1[Nr_sar == j]
    A_matrix <- diag(1, max_ids, max_ids)
    
    for (k in 1:max(rho2[["ids"]])) {
      at <- rho2[k == ids]
      A_matrix[at[["ids_1"]], at[["ids_2"]]] <- at[["rho_num1"]]
      A_matrix[at[["ids_2"]], at[["ids_1"]]] <- at[["rho_num1"]]
      if (method != "cros") {
        if (at[["ids_2"]] > subn & at[["ids_1"]] < subn + 1) {
          A_matrix[at[["ids_1"]], at[["ids_2"]]] <- -at[["rho_num1"]]
          A_matrix[at[["ids_2"]], at[["ids_1"]]] <- -at[["rho_num1"]]
        }
      }
    }
    
    cros_rho <- merge(yr12cros, rho2[1, sar, with = FALSE], by = sar, sort = FALSE)
    cros_rho[, cols := paste0("V", 1:.N)]
    cros_rho[, cros_se := sqrt(num1)]
    X <- cros_rho[["cros_se"]]
    
    annual_var <- data.table(rho2[1, sar, with = FALSE],
                             (1 - frate / 100) / (subn) ^ 2 *
                               (t(X) %*% A_matrix) %*% X)
    setnames(annual_var, c("V1"), c("var"))
    A_matrix <- data.table(rho2[1, sar, with = FALSE],
                           cols = paste0("V", 1:nrow(A_matrix)), A_matrix)
    list(cros_rho, A_matrix, annual_var)
  })
  
  cros_rho <- rbindlist(lapply(apstr, function(x) x[[1]]))
  A_matrix <- rbindlist(lapply(apstr, function(x) x[[2]]))
  annual_var <- rbindlist(lapply(apstr, function(x) x[[3]]))
  
  sars <- c(names(country), yearm, Dom, "namesY", "namesZ")
  sars <- sars[sars %in% names(cros_var_grad)]
  sarsb <- sars[!(sars %in% yearm)]
  sarc <- c("totalY", "totalZ", "totalY_male", 
            "totalY_female", "totalZ_male", "totalZ_female")
  sarc <- sarc[sarc %in% names(cros_var_grad)]
  
  ysum <- cros_var_grad[, lapply(.SD, mean), by = sars, .SDcols = sarc]
  
  if (!is.null(ysum$totalZ_male)) {
    ysum[, estim := (totalY_male / totalZ_male -
                       totalY_female / totalZ_female) * percentratio]
  } else if (!is.null(ysum$totalY_male)) {
    ysum[, estim := (totalY_male - totalY_female) * percentratio]
  } else if (!is.null(ysum$totalZ)) {
    ysum[, estim := totalY / totalZ * percentratio]
  } else ysum[, estim := totalY]
  
  year1m <- year1[[yearm]]
  ysum1 <- ysum[get(yearm) %in% year1m, c(sars, "estim"), with = FALSE]
  
  years1 <- copy(year1)[, Nrs := 1:.N]
  ysum1 <- merge(years1, ysum1, by = yearm, sort = FALSE,
                 allow.cartesian = TRUE)
  
  if (method != "cros") {
    years2 <- copy(year2)[, Nrs := 1:.N]
    year2m <- year2[[yearm]]
    ysum2 <- ysum[get(yearm) %in% year2m, c(sars, "estim"), with = FALSE]
    ysum2 <- merge(years2, ysum2, by = yearm, sort = FALSE,
                   allow.cartesian = TRUE)
    setnames(ysum1, c("estim", yearm), c("estim_1", paste0(yearm, "_1")))
    setnames(ysum2, c("estim", yearm), c("estim_2", paste0(yearm, "_2")))
    ysum <- merge(ysum1, ysum2, all.x = TRUE, by = c("Nrs", sarsb))   
    ysum[, estim := estim_2 - estim_1] 
  } else ysum <- ysum1
  
  ysum1 <- ysum2 <- NULL
  
  annual_results <- merge(ysum, annual_var, by = c("Nrs", sarsb), sort = FALSE)
  
  estim <- "estim"
  if (method != "cros") estim <- c("estim_1", "estim_2", "estim")
  annual_results <- annual_results[, c(years12, sarsb, estim, "var"),
                                   with = FALSE]
  ysum <- ysum[, c(years12, sarsb, estim), with = FALSE]
  
  grad_var <- merge(yrs[, c(pers12, period12), with = FALSE],
                    changes_calc$grad_var, all.y = TRUE,
                    by = pers12, allow.cartesian = TRUE)
  grad_var[, (pers12) := NULL]
  
  
  var_tau <- merge(yrs[, c(pers12, period12), with = FALSE],
                   changes_calc$var_tau, all.y = TRUE,
                   by = pers12, allow.cartesian = TRUE)
  var_tau[, (pers12) := NULL]
  
  vardchanges_results <- merge(yrs[, c(pers12, period12), with = FALSE],
                               changes_calc$changes_results, all.y = TRUE,
                               by = pers12, allow.cartesian = TRUE)
  vardchanges_results[, (pers12) := NULL]
  
  
  X_annual <- cros_rho
  if(method != "cros") {
    atsyear <- data.table(Nrs = 1:nrow(year1), year1, year2, check.names = TRUE)
    setnames(atsyear, names(atsyear)[2:3], years12) 
    X_annual <- merge(atsyear, cros_rho, all.y = TRUE, by = "Nrs", sort = FALSE)
  } else atsyear <- data.table(Nrs = 1:nrow(years), years, check.names = TRUE)
  
  vars <- c(years12, subpm, sarsb, "cols", "cros_se")
  X_annual <- X_annual[, vars[vars %in% names(X_annual)], with = FALSE]
  
  A_matrix <- merge(atsyear, A_matrix, all.y = TRUE, by = "Nrs", sort = FALSE)
  A_matrix[, Nrs := NULL]
  
  annual_results[, var_est2 := var]
  annual_results[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  annual_results[, se := sqrt(var_est2)]
  annual_results[, var_est2 := NULL]
  
  annual_results[, rse := se / estim]
  annual_results[, cv := rse * 100] 
  
  tsad <- qnorm(0.5 * (1 + confidence))
  absolute_margin_of_error <- relative_margin_of_error <- NULL
  annual_results[, absolute_margin_of_error := tsad * se]
  annual_results[, relative_margin_of_error := tsad * cv]
  
  annual_results[, CI_lower := estim - tsad * se]
  annual_results[, CI_upper := estim + tsad * se]
  annual_results[, confidence_level := confidence]
  
  if (method != "cros") {
    significant <- NULL
    annual_results[, significant := "YES"]
    annual_results[CI_lower <= 0 & CI_upper >= 0, significant := "NO"]
  }
  
  list(crossectional_results = crossectional_results,
       crossectional_var_grad = cros_var_grad,
       vardchanges_grad_var = grad_var,
       vardchanges_rho = rho,
       vardchanges_var_tau = var_tau,
       vardchanges_results = vardchanges_results,
       X_annual = X_annual, A_matrix = A_matrix,
       annual_sum = ysum,
       annual_results = annual_results)
  
}
