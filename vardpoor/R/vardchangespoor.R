#' Variance estimation for measures of change for sample surveys for indicators on social exclusion and poverty
#'
#' @description Computes the variance estimation for measures of change for indicators on social exclusion and poverty.
#'
#' @param Y Study variable (for example equalized disposable income or gross pension income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param age Age variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param pl085 Retirement variable (Number of months spent in retirement or early retirement). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param  month_at_work Variable for total number of month at work (sum of the number of months spent at full-time work as employee, number of months spent at part-time work as employee, number of months spent at full-time work as self-employed (including family worker), number of months spent at part-time work as self-employed (including family worker)).  One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y_den Denominator variable (for example gross individual earnings). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param  Y_thres Variable (for example equalized disposable income) used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. Variable specified for \code{inc} is used as \code{income_thres} if \code{income_thres} is  not defined.
#' @param wght_thres Weight variable used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. Variable specified for \code{weight} is used as \code{wght_thres} if \code{wght_thres} is not defined.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
#' @param ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level2 Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param country Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param period Variable for the all survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param period1 The vector from variable \code{period} describes the first period.
#' @param period2 The vector from variable \code{period} describes the second period.
#' @param gender Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param dataset Optional survey data object convertible to \code{data.frame}.
#' @param X Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param countryX Optional variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param periodX Optional variable of the survey periods and countries. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param X_ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ind_gr Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param g Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param datasetX Optional survey data object in household level convertible to \code{data.table}.
#' @param percentage A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
#'       \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'     For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.
#' @param order_quant A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
#'      \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param alpha a numeric value in range \eqn{[0,100]} for the order of the income quantile share ratio (in percentage).
#' @param use.estVar Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the estimation of covariance matrix of the residuals.
#' @param confidence optional; either a positive value for confidence interval. This variable by default is 0.95.
#' @param outp_lin Logical value. If \code{TRUE} linearized values of the ratio estimator will be printed out.
#' @param outp_res Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.
#' @param type a character vector (of length one unless several.ok is TRUE), example "linarpr","linarpt", "lingpg", "linpoormed", "linrmpg", "lingini", "lingini2", "linqsr", "linarr", "linrmir", "all_choices".
#' @param change_type character value net changes type - absolute or relative.
#' 
#' @return A list with objects are returned by the function:
#'  \itemize{    
#'      \item \code{cros_lin_out} - a \code{data.table} containing the linearized values of the ratio estimator with ID_level2 and PSU by periods and countries (if available).
#'      \item \code{cros_res_out} - a \code{data.table} containing the estimated residuals of calibration with ID_level1 and PSU by periods and countries (if available).
#'      \item \code{crossectional_results} - a \code{data.table} containing: \cr
#'         \code{period} -  survey periods, \cr
#'         \code{country} - survey countries, \cr
#'         \code{Dom} - optional variable of the population domains, \cr
#'         \code{type} - type variable, \cr
#'         \code{count_respondents} - the count of respondents, \cr
#'         \code{pop_size} - the population size (in numbers of individuals), \cr
#'         \code{estim} - the estimated value, \cr
#'         \code{se} - the estimated standard error, \cr
#'         \code{var} - the estimated variance, \cr
#'         \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'         \code{cv} - the estimated relative standard error (coefficient of variation) in percentage.
#'      \item \code{changes_results} - a \code{data.table} containing: \cr
#'         \code{period} -  survey periods, \cr
#'         \code{country} - survey countries, \cr
#'         \code{Dom} - optional variable of the population domains, \cr
#'         \code{type} - type variable, \cr
#'         \code{estim_1} - the estimated value for period1, \cr
#'         \code{estim_2} - the estimated value for period2, \cr
#'         \code{estim} - the estimated value, \cr
#'         \code{se} - the estimated standard error, \cr
#'         \code{var} - the estimated variance, \cr
#'         \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'         \code{cv} - the estimated relative standard error (coefficient of variation) in percentage.}
#'         
#' @references 
#' Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{https://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr
#' Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
#' Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
#' 
#' @examples
#'  
#' ### Example 
#' library("laeken")  
#' library("data.table")
#' data(eusilc)
#' set.seed(1)
#' dataset1 <- data.table(rbind(eusilc, eusilc),
#'                        year = c(rep(2010, nrow(eusilc)),
#'                                 rep(2011, nrow(eusilc))),
#'                        country = c(rep("AT", nrow(eusilc)),
#'                                    rep("AT", nrow(eusilc))))
#' dataset1[age < 0, age := 0]
#' PSU <- dataset1[, .N, keyby = "db030"][, N := NULL]
#' PSU[, PSU := trunc(runif(nrow(PSU), 0, 100))]
#' PSU$inc <- runif(nrow(PSU), 20, 100000)
#' dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
#' PSU <- eusilc <- NULL
#' dataset1[, strata := c("XXXX")]
#' dataset1$pl085 <- 12 * trunc(runif(nrow(dataset1), 0, 2))
#' dataset1$month_at_work <- 12 * trunc(runif(nrow(dataset1), 0, 2))
#' dataset1[, id_l2 := paste0("V", .I)]
#' result <- vardchangespoor(Y = "inc", age = "age",
#'                           pl085 = "pl085", month_at_work = "month_at_work",
#'                           Y_den = "inc", Y_thres = "inc",
#'                           wght_thres = "rb050",  H = "strata", 
#'                           PSU = "PSU", w_final="rb050",
#'                           ID_level1 = "db030",  ID_level2 = "id_l2",
#'                           Dom = c("rb090"), country = "country",
#'                           period = "year", sort = NULL,  
#'                           period1 = c(2010, 2011),
#'                           period2 = c(2011, 2010),
#'                           gender = NULL, dataset = dataset1,
#'                           percentage = 60, order_quant = 50L,
#'                           alpha = 20, confidence = 0.95,
#'                           type = "linrmpg")
#' result
#'
#'
#' @seealso \code{\link{domain}},
#'          \code{\link{vardchanges}},
#'          \code{\link{vardcros}},
#'          \code{\link{vardcrospoor}}
#'          
#' @keywords vardchanges
#' 
#' 
#' @import data.table
#' @import laeken
#' 
#' @export vardchangespoor
    
vardchangespoor <- function(Y, age = NULL,
                            pl085 = NULL,
                            month_at_work = NULL,
                            Y_den = NULL,
                            Y_thres = NULL,
                            wght_thres = NULL, 
                            H, PSU, w_final,
                            ID_level1, ID_level2, 
                            Dom = NULL, country = NULL,
                            period, sort = NULL,
                            period1, period2,
                            gender = NULL, dataset = NULL,
                            X = NULL, countryX = NULL,
                            periodX = NULL, X_ID_level1 = NULL,
                            ind_gr = NULL, g = NULL, q = NULL,
                            datasetX = NULL, percentage = 60,
                            order_quant = 50, alpha = 20,
                            use.estVar = FALSE,
                            confidence = 0.95,
                            outp_lin = FALSE,
                            outp_res = FALSE,
                            type = "linrmpg",
                            change_type = "absolute") {
 
  ### Checking
  . <- NULL
  change_type <- check_var(vars = change_type, varn = "change_type", varntype = "change_type") 

  all_choices <- c("linarpr", "linarpt", "lingpg",
                   "linpoormed", "linrmpg", "lingini",
                   "lingini2", "linqsr", "linrmir", "linarr")
  type <- tolower(type)
  type <- match.arg(type, all_choices, length(type) > 1) 

  percentage <- check_var(vars = percentage, varn = "percentage", varntype = "numeric0100") 
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "numeric0100") 
  alpha <- check_var(vars = alpha, varn = "alpha", varntype = "numeric0100") 
  use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical") 
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 

  if(!is.null(X)) {
         if (is.null(datasetX)) datasetX <- copy(dataset)
         equal_dataset <- identical(dataset, datasetX) & !is.null(dataset)
         if (equal_dataset) { X_ID_level1 <- ID_level1
                              countryX <- country }}

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
                     mustbedefined = any(type == "linarr"))
  
  Y_thres <- check_var(vars = Y_thres, varn = "Y_thres",
                       dataset = dataset, ncols = 1,
                       Ynrow = Ynrow, mustbedefined = FALSE,
                       isnumeric = TRUE, isvector = TRUE)
  
  wght_thres <- check_var(vars = wght_thres, varn = "wght_thres",
                          dataset = dataset, ncols = 1,
                          Ynrow = Ynrow, mustbedefined = FALSE,
                          isnumeric = TRUE, isvector = TRUE)
  
  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 ischaracter = TRUE, 
                 dif_name = c("type", "nameYs", "dataH_stratas"))
  
  sort <- check_var(vars = sort, varn = "sort",
                    dataset = dataset, ncols = 1,
                    Ynrow = Ynrow, mustbedefined = FALSE,
                    isnumeric = TRUE, isvector = TRUE)
  
  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   dif_name = c("type", "nameYs"),
                   grepls = "__")
  
  country <- check_var(vars = country, varn = "country",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       ischaracter = TRUE, mustbedefined = FALSE,
                       dif_name = c("percoun", "period_country",
                                    "type", "nameYs"))
  
  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, duplicatednames = TRUE,
                      dif_name = c("percoun", "period_country", 
                                   "type", "nameYs", names(country)))
  
  period1 <- check_var(vars = period1, varn = "period1", dataset = NULL,
                       ncols = 1, ischaracter = TRUE, periods = period)
  
  period2 <- check_var(vars = period2, varn = "period2", dataset = NULL,
                       ncols = 1, ischaracter = TRUE, periods = period)
  
  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Yncol = 0,
                         Ynrow = Ynrow, ischaracter = TRUE)
  
  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                          dataset = dataset, ncols = 1, Yncol = 0,
                          Ynrow = Ynrow, ischaracter = TRUE,
                          namesID1 = names(ID_level1), country = country,
                          periods = period)
  
  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, namesID1 = names(ID_level1))
  
  if(!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) | !is.null(countryX) |
      !is.null(periodX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
         X <- check_var(vars = X, varn = "X", dataset = datasetX,
                        check.names = TRUE, isnumeric = TRUE,
                        dif_name = c(names(period), names(country), names(H),
                                     names(PSU), names(ID_level1), "w_final",
                                     "w_design", "g", "q", "type", "nameYs"),
                        dX = "X")
         Xnrow <- nrow(X)
    
         ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                             dataset = datasetX, ncols = 1,
                             Xnrow = Xnrow, ischaracter = TRUE, 
                             dif_name = c(names(period), names(country), names(H),
                                          names(PSU), names(ID_level1), "w_final",
                                          "w_design", "g", "q", "type", "nameYs"),
                             dX = "X")
    
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
    
         periodX <- check_var(vars = periodX, varn = "periodX",
                              dataset = datasetX, ncols = 1, 
                              Xnrow = Xnrow, ischaracter = TRUE,
                              mustbedefined = !is.null(period),
                              duplicatednames = TRUE, varnout = "period",
                              varname = names(period), country = country,
                              countryX = countryX, periods = period, dX = "X")
    
         X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                  dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                  ischaracter = TRUE, varnout = "ID_level1",
                                  varname = names(ID_level1), country = country,
                                  countryX = countryX, periods = period,
                                  periodsX = periodX, ID_level1 = ID_level1, dX = "X")
     } 

  if (is.null(Y_thres)) Y_thres <- Y
  if (is.null(wght_thres)) wght_thres <- w_final

  cros_calc <- vardcrospoor(Y = Y, age = age, pl085 = pl085,
                            month_at_work = month_at_work,
                            Y_den = Y_den, Y_thres = Y_thres,
                            H = H, PSU = PSU, w_final = w_final,
                            ID_level1 = ID_level1, ID_level2 = ID_level2,
                            Dom = Dom, country = country,
                            period = period, sort = sort, 
                            gender = gender, dataset = NULL,
                            X = X, countryX = countryX,
                            periodX = periodX, X_ID_level1 = X_ID_level1,
                            ind_gr = ind_gr, g = g, q = q,
                            datasetX = NULL, percentage = percentage,
                            order_quant = order_quant,
                            alpha = alpha,
                            use.estVar = use.estVar,
                            withperiod = TRUE,
                            netchanges = TRUE,
                            confidence = confidence,
                            outp_lin = outp_lin,
                            outp_res = outp_res, 
                            type = type, checking = FALSE)

  cros_lin_out <- cros_calc$lin_out
  cros_res_out <- cros_calc$res_out
  data_res <- cros_calc$res_out
  data <- cros_calc$data_net_changes
  crossectional_results <- copy(cros_calc$results)
  ID_level1 <- ID_level2 <- percoun <- cros_calc <- NULL
  
  sar <- c(names(period), names(country), names(Dom), "percoun", "type", "estim", "var")
  sar <- sar[sar %in% names(crossectional_results)]
  cros_var_grad <- crossectional_results[, sar, with = FALSE]
  setnames(cros_var_grad, "var", "num1")
  value <- nameYs <- NULL

  var_grad0 <- melt(data, id = c(names(period), names(country)), measure.vars = c(names(data)[grepl("lin", names(data))]))
  var_grad0 <- var_grad0[, .(valueY1 = sum(value)), keyby = c(names(period), names(country), "variable")]
  setnames(var_grad0, "variable", "nameYs")
        
  if (!is.null(Dom)) {
          cros_var_grad[, nameYs := namesD(cros_var_grad[, "type"], cros_var_grad[, names(Dom), with = FALSE], uniqueD = FALSE)]
          cros_var_grad[, nameYs := paste0("lin_", tolower(type), "__", substr(nameYs, 7, nchar(nameYs)))]
   } else  cros_var_grad[, nameYs := paste0("lin_", tolower(type))]
 
  cros_var_grad <- merge(cros_var_grad, var_grad0, all = TRUE, by = c(names(period), names(country), "nameYs"))
  cros_var_grad[, nameYs := NULL]
  var_grad0 <- NULL

  changes_calc <- vardchanges_calculation(Y1 = "type", Z1 = NULL, Dom = names(Dom),
                                          names_country = names(country), per = names(period),
                                          PSU = names(PSU), H = names(H), period1 = period1,
                                          period2 = period2, cros_var_grad = cros_var_grad,
                                          change_type = change_type, data = data, linratio = FALSE, 
                                          annual = FALSE, percentratio = 1,
                                          use.estVar = use.estVar, confidence = confidence,
                                          poor = TRUE)

  Y1 <- Z1 <- Dom <- period <- PSU <- H <- period1 <- period2 <- NULL
  
  crossectional_results <- cros_calc$results
  if (is.null(names(country))) crossectional_results[, percoun := NULL]

  list(lin_out <- cros_calc$lin_out,
       res_out = cros_calc$res_out,
       crossectional_results = crossectional_results,
       crossectional_var_grad = changes_calc$cros_var_grad,
       grad_var = changes_calc$grad_var,
       rho = changes_calc$rho_matrix,
       var_tau = changes_calc$var_tau,
       changes_results = changes_calc$changes_results)
 }   


