#' Variance estimation for cross-sectional, longitudinal measures for indicators on social exclusion and poverty
#' 
#' Computes the variance estimation for cross-sectional and longitudinal measures for indicators on social exclusion and poverty.
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param age Age variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param pl085 Retirement variable (Number of months spent in retirement or early retirement). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param month_at_work Variable for total number of month at work (sum of the number of months spent at full-time work as employee, number of months spent at part-time work as employee, number of months spent at full-time work as self-employed (including family worker), number of months spent at part-time work as self-employed (including family worker)).  One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y_den Denominator variable (for example gross individual earnings). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y_thres Variable (for example equalized disposable income) used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}). Variable specified for \code{inc} is used as \code{income_thres} if \code{income_thres} is  not defined.
#' @param wght_thres Weight variable used for computation and linearization of poverty threshold. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number. Variable specified for \code{weight} is used as \code{wght_thres} if \code{wght_thres} is not defined.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level2 Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param country Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param period Variable for the survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param gender Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param X Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param countryX Optional variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param periodX Optional variable of the survey periods and countries. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param X_ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ind_gr}{Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param g Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param datasetX Optional survey data object in household level convertible to \code{data.table}.
#' @param percentage A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
#'  \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.
#' @param order_quant A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
#'   \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param alpha a numeric value in range \eqn{[0,100]} for the order of the income quantile share ratio (in percentage).
#' @param use.estVar}{Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the estimation of covariance matrix of the residuals.
#' @param withperiod Logical value. If \code{TRUE} is value, the results is with period, if \code{FALSE}, without period.
#' @param netchanges Logical value. If value is TRUE, then produce two objects: the first object is aggregation of weighted data by period (if available), country, strata and PSU, the second object is an estimation for Y, the variance, gradient for numerator and denominator by country and period (if available). If value is FALSE, then both objects containing \code{NULL}.
#' @param confidence Optional positive value for confidence interval. This variable by default is 0.95.
#' @param outp_lin Logical value. If \code{TRUE} linearized values of the ratio estimator will be printed out.
#' @param outp_res Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.
#' @param type a character vector (of length one unless several.ok is TRUE), example "linarpr","linarpt", "lingpg", "linpoormed", "linrmpg", "lingini", "lingini2", "linqsr", "linarr", "linrmir".
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' 
#' @return A list with objects are returned by the function:
#'   \itemize{
#'      \item \code{lin_out} - a \code{data.table} containing the linearized values of the ratio estimator with ID_level2 and PSU.
#'      \item \code{res_out} - a \code{data.table} containing the estimated residuals of calibration with ID_level1 and PSU.
#'      \item \code{data_net_changes} - a \code{data.table} containing aggregation of weighted data by period (if available), country, strata, PSU.
#'      \item \code{results} - a \code{data.table} containing: \cr
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
#'         \code{cv} - the estimated relative standard error (coefficient of variation) in percentage.} 
#'
#' 
#' @references
#' Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{https://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}.
#' Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en}
#' Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}
#' 
#' @seealso \code{\link{linrmir}},
#'          \code{\link{linarr}},
#'          \code{\link{vardchanges}}
#'
#' @keywords vardcros
#'  
#' @examples
#' 
#' library("data.table")
#' data("eusilc", package = "laeken")
#' setDT(eusilc)
#' 
#' set.seed(1)
#' eusilc <- eusilc[sample(x = .N, size = 3000)]
#' 
#' dataset1 <- data.table(rbindlist(list(eusilc, eusilc)),
#'                        year = c(rep(2010, nrow(eusilc)),
#'                                 rep(2011, nrow(eusilc))))
#' dataset1[age < 0, age := 0]
#' 
#' PSU <- dataset1[, .N, keyby = "db030"][, N := NULL]
#' PSU[, PSU := trunc(runif(nrow(PSU), 0, 100))]
#' PSU[, inc := runif(.N, 20, 100000)]
#' 
#' dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
#' dataset1[, strata := "XXXX"]
#' dataset1[, pl085 := 12 * trunc(runif(.N, 0, 2))]
#' dataset1[, month_at_work := 12 * trunc(runif(.N, 0, 2))]
#' dataset1[, id_l2 := paste0("V", .I)]
#' 
#' vardcrospoor(Y = "inc", age = "age",
#'              pl085 = "pl085", 
#'              month_at_work = "month_at_work",
#'              Y_den = "inc", Y_thres = "inc",
#'              wght_thres = "rb050",
#'              H = "strata", PSU = "PSU", 
#'              w_final = "rb050", ID_level1 = "db030",
#'              ID_level2 = "id_l2",
#'              Dom = c("rb090", "db040"),
#'              country = NULL, period = "year",
#'              sort = NULL, gender = NULL,
#'              dataset = dataset1,
#'              percentage = 60,
#'              order_quant = 50L,
#'              alpha = 20,
#'              confidence = 0.95,
#'              type = "linrmpg")
#'   
#' @import data.table
#' @import laeken
#' @export vardcrospoor

vardcrospoor <- function(Y, age = NULL, pl085 = NULL,
                         month_at_work = NULL, Y_den = NULL,
                         Y_thres = NULL,  wght_thres = NULL, 
                         H, PSU, w_final, ID_level1, ID_level2,
                         Dom = NULL, country = NULL,
                         period, sort = NULL, gender = NULL,
                         dataset = NULL, X = NULL, 
                         countryX = NULL, periodX = NULL, 
                         X_ID_level1 = NULL, ind_gr = NULL,
                         g = NULL, q = NULL, datasetX = NULL,
                         percentage = 60, order_quant = 50,
                         alpha = 20, use.estVar = FALSE,
                         withperiod = TRUE, netchanges = TRUE,
                         confidence = .95, outp_lin = FALSE,
                         outp_res = FALSE, type = "linrmpg",
                         checking = TRUE) {
  ### Checking
  . <- NULL
  all_choices <- c("linarpr", "linarpt", "lingpg",
                   "linpoormed", "linrmpg", "lingini",
                   "lingini2", "linqsr", "linrmir", "linarr")
  type <- tolower(type)
  type <- match.arg(type, all_choices, length(type) > 1) 
  
  percentage <- check_var(vars = percentage, varn = "percentage", varntype = "numeric0100") 
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "numeric0100") 
  alpha <- check_var(vars = alpha, varn = "alpha", varntype = "numeric0100") 
  netchanges <- check_var(vars = netchanges, varn = "netchanges", varntype = "logical") 
  withperiod <- check_var(vars = withperiod, varn = "withperiod", varntype = "logical") 
  use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical") 
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 
  
  if (checking) {
    if (!is.null(X)) {
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
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   dif_name = "dataH_stratas")
    
    sort <- check_var(vars = sort, varn = "sort",
                      dataset = dataset, ncols = 1,
                      Ynrow = Ynrow, mustbedefined = FALSE,
                      isnumeric = TRUE, isvector = TRUE)
    
    Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                     Ynrow = Ynrow, ischaracter = TRUE,
                     mustbedefined = FALSE, duplicatednames = TRUE,
                     dif_name = c("type", "spectype"),
                     grepls = "__")
    
    country <- check_var(vars = country, varn = "country",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE, mustbedefined = FALSE,
                         dif_name = c("percoun", "period_country",
                                      "type", "spectype"))
    
    period <- check_var(vars = period, varn = "period",
                        dataset = dataset, Ynrow = Ynrow,
                        ischaracter = TRUE, duplicatednames = TRUE,
                        withperiod = withperiod,
                        dif_name = c("percoun", "period_country",
                                     names(country), "type", "spectype"))
    
    ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                           dataset = dataset, ncols = 1, Yncol = 0,
                           Ynrow = Ynrow, ischaracter = TRUE)
    
    ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                           dataset = dataset, ncols = 1, Ynrow = Ynrow,
                           ischaracter = TRUE, namesID1 = names(ID_level1),
                           country = country, periods = period)
    
    PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                     ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                     namesID1 = names(ID_level1))
    
    if (!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) | !is.null(countryX) 
       | !is.null(periodX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
      X <- check_var(vars = X, varn = "X", dataset = datasetX,
                     check.names = TRUE, isnumeric = TRUE,
                     dif_name = c(names(period), names(country), names(H),
                                  names(PSU), names(ID_level1), "w_final",
                                  "w_design", "g", "q", "type", "spectype"), dX = "X")
      Xnrow <- nrow(X)
      
      
      ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                          dataset = datasetX, ncols = 1,
                          Xnrow = Xnrow, ischaracter = TRUE,
                          dif_name = c(names(period), names(country), names(H),
                                       names(PSU), names(ID_level1), "w_final",
                                       names(X), "w_design", "g", "q",
                                       "type", "spectype"), dX = "X")
      
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
                               countryX = countryX, periods = period, dX = "X",
                               periodsX = periodX, ID_level1 = ID_level1)            
    }
  }
  
  if (is.null(Y_thres)) Y_thres <- Y
  if (is.null(wght_thres)) wght_thres <- w_final
  namesDom <- names(Dom)  
  
  # Calculation
  Dom1 <- n_h <- stratasf <- name1 <- nhcor <- n_h <- var <- NULL
  num <- count_respondents <- value <- estim <- pop_size <- NULL
  period_country <- N <- se <- rse <- cv <- namesY <- H_sk <- NULL 
  
  estim <- c()
  if (!is.null(country)) { countryper <- copy(country)
  } else countryper <- data.table(percoun = rep("1", length(Y)))
  if (!is.null(period)) countryper <- data.table(period, countryper)
  idper <- data.table(ID_level1, ID_level2, countryper)
  countryperid2 <- c(names(countryper), names(ID_level2))
  
  size <- copy(countryper)
  if (!is.null(Dom)) size <- data.table(size, Dom)
  names_size <- names(size)
  size <- data.table(size, sk = 1, w_final)
  size <- size[, .(count_respondents = .N,
                   pop_size = sum(w_final)), keyby = names_size]
  
  Y1 <- data.table(idper)
  Y1$period_country <- do.call("paste", c(as.list(Y1[, names(countryper), with = FALSE]), sep = "_"))
  Y1 <- data.table(Y1, H, PSU, w_final, check.names = TRUE)
  namesY1 <- names(Y1)
  
  if ("linarpt" %in% type) {
    varpt <- linarpt(Y = Y, id = ID_level2,
                     weight = w_final, sort = sort, 
                     Dom = Dom, period = countryper,
                     dataset = NULL, percentage = percentage,
                     order_quant = order_quant,
                     var_name = "lin_arpt", checking = FALSE)
    Y1 <- merge(Y1, varpt$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("ARPT", varpt$value, NA)
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    varpt <- esti <- NULL
  }
  if ("linarpr" %in% type) {
    varpr <- linarpr(Y = Y, id = ID_level2,
                     weight = w_final, Y_thres = Y_thres,
                     wght_thres = wght_thres, sort = sort, 
                     Dom = Dom, period = countryper,
                     dataset = NULL, percentage = percentage,
                     order_quant = order_quant, var_name = "lin_arpr",
                     checking = FALSE)
    Y1 <- merge(Y1, varpr$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("ARPR", varpr$value, NA)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    varpr <- esti <- NULL
  }
  if (("lingpg" %in% type) & (all(!is.null(gender)))) {
    vgpg <- lingpg(Y = Y, gender = gender, id = ID_level2,
                   weight = w_final, sort = sort, Dom = Dom,
                   period = countryper, dataset = NULL,
                   var_name = "lin_gpg", checking = FALSE)
    Y1 <- merge(Y1, vgpg$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("GPG", vgpg$value, NA)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    vgpg <- esti <- NULL
  }
  if ("linpoormed" %in% type) {
    vporm <- linpoormed(Y = Y, id = ID_level2, weight = w_final,
                        sort = sort, Dom = Dom, period = countryper, 
                        dataset = NULL, percentage = percentage,
                        order_quant = order_quant, var_name = "lin_poormed",
                        checking = FALSE)
    Y1 <- merge(Y1, vporm$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("POORMED", vporm$value, NA)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    vporm <- esti <- NULL
  }
  if ("linrmpg" %in% type) {
    vrmpg <- linrmpg(Y = Y, id = ID_level2, weight = w_final,
                     sort = sort, Dom = Dom, period = countryper,
                     dataset = NULL, percentage = percentage,
                     order_quant = order_quant, var_name = "lin_rmpg",
                     checking = FALSE)
    Y1 <- merge(Y1, vrmpg$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("RMPG", vrmpg$value, NA)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu")) 
    estim <- rbind(estim, esti)
    vrmpg <- esti <- NULL
  }
  if ("linqsr" %in% type) {
    vqsr <- linqsr(Y = Y, id = ID_level2, weight = w_final, 
                   sort = sort, Dom = Dom, period = countryper,
                   dataset = NULL, alpha = alpha, var_name = "lin_qsr",
                   checking = FALSE) 
    Y1 <- merge(Y1, vqsr$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("QSR", vqsr$value)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    vqsr <- esti <- NULL
  }
  if ("lingini" %in% type) {
    vgini <- lingini(Y = Y, id = ID_level2, weight = w_final,
                     sort = sort, Dom = Dom, period = countryper,
                     dataset = NULL, var_name = "lin_gini",
                     checking = FALSE)
    Y1 <- merge(Y1, vgini$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("GINI", vgini$value)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    vgini <- vginia <- esti <- NULL
  }
  if ("lingini2" %in% type) {
    vgini2 <- lingini2(Y = Y, id = ID_level2, weight = w_final,
                       sort = sort, Dom = Dom, period = countryper,
                       dataset = NULL, var_name = "lin_gini2",
                       checking = FALSE)
    Y1 <- merge(Y1, vgini2$lin, all.x = TRUE, by = countryperid2)
    esti <- data.table("GINI2", vgini2$value)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    vgini2 <- esti <- NULL
  }
  if (("linrmir" %in% type) & all(!is.null(age))) {
    vrmir <- linrmir(Y = Y, id = ID_level2, age = age,
                     weight = w_final, sort = sort, Dom = Dom,
                     period = countryper, dataset = NULL,
                     order_quant = order_quant, var_name = "lin_rmir",
                     checking = FALSE) 
    Y1 <- merge(Y1, vrmir$lin, all.x = TRUE, by = countryperid2)
    
    esti <- data.table("RMIR", vrmir$value, NA)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    vrmir <-  esti <- NULL
  } 
  if (("linarr" %in% type) & all(!is.null(age)
                                 & !is.null(pl085) & !is.null(month_at_work))) {
    
    varr <- linarr(Y = Y, Y_den = Y_den, id = ID_level2, age = age,
                   pl085 = pl085, month_at_work = month_at_work, weight = w_final, 
                   sort = sort, Dom = Dom, period = countryper, dataset = NULL,
                   order_quant = order_quant, var_name = "lin_arr",
                   checking = FALSE) 
    
    Y1 <- merge(Y1, varr$lin, all.x = TRUE, by = countryperid2)
    
    esti <- data.table("ARR", varr$value, NA)  
    setnames(esti, names(esti)[c(1, -1:0 + ncol(esti))],
             c("type", "value", "value_eu"))
    estim <- rbind(estim, esti)
    varr <- esti <- NULL
  }
  
  lin_out <- copy(Y1)
  if (!outp_lin) lin_out <- NULL
  
  setnames(estim, "value", "estim")
  estim$period_country <- do.call("paste", c(as.list(estim[, names(countryper), with = FALSE]), sep = "_"))
  nams <- names(countryper)
  if (!is.null(namesDom)) nams <- c(nams, namesDom)
  estim <- merge(estim, size, all = TRUE, by = nams)
  
  namesY2 <- names(Y1)[!(names(Y1) %in% namesY1)]
  namesY2w <- paste0(namesY2, "w")
  
  
  # Calibration
  
  w_design <- res_outp <- NULL
  names_id <- names(ID_level1)
  names_H <- names(H)
  names_PSU <- names(PSU)
  
  namesperc <- c("period_country", names(countryper))
  namesDT1k <- c(namesperc, names_H, names_PSU)
  DTc <- Y1[, lapply(.SD, sum, na.rm = TRUE), keyby = c(namesDT1k, names(ID_level1)), .SDcols = namesY2]
  
  if (!is.null(X)) {
    X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
    X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
    if (!is.null(countryX)) X0 <- data.table(countryX, X0)
    if (!is.null(periodX)) X0 <- data.table(periodX, X0)
    nos <- c(names(periodX), names(countryX), names(ID_level1))
    DT1 <- merge(DTc, X0, by = nos)
    DT1[, w_design := w_final / g ]
    
    ind_gr <- DT1[, c(namesperc, names(ind_gr)), with = FALSE]
    ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))
    
    res <- lapply(split(DT1[, .I], ind_period), function(i)                  
      data.table(DT1[i, nos, with = FALSE],
                 res <- residual_est(Y = DT1[i, namesY2, with = FALSE],
                                     X = DT1[i, names(X), with = FALSE],
                                     weight = DT1[i][["w_design"]],
                                     q = DT1[i][["q"]], dataset = NULL,
                                     checking = FALSE)))
    
    res <- rbindlist(res)
    setnames(res, namesY2, namesY2w)
    DTc <- merge(DTc, res, by = nos) 
    if (outp_res) res_outp <- DTc[, c(nos, names_PSU, "w_final", namesY2w), with = FALSE]
  } else DTc[, (namesY2w) := .SD[, namesY2, with = FALSE]]
  
  DTc[, (namesY2w) := .SD[, namesY2, with = FALSE] * get("w_final")]
  
  size <- ID_level1 <- ID_level2 <- Dom <- country <- NULL
  country <- H <- PSU <- nh <- nh_cor <- NULL
  
  #--------------------------------------------------------*
  # AGGREGATION AT PSU LEVEL ("ULTIMATE CLUSTER" APPROACH) |
  #--------------------------------------------------------*
  
  DTY2 <- DTc[, lapply(.SD, sum, na.rm = TRUE), keyby = namesDT1k, .SDcols = namesY2w]
  setnames(DTY2, namesY2w, namesY2)
  DT1 <- copy(DTY2)
  if (!netchanges) DT1 <- NULL
  
  # NUMBER OF PSUs PER STRATUM
  setkeyv(DTY2, c(namesperc, names_H))
  DTY2[, nh := .N, by = c(namesperc, names_H)]
  
  #--------------------------------------------------------------------------*
  # MULTIVARIATE REGRESSION APPROACH USING STRATUM DUMMIES AS REGRESSORS AND |
  # STANDARD ERROR ESTIMATION 						      |
  #--------------------------------------------------------------------------*
  
  DTY2[, (names_H) := as.factor(get(names_H))]
  DTY2[, paste0(names_H, "_", levels(get(names_H)))] -> DTY2H
  DTY2[, (DTY2H) := transpose(lapply(get(names_H), FUN = function(x){as.numeric(x == levels(get(names_H)))})) ]
  
  namesY2m <-  make.names(namesY2)
  setnames(DTY2, namesY2, namesY2m)
  
  fits <- lapply(1:length(namesY2), function(i) {
    fitss <- lapply(split(DTY2, DTY2$period_country), function(DTY2c) {
      y <- namesY2m[i]
      funkc <- as.formula(paste("cbind(", trimws(toString(y)), ") ~ ",
                                paste(c(-1, DTY2H), collapse = "+")))
      res1 <- lm(funkc, data = DTY2c)
      
      if (use.estVar == TRUE) {res1 <- data.table(crossprod(res1$res))
      } else res1 <- data.table(res1$res)
      setnames(res1, names(res1)[1], "num") 
      res1[, namesY := y]
      
      if (use.estVar == TRUE) {
        setnames(res1, "num", "var") 
        res1 <- data.table(res1[1], DTY2c[1])
      } else {
        res1 <- data.table(res1, DTY2c)
        res1[, nhcor := ifelse(nh > 1, nh / (nh - 1), 1)]
        res1[, var := nhcor * num * num]
      }
      fits <- res1[, lapply(.SD, sum), 
                   keyby = c(namesperc, "namesY"),
                   .SDcols = "var"]
      return(fits)
    })
    return(rbindlist(fitss))
  })
  res <- rbindlist(fits)
  
  estim[, namesY := paste0("lin_", tolower(type))]
  if (!is.null(namesDom)) {
    Dom1 <- estim[, lapply(namesDom, function(x) make.names(paste0(x, ".", get(x))))]
    Dom1 <- Dom1[, Dom := Reduce(function(x, y) paste(x, y, sep = "__"), .SD)]    
    estim <- data.table(estim, Dom1 = Dom1[, Dom])
    estim[, namesY := paste0(namesY, "__", Dom1)]
  } 
  
  res <- merge(estim, res, all = TRUE, 
               by = names(res)[!(names(res) %in% "var")])
  
  Dom1 <- estim <- DT3H <- NULL
  if (is.null(res$Dom1)) res[, Dom1 := "1"]
  res[, (c("namesY", "Dom1", "period_country")) := NULL]
  
  res[, se := sqrt(var)]
  res[, rse := se / estim]
  res[, cv := rse * 100]
  
  res <- res[, c(names(countryper), namesDom, "type", "count_respondents",
                 "pop_size", "estim", "se", "var", "rse", "cv"), with = FALSE]
  
  list(lin_out = lin_out, res_out = res_outp, data_net_changes = DT1, results = res)
}   