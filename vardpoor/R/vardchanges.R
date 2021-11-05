#' Variance estimation for measures of change for single and multistage stage cluster sampling designs
#'
#' @description Computes the variance estimation for measures of change for single and multistage stage cluster sampling designs.
#' 
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level2 Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param Z Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.table} or variable names as character, column numbers. This variable is \code{NULL} by default.
#' @param gender Numerical variable for gender, where 1 is for males, but 2 is for females. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param country Variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param period Variable for the all survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param period1 The vector of periods from variable \code{periods} describes the first period.
#' @param period2 The vector of periods from variable \code{periods} describes the second period.
#' @param X Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param countryX Optional variable for the survey countries. The values for each country are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param periodX Optional variable of the all survey periods. If supplied, residual estimation of calibration is done independently for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param X_ID_level1 Variable for level1 ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ind_gr Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param g Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param datasetX Optional survey data object in household level convertible to \code{data.table}.
#' @param linratio Logical value. If value is \code{TRUE}, then the linearized variables for the ratio estimator is used for variance estimation. If value is \code{FALSE}, then the gradients is used for variance estimation.
#' @param percentratio Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.
#' @param use.estVar Logical value. If value is \code{TRUE}, then \code{R} function \code{estVar} is used for the  estimation of covariance matrix of the residuals. If value is \code{FALSE}, then \code{R} function \code{estVar} is not used for the  estimation of covariance matrix of the residuals.
#' @param outp_res Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.
#' @param confidence optional; either a positive value for confidence interval. This variable by default is 0.95 .
#' @param change_type character value net changes type - absolute or relative.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' @return A list with objects are returned by the function:
#' \itemize{
#'     \item \code{res_out} - a \code{data.table} containing the estimated residuals of calibration with ID_level1 and PSU by periods and countries (if available).
#'     #'     \item \code{crossectional_results} - a \code{data.table} containing: \cr
#'        \code{period} -  survey periods, \cr
#'        \code{country} - survey countries, \cr
#'        \code{Dom} - optional variable of the population domains, \cr
#'        \code{namesY} - variable with names of variables of interest, \cr
#'        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
#'        \code{sample_size} - the sample size (in numbers of individuals), \cr
#'        \code{pop_size} - the population size (in numbers of individuals), \cr
#'        \code{total} - the estimated totals, \cr
#'        \code{variance} - the estimated variance of cross-sectional or longitudinal measures, \cr
#'        \code{sd_w} - the estimated weighted variance of simple random sample, \cr
#'        \code{sd_nw} - the estimated variance estimation of simple random sample, \cr
#'        \code{pop} - the population size (in numbers of households), \cr
#'        \code{sampl_siz} - the sample size (in numbers of households), \cr
#'        \code{stderr_w} - the estimated weighted standard error of simple random sample, \cr
#'        \code{stderr_nw} - the estimated standard error of simple random sample, \cr
#'        \code{se} - the estimated standard error of cross-sectional or longitudinal, \cr
#'        \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'        \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'        \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
#'        \code{relative_margin_of_error} - the estimated relative margin of error, \cr
#'        \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'        \code{CI_upper} - the estimated confidence interval upper bound.
#'        #'     \item \code{crossectional_var_grad} - a \code{data.table} containing: \cr
#'        \code{periods} -  survey periods, \cr
#'        \code{country} - survey countries, \cr
#'        \code{Dom} - optional variable of the population domains, \cr
#'        \code{namesY} - variable with names of variables of interest, \cr
#'        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
#'        \code{grad} - the estimated gradient, \cr
#'        \code{var} - the estimated a design-based variance.
#'     \item \code{rho} - a \code{data.table} containing: \cr
#'        \code{periods_1} -  survey periods of \code{periods1}, \cr
#'        \code{periods_2} -  survey periods of \code{periods2}, \cr
#'        \code{country} - survey countries, \cr
#'        \code{Dom} - optional variable of the population domains, \cr
#'        \code{namesY} - variable with names of variables of interest, \cr
#'        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
#'        \code{nams} - the variable names in correlation matrix, \cr
#'        \code{rho} - the estimated correlation matrix.
#'    \item \code{var_tau} - a \code{data.table} containing: \cr
#'        \code{periods_1} -  survey periods of \code{periods1}, \cr
#'        \code{periods_2} -  survey periods of \code{periods2}, \cr
#'        \code{country} - survey countries, \cr
#'        \code{Dom} - optional variable of the population domains, \cr
#'        \code{namesY} - variable with names of variables of interest, \cr
#'        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
#'        \code{nams} - the variable names in correlation matrix, \cr
#'        \code{var_tau} - the estimated covariance matrix.
#'    \item \code{changes_results} - a \code{data.table} containing: \cr
#'        \code{periods_1} -  survey periods of \code{periods1}, \cr
#'        \code{periods_2} -  survey periods of \code{periods2}, \cr
#'        \code{country} - survey countries, \cr
#'        \code{Dom} - optional variable of the population domains, \cr
#'        \code{namesY} - variable with names of variables of interest, \cr
#'        \code{namesZ} - optional variable with names of denominator for ratio estimation, \cr
#'        \code{estim_1} - the estimated value for period1, \cr
#'        \code{estim_2} - the estimated value for period2, \cr
#'        \code{estim} - the estimated value, \cr
#'        \code{var} - the estimated variance, \cr
#'        \code{se} - the estimated standard error, \cr
#'        \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'        \code{CI_upper} - the estimated confidence interval upper bound. \cr
#'        \code{significant} - is the the difference significant.
#'        }
#'
#' @references
#'Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{https://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr
#'Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
#'Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
#'
#' @seealso \code{\link{domain}},
#'          \code{\link{vardcros}},
#'          \code{\link{vardchangespoor}}
#'          
#' @keywords vardchanges
#' 
#' @examples
#' 
#' ### Example 
#' library("data.table")
#' library("laeken")
#' data("eusilc")
#' set.seed(1)
#' eusilc1 <- eusilc[1:40,]
#' set.seed(1)
#' dataset1 <- data.table(rbind(eusilc1, eusilc1),
#'                        year = c(rep(2010, nrow(eusilc1)),
#'                                 rep(2011, nrow(eusilc1))))
#' dataset1[age < 0, age := 0]
#' PSU <- dataset1[, .N, keyby = "db030"][, N := NULL]
#' PSU[, PSU := trunc(runif(nrow(PSU), 0, 5))]
#' dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
#' PSU <- eusilc <- NULL
#' dataset1[, strata := c("XXXX")]
#'
#' dataset1[, t_pov := trunc(runif(nrow(dataset1), 0, 2))]
#' dataset1[, exp := 1]
#' 
#' # At-risk-of-poverty (AROP)
#' dataset1[, pov := ifelse (t_pov == 1, 1, 0)]
#' dataset1[, id_lev2 := paste0("V", .I)]
#'
#'
#' result <- vardchanges(Y = "pov", H = "strata", 
#'                       PSU = "PSU", w_final = "rb050",
#'                       ID_level1 = "db030", ID_level2 = "id_lev2",
#'                       Dom = NULL, Z = NULL, period = "year",
#'                       dataset = dataset1, period1 = 2010,
#'                       period2 = 2011, change_type = "absolute")
#' result
#' 
#' \dontrun{
#' data("eusilc")
#' dataset1 <- data.table(rbind(eusilc, eusilc),
#'                        year = c(rep(2010, nrow(eusilc)),
#'                                 rep(2011, nrow(eusilc))))
#' dataset1[age < 0, age := 0]
#' PSU <- dataset1[,.N, keyby = "db030"][, N := NULL]
#' PSU[, PSU := trunc(runif(nrow(PSU), 0, 100))]
#' dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
#' PSU <- eusilc <- NULL
#' dataset1[, strata := "XXXX"]
#'   
#' dataset1[, t_pov := trunc(runif(nrow(dataset1), 0, 2))]
#' dataset1[, t_dep := trunc(runif(nrow(dataset1), 0, 2))]
#' dataset1[, t_lwi := trunc(runif(nrow(dataset1), 0, 2))]
#' dataset1[, exp := 1]
#' dataset1[, exp2 := 1 * (age < 60)]
#'   
#' # At-risk-of-poverty (AROP)
#' dataset1[, pov := ifelse (t_pov == 1, 1, 0)]
#'   
#' # Severe material deprivation (DEP)
#' dataset1[, dep := ifelse (t_dep == 1, 1, 0)]
#'   
#' # Low work intensity (LWI)
#' dataset1[, lwi := ifelse (t_lwi == 1 & exp2 == 1, 1, 0)]
#'   
#' # At-risk-of-poverty or social exclusion (AROPE)
#' dataset1[, arope := ifelse (pov == 1 | dep == 1 | lwi == 1, 1, 0)]
#' dataset1[, dom := 1]
#' dataset1[, id_lev2 := .I]
#'   
#' result <- vardchanges(Y = c("pov", "dep", "lwi", "arope"),
#'                       H = "strata", PSU = "PSU", w_final = "rb050",
#'                       ID_level1 = "db030", ID_level2 = "id_lev2",
#'                       Dom = "rb090", Z = NULL, period = "year",
#'                       dataset = dataset1, period1 = 2010, 
#'                       period2 = 2011, change_type = "absolute")
#' result}
#' 
#' @import data.table
#' @import laeken
#' 
#' @export vardchanges



vardchanges <- function(Y, H, PSU, w_final,
                        ID_level1, ID_level2,
                        Dom = NULL, Z = NULL,
                        gender = NULL,
                        country = NULL, period,
                        dataset = NULL,
                        period1, period2,
                        X = NULL, countryX = NULL,
                        periodX = NULL, X_ID_level1 = NULL,
                        ind_gr = NULL, g = NULL,
                        q = NULL, datasetX = NULL,
                        linratio = FALSE,
                        percentratio = 1,
                        use.estVar = FALSE,
                        outp_res = FALSE,
                        confidence = 0.95,
                        change_type = "absolute",
                        checking = TRUE) {

  ### Checking
  change_type <- check_var(vars = change_type, varn ="change_type", varntype = "change_type") 
         
  if (checking) { 
        percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger") 
        linratio <- check_var(vars = linratio, varn = "linratio", varntype = "logical") 
        use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical")
        outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical") 
        confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")

        if(!is.null(X)) {
            if (is.null(datasetX)) datasetX <- copy(dataset)
            equal_dataset <- identical(dataset, datasetX) & !is.null(dataset)
            if (equal_dataset) { X_ID_level1 <- ID_level1
            countryX <- country }}

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
                             dif_name = c("percoun", "period_country"))

        period <- check_var(vars = period, varn = "period",
                            dataset = dataset, ncols = 1, Ynrow = Ynrow,
                            ischaracter = TRUE, duplicatednames = TRUE,
                            dif_name = c("percoun", "period_country", names(country)))

        period1 <- check_var(vars = period1, varn = "period1", dataset = NULL,
                             ncols = 1, ischaracter = TRUE, periods = period)

        period2 <- check_var(vars = period2, varn = "period2", dataset = NULL,
                              ncols = 1, ischaracter = TRUE, periods = period)

        ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                               dataset = dataset, ncols = 1, Ynrow = Ynrow,
                               ischaracter = TRUE)

        ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                               dataset = dataset, ncols = 1, Ynrow = Ynrow,
                               ischaracter = TRUE, namesID1 = names(ID_level1),
                               country = country, periods = period)

        PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                         ncols = 1, Yncol = 0, Ynrow = Ynrow,
                         ischaracter = TRUE, namesID1 = names(ID_level1))

        if(!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) | !is.null(countryX) | 
             !is.null(periodX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
                X <- check_var(vars = X, varn = "X", dataset = datasetX,
                               check.names = TRUE, isnumeric = TRUE,
                               dif_name = c(names(period), names(country), names(H),
                                            names(PSU), names(ID_level1), "w_final",
                                            names(Y), "w_design", "g", "q"), dX = "X")
                Xnrow <- nrow(X)

                ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                                    dataset = datasetX, ncols = 1,
                                    Xnrow = Xnrow, ischaracter = TRUE, dX = "X", 
                                    dif_name = c(names(period), names(country), names(H),
                                                 names(PSU), names(ID_level1), "w_final",
                                                 names(Y), names(X), "w_design", "g", "q"))

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
    }
  percoun <- dataset <- datasetX <- NULL

  cros_calc <- vardcros(Y = Y, H = H, PSU = PSU, w_final = w_final,
                        ID_level1 = ID_level1, ID_level2 = ID_level2,
                        Dom = Dom, gender = gender, Z = Z,
                        country = country, period = period,
                        dataset = NULL, X = X, countryX = countryX,
                        periodX = periodX, X_ID_level1 = X_ID_level1,
                        ind_gr = ind_gr, g = g, q = q, datasetX = NULL,
                        linratio = linratio,
                        percentratio = percentratio,
                        use.estVar = use.estVar,
                        ID_level1_max = is.null(X),
                        outp_res = outp_res,
                        withperiod = TRUE,
                        netchanges = TRUE,
                        confidence = confidence,
                        checking = FALSE)
  
  if (!is.null(Dom)) {
         Y1 <- namesD(Y, Dom, uniqueD = TRUE)
         if (!is.null(Z)) Z1 <- namesD(Z, Dom, uniqueD = TRUE)
    } else { Y1 <- names(Y)
             Z1 <- names(Z) }

  countryX <- periodX <- X_ID_level1 <- NULL
  X_ID_level1 <- ind_gr <- g  <- q  <- NULL
  Y <- Z <- gender <- dataset <- w_final <- NULL

  changes_calc <- vardchanges_calculation(Y1 = Y1, Z1 = Z1, Dom = names(Dom),
                                          names_country = names(country), per = names(period),
                                          PSU = names(PSU), H = names(H), period1 = period1,
                                          period2 = period2, cros_var_grad = cros_calc$var_grad,
                                          change_type = change_type, data = cros_calc$data_net_changes,
                                          linratio = linratio, annual = FALSE,
                                          percentratio = percentratio, use.estVar = use.estVar,
                                          confidence = confidence, poor = FALSE)
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


vardchanges_calculation <- function(Y1, Z1, Dom, names_country,
                                    per, PSU, H, period1, period2,
                                    cros_var_grad, change_type, data,
                                    linratio, annual, percentratio, 
                                    use.estVar, confidence,
                                    poor = FALSE){

  country <- ifelse(!is.null(names_country), names_country, "percoun")
  #sarp <- c(country, H, PSU)
  sarp <- c(country, PSU)
  
  namesY <- namesZ <- ind <- nameYs <- nameZs <- grad1 <- grad2 <- NULL
  rot_1 <- rot_2 <- rot_1_rot_2 <- stratasf <- name1 <- NULL
  num1 <- num1num1 <- den1den1 <- den1 <- num2num2 <- NULL
  den2den2 <- den2 <- num1den1 <- num1num2 <- num2 <- NULL
  num1den2 <- den1num2 <- den1den2 <- num2den2 <- num1_1 <- NULL
  den1_1 <- num1den1 <- den1den1 <- num1_2 <- den1_2 <- NULL
  estim <- estim_1 <- estim_2 <- grad1_1 <- grad1_2 <- NULL
  CI_upper <- grad2_1 <- ids_nr <- rot <- grad2_2 <- NULL
  se <-  CI_lower <- valueY1_1 <- valueZ1_1 <- valueY1_2 <- NULL
  valueZ1_2 <- nh <- period_country_1 <- period_country_2 <- NULL
  nhcor <- significant <- id_nams <- nams <- ids_nr <- NULL
  N <- percoun <- confidence_level <- NULL

  
  per1 <- paste0(per, "_1")
  per2 <- paste0(per, "_2")
  period1[, ind := .I]
  period2[, ind := .I]
  setnames(period1, per, per1)
  setnames(period2, per, per2)
  period1 <- merge(period1, period2, by = "ind")
  period2 <- NULL
  var_grad1 <- merge(period1, cros_var_grad, all.x = TRUE,
                              by.x = per1, by.y = per,
                              allow.cartesian = TRUE)
  var_grad2 <- merge(period1, cros_var_grad, all.x = TRUE,
                              by.x = per2, by.y = per,
                              allow.cartesian = TRUE)

  sarc <- c("ind", per1, per2, country, Dom, "type", "namesY", "namesZ")
  sarc <- sarc[sarc %in% names(var_grad1)]
  sar <- names(var_grad1)[!(names(var_grad1) %in% sarc)]
  setnames(var_grad1, sar, paste0(sar, "_1"))
  setnames(var_grad2, sar, paste0(sar, "_2"))

  var_grad <- merge(var_grad1, var_grad2, all = TRUE, by = sarc)
  var_grad[, ids_nr := 1 : .N]

  if (change_type == "relative"){
          if (!linratio & !is.null(Z1)){
                var_grad[, grad1_1 := - valueY1_2 * valueZ1_1 / (valueZ1_2 * (valueY1_1)^2)]
                var_grad[, grad1_2 := valueY1_2 / (valueZ1_2 * valueY1_1)]
                var_grad[, grad2_1 := valueZ1_1 / (valueZ1_2 * valueY1_1)]
                var_grad[, grad1_1 := - valueY1_2 * valueZ1_1 / ((valueZ1_2)^2 * valueY1_1)]
             } else {
                var_grad[, grad1_1 := - valueY1_2 / (valueY1_1)^2]
                var_grad[, grad1_2 := 1 / valueY1_1] }
         } else {
             if (!is.null(var_grad$grad1_1) & !poor){
                     var_grad[, grad1_1 := - grad1_1]
                     var_grad[, grad2_1 := - grad2_1]
                } else {var_grad[, grad1_1 := - 1]
                        var_grad[, grad1_2 := 1] }}
 
  var_grad11 <- copy(var_grad)
  var_grad12 <- copy(var_grad)
  var_grad11[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad1_1, num1_1, 1, "num1")]
  var_grad12[, (c("grad", "cros_var", "nams")) := list(grad1_2, num1_2, "num2")]
  var_grad12[, id_nams := 2 + as.numeric(!is.null(var_grad$grad2_1))]

  var_grad21 <- var_grad22 <- NULL
  if (!is.null(var_grad$grad2_1)) {
        var_grad21 <- copy(var_grad)
        var_grad22 <- copy(var_grad)
        var_grad21[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad2_1, den1_1, 2, "den1")]
        var_grad22[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad2_2, den1_2, 4, "den2")]
   }
  var_gradn <- rbindlist(list(var_grad11, var_grad12,
                              var_grad21, var_grad22), fill = TRUE)

  var_gradn <- var_gradn[, c(sarc, "ids_nr", "id_nams",
                             "nams", "grad", "cros_var"), with = FALSE]

  var_grad11 <- var_grad12 <- NULL
  var_grad21 <- var_grad22 <- NULL
  vstrata1 <- vstrata2 <- vstrata12 <- NULL

  data[, rot := 1]
  data1 <- merge(period1, data, all.x = TRUE,
                    by.x = per1, by.y = per,
                    allow.cartesian = TRUE)
  data2 <- merge(period1, data, all.x = TRUE,
                    by.x = per2, by.y = per,
                    allow.cartesian = TRUE)
  sard <- names(data)[!(names(data) %in% c(sarp, per))]
  
  data[is.na(ids_nr)]
  
  setnames(data1, sard, paste0(sard, "_1"))
  setnames(data2, sard, paste0(sard, "_2"))

  data <- merge(data1, data2, all = TRUE, by = c("ind", per1, per2, sarp))

  if (country == "country") {
               data[is.na(period_country_1), period_country_1 := paste(get(per1), country, sep = "_")] 
               data[is.na(period_country_2), period_country_2 := paste(get(per2), country, sep = "_")]
          } else {
               data[is.na(period_country_1), period_country_1 := paste(get(per1), get(country), sep = "_")] 
               data[is.na(period_country_2), period_country_2 := paste(get(per2), get(country), sep = "_")] }

  data1 <- data2 <- NULL

  recode.NA <- function(DT, cols = seq_len(ncol(DT))) {
     for (j in cols) if (is.numeric(DT[[j]]))
      set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]), 0L, 0))
   }

  data[, nh := .N, by = c("ind", paste0("period_country_", 1:2), paste0(H, "_", 1:2))]

  Hq1 <- paste0(H, "q_1")
  Hq2 <- paste0(H, "q_2")

  data[, (Hq1) := as.factor(get(paste0(H, "_1")))]
  data[, (Hq2) := as.factor(get(paste0(H, "_2")))]
  data[, paste0(Hq1, "_", levels(get(Hq1)))] -> dataHq1
  data[, paste0(Hq2, "_", levels(get(Hq2)))] -> dataHq2
  data[, (dataHq1) := transpose(lapply(get(Hq1), FUN = function(x){as.numeric(x == levels(get(Hq1)))})) ]
  data[, (dataHq2) := transpose(lapply(get(Hq2), FUN = function(x){as.numeric(x == levels(get(Hq2)))})) ]

  den1 <- den2 <- NULL
  sard <- sard[!(sard %in% c(H, "period_country"))]
  recode.NA(data, c(dataHq1, dataHq2, paste0(sard, "_1"), paste0(sard, "_2")))
  
  saraks <- CJ(dataHq1, dataHq2, unique = FALSE)
  saraks[, vstrata1 := paste0(" rot_1 : ", get("dataHq1"), " ")]
  saraks[, vstrata2 := paste0(" rot_2 : ", get("dataHq2"), " ")]
  saraks[, vstrata12 := paste0(" rot_1 : rot_2 : ", get("dataHq1"), " : " , get("dataHq2"), " ")]
  saraks <- unique(unlist(saraks[, c("vstrata1", "vstrata2", "vstrata12")]))
  saraks <- paste(saraks, collapse = "+")

  if (poor) Y1 <- sard[1:(length(sard) - 1)]
  
  fit <- lapply(1 : length(Y1), function(i) {
       fitd <- lapply(split(data, data[["ind"]]), function(data1) {
           fits <- lapply(split(data1, data1[[country]]), function(DT3c) {

                      y1 <- paste0(Y1[i], "_1")
                      y2 <- paste0(Y1[i], "_2")
                      if (!is.null(namesZ) & !linratio) {
                                          z1 <- paste0(",", Z1[i], "_1")
	                                        z2 <- paste0(",", Z1[i], "_2")
                               } else z1 <- z2 <- ""

                      funkc <- as.formula(paste0("cbind(", y1, z1, ", ",
                                                           y2, z2, ") ~ 0 + ", saraks))
                      res <- lm(funkc, data = DT3c)
                      if (use.estVar) { res <- data.table(estVar(res))
                                  } else res <- data.table(res$res)

                      if (!is.null(namesZ) & !linratio) {
                                   setnames(res, names(res), c("num1", "den1", "num2", "den2"))
                                   res[, nameZs := Z1[i]]
                            } else setnames(res, names(res), c("num1", "num2"))

                      nosv <- c("num1", "den1", "num2", "den2")
                      nosv <- names(res)[names(res) %in% nosv]
                      Zvn <- as.integer(!is.null(namesZ) & !linratio)
                      res[, nameYs := Y1[i]]

                      keynames <- c(country, "ind", paste0(per, "_1"),
                                    paste0(per, "_2"), "nameYs", "nameZs")
                      keynames <- keynames[keynames %in% c(names(DT3c), names(res))]

                      if (use.estVar) {
                            res <- data.table(id_nams = 1 : nrow(res), nams = nosv, res, DT3c[1])
                        } else {
                            res <- data.table(res, DT3c)
                            if (annual) { res[, nhcor := ifelse(nh > 1, nh / (nh - 1), 1)]
                                        } else res[, nhcor := 1]

                            res[, num1num1 := num1 * num1 * nhcor]
                            res[, num2num2 := num2 * num2 * nhcor]
                            res[, num1num2 := num1 * num2 * nhcor]
                            res[, id_nams := 0]
                            res[, nams := ""]
                            if (!is.null(namesZ) & !linratio) {
                                  res[, den1den1 := den1 * den1 * nhcor]
                                  res[, den2den2 := den2 * den2 * nhcor]
                                  res[, num1den1 := num1 * den1 * nhcor]
                                  res[, num1den2 := num1 * den2 * nhcor]
                                  res[, den1num2 := den1 * num2 * nhcor]
                                  res[, den1den2 := den1 * den2 * nhcor]
                                  res[, num2den2 := num2 * den2 * nhcor] }

                            varsp <- c("num1num1", "den1den1",
                                       "num2num2", "den2den2",
                                       "num1den1", "num1num2",
                                       "num1den2", "den1num2",
                                       "den1den2", "num2den2")
                            varsp <- varsp[varsp %in% names(res)]
                            fits <- res[, lapply(.SD, sum), keyby = c(keynames,
                                                              "id_nams", "nams"),
                                                           .SDcols = varsp]
                            fits1 <- copy(fits)
                            fits1[, (c("id_nams", "nams")) := list(1, "num1")]
                            setnames(fits1, (c("num1num1", "num1num2")), c("num1", "num2"))

                            fits2 <- copy(fits)
                            fits2[, id_nams := 2 + as.numeric(!is.null(fits$den2den2))]
                            fits2[, nams := "num2"]
                            setnames(fits2, c("num1num2", "num2num2"), c("num1", "num2"))

                            fits3 <- fits4 <- NULL
                             if (!is.null(fits$den2den2)){
                                 setnames(fits1, c("num1den1", "num1den2"), c("den1", "den2"))
                                 setnames(fits2, c("den1num2", "num2den2"), c("den1", "den2"))

                                 fits3 <- copy(fits)
                                 fits3[, (c("id_nams", "nams")) := list(2, "den1")]
                                 setnames(fits3, c("num1den1", "den1num2",
                                                   "den1den1", "den1den2"),
                                                 c("num1", "num2", "den1", "den2"))

                                 fits4 <- copy(fits)
                                 fits4[, (c("id_nams", "nams")) := list(4, "den2")]
                                 setnames(fits4, c("num1den2", "num2den2",
                                                   "den1den2", "den2den2"),
                                                 c("num1", "num2", "den1", "den2"))
                               }
                            res <- rbindlist(list(fits1, fits2, fits3, fits4), fill = TRUE)
                            fits <- fits1 <- fits2 <- fits3 <- fits4 <- NULL
                        }
                      fits <- res[, lapply(.SD, sum),
                                     keyby = c(keynames, "id_nams", "nams"),
                                    .SDcols = nosv]
                     return(fits)
                })
            rbindlist(fits)
         })
       rbindlist(fitd)
   })
   res <- rbindlist(fit)

   set(res, j = country, value = as.character(res[[country]]))
   
   if (poor) var_gradn[, namesY := paste0("lin_", tolower(get("type")))]
   if (!is.null(Dom)) {
          var_gradn[, paste0(Dom, "_ss") := lapply(Dom, function(x) paste0(x,".", get(x)))]
          var_gradn[, nameYs := Reduce(function(x, y)
                                      paste(x, y, sep = "__"), .SD),
                                     .SDcols = c("namesY", paste0(Dom, "_ss"))]
          if (!is.null(namesZ)) { var_gradn[, nameZs := Reduce(function(x, y)
                                                                 paste(x, y, sep = "__"), .SD),
                                                               .SDcols = c("namesZ", paste0(Dom, "_ss"))]
          }
          var_gradn[, (paste0(Dom, "_ss")) := NULL]
       } else { var_gradn[, nameYs := namesY]
                if (!is.null(namesZ)) var_gradn[, nameZs := namesZ]}
 
   if (poor) var_gradn[, namesY := NULL]
   
   nameYZ <- c("nameYs", "nameZs")
   nameYZ <- nameYZ[nameYZ %in% names(res)]

   sars <- c(country, "ind", paste0(per, "_1"),
             paste0(per, "_2"), nameYZ)

   data <- merge(res, var_gradn, all = TRUE, by = c(sars, "id_nams", "nams"))
   res <- fit <- var_gradn <- NULL

   rmax <- max(data[!is.na(ids_nr), .N, by = "ids_nr"][["ids_nr"]])

   nosv <- c("num1", "den1", "num2", "den2")
   nosv <- names(data)[names(data) %in% nosv]


   dat <- lapply(1:rmax, function(i) {

             res <- data[get("ids_nr") == i] 
             res1 <- as.matrix(res[, nosv, with = FALSE])
             rhod <- diag(sqrt(1 / diag(res1)), length(nosv), length(nosv))
             rhod <- data.table((t(rhod) %*% res1) %*% rhod)

             setnames(rhod, names(rhod), paste0("rho_", nosv))
             dmatr <- diag(sqrt(res[["cros_var"]] / diag(res1)),
                                length(nosv), length(nosv))
             var_tau <- data.table((t(dmatr) %*% res1) %*% dmatr)
             dmatr <- data.table(dmatr)
             setnames(dmatr, names(dmatr), paste0("d_", nosv))
             setnames(var_tau, names(var_tau), paste0("var_tau_", nosv))
             res <- data.table(res, rhod, dmatr, var_tau)

             var_t <- (t(res[["grad"]]) %*% as.matrix(var_tau)) %*% res[["grad"]]
             var_t <- var_t[, 1]
             var_grads <- var_grad[get("ids_nr") == i]

             if (change_type == "absolute") {
                          var_grads[, estim := estim_2 - estim_1]
                     } else var_grads[, estim := estim_2 / estim_1 * percentratio]

             var_grads[, var := var_t]
             list(matricas = res, data = var_grads) })

   matricas <- rbindlist(lapply(dat, function(x) x[[1]]))
   datas <- rbindlist(lapply(dat, function(x) x[[2]]))

   if (change_type == "relative" | (!is.null(datas$namesZ) & !linratio)) {
                  datas[, var:=var * (percentratio) ^ 2] }

   datas[var >= 0, se := sqrt(var)]
   tsad <- qnorm(0.5 * (1 + confidence))
   datas[, CI_lower := estim - tsad * se]
   datas[, CI_upper := estim + tsad * se]

   sarc <- c(sarc, "nams")
   sarc <- sarc[!(sarc %in% c("ind"))]

   rho_matrix <- matricas[, c(sarc, paste0("rho_", nosv)), with = FALSE]
   var_tau <- matricas[, c(sarc, paste0("var_tau_", nosv)), with = FALSE]
   grad_var <- matricas[, c(sarc, "grad", "cros_var"), with = FALSE]

   namesYZ <- c("namesY", "namesZ", "type")
   namesYZ <- names(datas)[(names(datas) %in% namesYZ)]

   changes_results <- datas[, c(paste0(per,"_", c(1, 2)), country, Dom,
                                namesYZ, "estim_1",  "estim_2", "estim",
                                "var", "se", "CI_lower", "CI_upper"), with = FALSE]
   changes_results[, confidence_level := confidence]
   changes_results[, significant := TRUE]
   boundss <- as.numeric(change_type == "relative")
   changes_results[CI_lower <= boundss & CI_upper >= boundss, significant := FALSE]

   if (is.null(names_country)) {
            cros_var_grad[, percoun := NULL]
            grad_var[, percoun := NULL]
            rho_matrix[, percoun := NULL]
            var_tau[, percoun := NULL]
            changes_results[, percoun := NULL]  }

   list(cros_var_grad = cros_var_grad,
        grad_var = grad_var,
        rho_matrix = rho_matrix,
        var_tau = var_tau,
        changes_results = changes_results)
 } 