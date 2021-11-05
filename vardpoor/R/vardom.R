#' Variance estimation of the sample surveys in domain by the ultimate cluster method
#'
#' @description Computes the variance estimation of the sample surveys in domain by the ultimate cluster method.
#' 
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables of interest are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, residual estimation of calibration is done independently for each time period. One dimensional object convertible to one-column \code{data.table}.
#' @param PSU_sort optional; if PSU_sort is defined, then variance is calculated for systematic sample.
#' @param N_h Number of primary sampling units in population for each stratum (and period if \code{period} is not \code{NULL}). If \code{N_h = NULL} and \code{fh_zero = FALSE} (default), \code{N_h} is estimated from sample data as sum of weights (\code{w_final}) in each stratum (and period if \code{period} is not \code{NULL}). Optional for single-stage sampling design as it will be estimated from sample data. Recommended for multi-stage sampling design as \code{N_h} can not be correctly estimated from the sample data in this case. If \code{N_h} is not used in case of multi-stage sampling design (for example, because this information is not available), it is advisable to set \code{fh_zero = TRUE}. If \code{period} \bold{is} \code{NULL}. A two-column matrix with rows for each stratum. The first column should contain stratum code. The second column - the number of primary sampling units in the population of each stratum. If \code{period} \bold{is not} \code{NULL}. A three-column matrix with rows for each intersection of strata and period. The first column should contain period. The second column should contain stratum code. The third column - the number of primary sampling units in the population of each stratum and period.
#' @param fh_zero by default FALSE; \code{fh} is calculated as division of n_h and N_h in each strata, if TRUE, \code{fh} value is zero in each strata.
#' @param PSU_level by default TRUE; if PSU_level is TRUE, in each strata \code{fh} is calculated as division of count of PSU in sample (n_h) and count of PSU in frame(N_h). if PSU_level is FALSE, in each strata \code{fh} is calculated as division of count of units in sample (n_h) and count of units in frame (N_h), which calculated as sum of weights.
#' @param Z Optional variables of denominator for ratio estimation. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param X Optional matrix of the auxiliary variables for the calibration estimator. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param ind_gr Optional variable by which divided independently X matrix of the auxiliary variables for the calibration. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param g Optional variable of the g weights. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param confidence Optional positive value for confidence interval. This variable by default is 0.95.
#' @param percentratio Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.
#' @param outp_lin Logical value. If \code{TRUE} linearized values of the ratio estimator will be printed out.
#' @param outp_res Logical value. If \code{TRUE} estimated residuals of calibration will be printed out.
#'
#' @return  A list with objects is returned by the function:
#'   \itemize{
#'         \item \code{lin_out} - a \code{data.table} containing the linearized values of the ratio estimator with id and PSU.
#'         \item \code{res_out} - a \code{data.table} containing the estimated residuals of calibration with id and PSU.
#'         \item \code{betas} - a numeric \code{data.table} containing the estimated coefficients of calibration.
#'         \item \code{all_result} - a \code{data.table}, which containing variables:
#'           \code{variable} - names of variables of interest, \cr
#'           \code{Dom} - optional variable of the population domains, \cr
#'           \code{period} - optional variable of the survey periods, \cr
#'           \code{respondent_count} - the count of respondents, \cr
#'           \code{pop_size} - the estimated size of population, \cr
#'           \code{n_nonzero} - the count of respondents, who answers are larger than zero, \cr
#'           \code{estim} - the estimated value, \cr
#'           \code{var} - the estimated variance, \cr
#'           \code{se} - the estimated standard error, \cr
#'           \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'           \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'           \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
#'           \code{relative_margin_of_error} - the estimated relative margin of error in percentage, \cr
#'           \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'           \code{CI_upper} - the estimated confidence interval upper bound, \cr
#'           \code{confidence_level} - the positive value for confidence interval, \cr       
#'           \code{S2_y_HT} - the estimated variance of the y variable in case of total or the estimated variance of the linearised variable in case of the ratio of two totals using non-calibrated weights, \cr
#'           \code{S2_y_ca} - the estimated variance of the y variable in case of total or the estimated variance of the linearised variable in case of the ratio of two totals using calibrated weights, \cr
#'           \code{S2_res} - the estimated variance of the regression residuals, \cr
#'           \code{var_srs_HT} - the estimated variance of the HT estimator under SRS, \cr
#'           \code{var_cur_HT} - the estimated variance of the HT estimator under current design, \cr
#'           \code{var_srs_ca} - the estimated variance of the calibrated estimator under SRS, \cr
#'           \code{deff_sam} - the estimated design effect of sample design, \cr
#'           \code{deff_est} - the estimated design effect of estimator, \cr
#'           \code{deff} - the overall estimated design effect of sample design and estimator, \cr
#'           \code{n_eff} - the effective sample size.
#'      }
#' @details Calculate variance estimation in domains based on book of Hansen, Hurwitz and Madow.
#'  
#' @references
#' Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr
#' Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second wave onwards? 2012 \cr
#' Guillaume Osier,  Yves Berger,  Tim Goedeme, (2013), Standard error estimation for the EU-SILC indicators of poverty and social exclusion,  Eurostat Methodologies and Working papers, URL \url{https://ec.europa.eu/eurostat/documents/3888793/5855973/KS-RA-13-024-EN.PDF}. \cr
#' Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
#' Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}. \cr
#' 
#' 
#' @seealso \code{\link{domain}},   
#'          \code{\link{lin.ratio}},
#'          \code{\link{residual_est}},
#'          \code{\link{vardomh}},
#'          \code{\link{var_srs}},
#'          \code{\link{variance_est}},
#'          \code{\link{variance_othstr}}
#'          
#' @keywords vardpoor
#' 
#' @examples
#' library("data.table")
#' library("laeken")
#' data(eusilc)
#' dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#' 
#' aa <- vardom(Y = "eqIncome", H = "db040", PSU = "db030",
#'              w_final = "rb050", id = "rb030", Dom = "db040",
#'              period = NULL, N_h = NULL, Z = NULL,
#'              X = NULL, g = NULL, q = NULL, dataset = dataset1,
#'              confidence = .95, percentratio = 100, 
#'              outp_lin = TRUE, outp_res = TRUE)
#'
#' 
#' @import data.table
#' @export vardom

vardom <- function(Y, H, PSU, w_final,
                   id = NULL,
                   Dom = NULL,
                   period = NULL,
                   PSU_sort=NULL,
                   N_h = NULL,
                   fh_zero=FALSE,
                   PSU_level = TRUE,
                   Z = NULL,
                   X = NULL,
                   ind_gr = NULL,
                   g = NULL,
                   q = NULL,
                   dataset = NULL,
                   confidence = .95,
                   percentratio = 1,
                   outp_lin = FALSE,
                   outp_res = FALSE) {

  ### Checking

  fh_zero <- check_var(vars = fh_zero, varn = "fh_zero", varntype = "logical")
  PSU_level <- check_var(vars = PSU_level, varn = "PSU_level", varntype = "logical")
  outp_lin <- check_var(vars = outp_lin, varn = "outp_lin", varntype = "logical")
  outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical")
  percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger")
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")

  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, isnumeric = FALSE,
                 ischaracter = TRUE)

  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, mustbedefined = FALSE,
                      duplicatednames = TRUE)

  id <- check_var(vars = id, varn = "id", dataset = dataset,
                  ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                  periods = period)

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                    namesID1 = names(id))

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   grepls = "__")
  namesDom <- names(Dom)

  w_final <- check_var(vars = w_final, varn = "w_final",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       isnumeric = TRUE, isvector = TRUE)

  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustbedefined = FALSE)

  PSU_sort <- check_var(vars = PSU_sort, varn = "PSU_sort", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        isvector = TRUE, mustbedefined = FALSE, PSUs = PSU)

  if (!is.null(X) | !is.null(g) | !is.null(q) | !is.null(ind_gr)) {
         X <- check_var(vars = X, varn = "X", dataset = dataset,
                        check.names = TRUE, Ynrow = Ynrow, isnumeric = TRUE,
                        dif_name = c(names(period), "g", "q"), dX = "X")
         Xnrow <- nrow(X)

         ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                             dataset = dataset, ncols = 1, Xnrow = Xnrow,
                             ischaracter = TRUE, dX = "X",
                             dif_name = c(names(period), names(X), "g", "q"))

         g <- check_var(vars = g, varn = "g", dataset = dataset,
                        ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                        isvector = TRUE, dX = "X")

         q <- check_var(vars = q, varn = "q", dataset = dataset,
                        ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                        isvector = TRUE, dX = "X")
    }
  N <- dataset <- NULL

  # N_h
  np <- sum(ncol(period))
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (anyNA(N_h)) stop("'N_h' has missing values")
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2," columns"))
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
                pH <- NULL
              }
      setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  }

  ### Calculation


  # Domains
  if (!is.null(Dom)) Y1 <- domain(Y = Y, D = Dom,
                                  dataset = NULL,
                                  checking = FALSE) else Y1 <- Y
  Y <- NULL
  n_nonzero <- copy(Y1)
  Z1 <- NULL
  if (!is.null(period)){ n_nonzero <- data.table(period, n_nonzero)
                         n_nonzero <- n_nonzero[, lapply(.SD, function(x)
                                                          sum(as.integer(abs(x) > .Machine$double.eps))),
                                                          keyby = names(period),
                                                         .SDcols = names(Y1)]
                  } else n_nonzero <- n_nonzero[, lapply(.SD, function(x)
                                                          sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         .SDcols = names(Y1)]


  sar_nr <- respondent_count <- pop_size <- NULL
  nhs <- data.table(respondent_count = 1, pop_size = w_final)
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                      keyby = eval(names(nhs)[0 : 1 - ncol(nhs)]),
                                                     .SDcols = c("respondent_count", "pop_size")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                     .SDcols = c("respondent_count", "pop_size")]

  # Design weights
  if (!is.null(X)) w_design <- w_final / g else w_design <- w_final

  # Ratio of two totals
  linratio_outp <- variableZ <- estim <- deff_sam <- NULL
  deff_est <- deff <- var_est2 <- se <- rse <- cv <- NULL
  absolute_margin_of_error <- relative_margin_of_error <- NULL
  S2_y_HT <- S2_y_ca <- S2_res <- CI_lower <- CI_upper <- NULL
  variable <- deff_sam <- deff_est <- deff <- n_eff <- NULL

  aH <- names(H)
  idper <- id
  if (!is.null(period)) idper <- data.table(idper, period)

  if (!is.null(Z)) {
    if (!is.null(Dom)) Z1 <- domain(Y = Z, D = Dom,
                                    dataset = NULL,
                                    checking = FALSE) else Z1 <- Z
    if (is.null(period)) {
          Y2 <- lin.ratio(Y = Y1, Z = Z1, weight = w_final, Dom = NULL,
                          dataset = NULL, percentratio = percentratio,
                          checking = FALSE)
        } else {
            periodap <- do.call("paste", c(as.list(period), sep = "_"))
            lin1 <- lapply(split(Y1[, .I], periodap), function(i)
                            data.table(sar_nr = i,
                                   lin.ratio(Y = Y1[i], Z = Z1[i],
                                             weight = w_final[i],
                                             Dom = NULL, dataset = NULL,
                                             percentratio = percentratio,
                                             checking = FALSE)))
            Y2 <- rbindlist(lin1)
            setkeyv(Y2, "sar_nr")
            Y2[, sar_nr := NULL]
        }
    if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
    if (outp_lin) linratio_outp <- data.table(idper, PSU, Y2)
  } else {
          Y2 <- Y1
         }

  # Total estimation
  lin1 <- Z <- Y_est <- Z_est <- variableDZ <- NULL

  hY <- data.table(Y1 * w_final)
  if (is.null(period)) { Y_est <- hY[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Y1)]
                } else { hY <- data.table(period, hY)
                         Y_est <- hY[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Y1)]
                       }
  Y_est <- transpos(Y_est, is.null(period), "Y_est", names(period))
  all_result <- Y_est

  if (!is.null(Z1)) {
         YZnames <- data.table(variable = names(Y1), variableDZ = names(Z1))
         all_result <- merge(all_result, YZnames, all = TRUE, by = "variable")

         hZ <- data.table(Z1 * w_final)
         if (is.null(period)) { Z_est <- hZ[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Z1)]
                       } else { hZ <- data.table(period, hZ)
                                Z_est <- hZ[, lapply(.SD, sum, na.rm = T), keyby = names(period), .SDcols = names(Z1)]
                              }
         Z_est <- transpos(Z_est, is.null(period), "Z_est", names(period), "variableDZ")
         all_result <- merge(all_result, Z_est, all = TRUE, by = c(names(period), "variableDZ"))
      }

  vars <- data.table(variable = names(Y1), nr_names = 1 : ncol(Y1))
  all_result <- merge(vars, all_result, all = TRUE, by = "variable")

  n_nonzero <- transpos(n_nonzero, is.null(period), "n_nonzero", names(period))
  all_result <- merge(all_result, n_nonzero, all = TRUE, by = c(names(period), "variable"))
  n_nonzero <- vars <- Y1 <- Z1 <- Y_est <- Z_est <- hY <- hZ <- YZnames <- NULL

  # Calibration

  res_outp <- NULL
  betas <- NULL
  if (!is.null(X)) {
        if (!is.null(period)) ind_gr <- data.table(ind_gr, period)
        ind_gr1 <- copy(ind_gr)
        ind_gr <- do.call("paste", c(as.list(ind_gr), sep = "_"))

        lin1 <- lapply(split(Y2[,.I], ind_gr), function(i) {
                       resid <- residual_est(Y = Y2[i],
                                             X = X[i],
                                             weight = w_design[i],
                                             q = q[i],
                                             dataset = NULL,
                                             checking = FALSE)
                       pers0 <- ind_gr1[i, .N, keyby = c(names(ind_gr1))]
                       list(data.table(sar_nr = i, resid$residuals),
                            data.table(pers0[, N := NULL], resid$betas))
                                    })

        Y3 <- rbindlist(lapply(lin1, function(x) x[[1]]))
        betas <- rbindlist(lapply(lin1, function(x) x[[2]]))
        setkeyv(Y3, "sar_nr")
        Y3[, sar_nr := NULL]
        lin1 <- X <- g <- q <- NULL
        if (outp_res) res_outp <- data.table(idper, PSU, Y3)
    } else Y3 <- Y2

  var_est <- variance_est(Y = Y3, H = H, PSU = PSU,
                          w_final = w_final, N_h = N_h,
                          fh_zero = fh_zero,
                          PSU_level = PSU_level,
                          PSU_sort = PSU_sort,
                          period = period,
                          dataset = NULL,
                          msg = "Current variance estimation",
                          checking = FALSE)
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- merge(all_result, var_est, all = TRUE, by = c(names(period), "variable"))

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y2, H = H, PSU = PSU,
                             w_final = w_design, N_h = N_h,
                             fh_zero = fh_zero,
                             PSU_level = PSU_level,
                             PSU_sort = PSU_sort,
                             period = period,
                             dataset = NULL,
                             msg = "Variance of HT estimator under current design",
                             checking = FALSE)
  idper <- H <- PSU <- PSU_sort <- N_h <- NULL
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT, all = TRUE, by = c(names(period), "variable"))
  var_est <- var_cur_HT <- NULL

  # Variance of HT estimator under SRS
  if (is.null(period)) {
           varsrs <- var_srs(Y = Y2, w = w_design)
           S2_y_HT <- varsrs$S2p
           S2_y_ca <- var_srs(Y = Y2, w = w_final)$S2p
           var_srs_HT <- varsrs$varsrs
       } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y2)),]
                          ind <- (rowSums(per  ==  period)  ==  ncol(period))
                          varsrs <- var_srs(Y = Y2[ind], w = w_design[ind])
                          varsca <- var_srs(Y = Y2[ind], w = w_final[ind])
                          list(S2p = data.table(period_agg[i,], varsrs$S2p),
                               varsrs = data.table(period_agg[i,], varsrs$varsrs),
                               S2pca = data.table(period_agg[i,], varsca$S2p))
                        })
           S2_y_HT <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_HT <- rbindlist(lapply(lin1, function(x) x[[2]]))
           S2_y_ca <- rbindlist(lapply(lin1, function(x) x[[3]]))
      }
  Y2 <- w_design <- NULL
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_HT <- transpos(S2_y_HT, is.null(period), "S2_y_HT", names(period))
  all_result <- merge(all_result, S2_y_HT, all = TRUE, by = c(names(period), "variable"))

  S2_y_ca <- transpos(S2_y_ca, is.null(period), "S2_y_ca", names(period))
  all_result <- merge(all_result, S2_y_ca, all = TRUE, by = c(names(period), "variable"))

  # Variance of calibrated estimator under SRS
  if (is.null(period)) {
           varsres <- var_srs(Y = Y3, w = w_final)
           S2_res <- varsres$S2p
           var_srs_ca <- varsres$varsrs
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
                          per <- period_agg[i][rep(1, nrow(Y3))]
                          ind <- (rowSums(per  ==  period)  ==  ncol(period))
                          varsres <- var_srs(Y = Y3[ind], w = w_final[ind])
                          list(S2p = data.table(period_agg[i,], varsres$S2p),
                               varsrs = data.table(period_agg[i,], varsres$varsrs))
                        })
           S2_res <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_ca <- rbindlist(lapply(lin1, function(x) x[[2]]))
        }
  Y3 <- w_final <- NULL
  var_srs_ca <- transpos(var_srs_ca, is.null(period), "var_srs_ca", names(period), )
  all_result <- merge(all_result, var_srs_ca, all = TRUE, by = c(names(period), "variable"))

  S2_res <- transpos(S2_res, is.null(period), "S2_res", names(period), "variable")
  all_result <- merge(all_result, S2_res, all = TRUE, by = c(names(period), "variable"))
  S2_y_HT <- S2_y_ca <- S2_res <- var_srs_HT <- var_srs_ca <- NULL

  all_result[, estim := Y_est]
  if (!is.null(all_result$Z_est)) all_result[, estim := Y_est / Z_est * percentratio]

  if (nrow(all_result[var_est < 0]) > 0) stop("Estimation of variance are negative!")

  # Design effect of sample design
  all_result[, deff_sam := var_cur_HT / var_srs_HT]

  # Design effect of estimator
  all_result[, deff_est := var_est / var_cur_HT]

  # Overall effect of sample design and estimator
  all_result[, deff := deff_sam * deff_est]

  all_result[, var_est2 := var_est]
  all_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  all_result[, se := sqrt(var_est2)]
  all_result[(estim != 0) & !is.nan(estim), rse := se / estim]
  all_result[estim == 0 | is.nan(estim), rse := NA]
  all_result[, cv := rse * 100]

  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, absolute_margin_of_error := tsad * se]
  all_result[, relative_margin_of_error:= tsad * cv]
  all_result[, CI_lower := estim - absolute_margin_of_error]
  all_result[, CI_upper := estim + absolute_margin_of_error]

  variableD <- NULL
  setnames(all_result, c("variable", "var_est"), c("variableD", "var"))
  if (!is.null(all_result$Z_est)) {
                       nosrZ <- data.table(all_result[, "variableDZ"], all_result[, tstrsplit(variableDZ, "__")][, 1])
                       nosrZ <- nosrZ[!duplicated(nosrZ)]
                       setnames(nosrZ, "V1", "variableZ")
                       all_result <- merge(all_result, nosrZ, by = "variableDZ")
                       nosrZ <- NULL
                    }

  nosr <- data.table(all_result[, "variableD"], all_result[, tstrsplit(variableD, "__")])
  nosr <- nosr[!duplicated(nosr)]
  nosr <- nosr[, lapply(nosr, as.character)]
  setnames(nosr, names(nosr)[2], "variable")

  namesDom1 <- namesDom
  if (!is.null(Dom)) {
       setnames(nosr, names(nosr)[3:ncol(nosr)], paste0(namesDom, "_new"))
       nhs[, (paste0(namesDom, "_new")) := lapply(namesDom, function(x) make.names(paste0(x, ".", get(x))))]
       namesDom1 <- paste0(namesDom, "_new")
    }

  all_result <- merge(nosr, all_result, by = "variableD")
  namesDom <- nosr <- confidence_level <- NULL

  if (!is.null(all_result$Z_est)) {
       all_result[, variable := paste("R", get("variable"), get("variableZ"), sep = "__")] }

  if (!is.null(c(Dom, period))) { all_result <- merge(all_result, nhs,
                                                      all = TRUE, by = c(namesDom1, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]}

  all_result[, n_eff := ifelse(is.na(deff) | deff < .Machine$double.eps, NA, respondent_count / deff)]


  all_result[, confidence_level := confidence]
  variab <- c("respondent_count", "n_nonzero", "pop_size")
  if (!is.null(all_result$Z_est)) variab <- c(variab, "Y_est", "Z_est")
  variab <- c(variab, "estim", "var", "se", "rse", "cv",
              "absolute_margin_of_error", "relative_margin_of_error",
              "CI_lower", "CI_upper", "confidence_level")
  if (is.null(Dom))  variab <- c(variab, "S2_y_HT", "S2_y_ca", "S2_res")
  variab <- c(variab, "var_srs_HT",  "var_cur_HT", "var_srs_ca",
              "deff_sam", "deff_est", "deff", "n_eff")
  setkeyv(all_result, c("nr_names", names(Dom), names(period)))
  all_result <- all_result[, c("variable", names(Dom), names(period), variab), with = FALSE]

  list(lin_out = linratio_outp,
       res_out = res_outp,
       betas = betas,
       all_result = all_result)
}

transpos <- function(variable, period_NULL, valnames, pernames, variabname = NULL) {
  if (period_NULL) {dati <- data.table(nv = names(variable), t(variable))
  setnames(dati, names(dati), c("variable", valnames))
  } else { dati <- melt(variable, id=c(pernames))
  setnames(dati, names(dati)[ncol(dati)], valnames)
  }
  dati[, variable := as.character(variable)]
  if (!is.null(variabname)) { setnames(dati, "variable", variabname)
  } else variabname <- "variable"
  setkeyv(dati, c(pernames, variabname))
  return(dati)
}
