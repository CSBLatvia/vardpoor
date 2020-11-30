#' Variance estimation for measures of annual net change or annual for single stratified sampling designs
#' 
#' @description Computes the variance estimation for measures of annual net change or annual for single stratified sampling designs.
#' 
#' @param boots_count Positive numeric value. Number of replicates, by default - 100
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param ID_level1 Variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Z Optional variables of denominator for ratio estimation. If supplied, the ratio estimation is computed. Object convertible to \code{data.table} or variable names as character, column numbers. This variable is \code{NULL} by default.
#' @param Dom Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param dh n_h-m_h, where n_h is the stratum size and m_h the number of units sampled with replacement. By default, \code{dh=1} (HFCN recommendation)
#' @param fpc Variable for the finite population correction (sampling rate = n_h/N_h). Default = 0.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param years Variable for the all survey years. The values for each year are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param subperiods Variable for the all survey sub-periods. The values for each sub-period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param year1 The vector of years from variable \code{years} describes the first year for measures of annual net change.
#' @param year2 The vector of years from variable \code{periods} describes the second year for measures of annual net change.
#' @param percentratio Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.
#' @param confidence optional; either a positive value for confidence interval. This variable by default is 0.95.
#' @param method character value; value 'cros' is for measures of annual or value 'netchanges' is for measures of annual net change. This variable by default is netchanges.
#'
#'
#' @return  A list with objects are returned by the function:
#'  \itemize{
#'  \item \code{crossectional_results} - a \code{data.table} containing: \cr
#'     \code{year} -  survey years, \cr
#'     \code{subperiods} -  survey sub-periods, \cr
#'     \code{variable} - names of variables of interest, \cr
#'     \code{Dom} - optional variable of the population domains, \cr
#'     \code{estim} - the estimated value, \cr
#'     \code{var} - the estimated variance of cross-sectional and longitudinal measures, \cr
#'     \code{sd_w} - the estimated weighted variance of simple random sample, \cr
#'     \code{se} - the estimated standard error of cross-sectional or longitudinal, \cr
#'     \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'     \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'     \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
#'     \code{relative_margin_of_error} - the estimated relative margin of error, \cr
#'     \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'     \code{CI_upper} - the estimated confidence interval upper bound, \cr 
#'     \code{confidence_level} - the positive value for confidence interval.
#'  \item \code{annual_results} - a \code{data.table} containing: \cr
#'     \code{year_1} -  survey years of \code{years1} for measures of annual net change, \cr
#'     \code{year_2} -  survey years of \code{years2} for measures of annual net change, \cr
#'     \code{Dom} - optional variable of the population domains, \cr
#'     \code{variable} - names of variables of interest, \cr
#'     \code{estim_2} - the estimated value for period2 for measures of annual net change, \cr
#'     \code{estim_1} - the estimated value for period1 for measures of annual net change, \cr
#'     \code{estim} - the estimated value, \cr
#'     \code{var} - the estimated variance, \cr
#'     \code{se} - the estimated standard error, \cr
#'     \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'     \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'     \code{absolute_margin_of_error} - the estimated absolute margin of error for period1 for measures of annual, \cr
#'     \code{relative_margin_of_error} - the estimated relative margin of error in percentage for measures of annual, \cr
#'     \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'     \code{CI_upper} - the estimated confidence interval upper bound, \cr
#'     \code{confidence_level} - the positive value for confidence interval, \cr 
#'     \code{significant} - is the the difference significant \cr
#'  }  
#'  
#'  @references
#'Guillaume OSIER, Virginie RAYMOND, (2015), Development of methodology for the estimate of variance of annual net changes for LFS-based indicators. Deliverable 1 - Short document with derivation of the methodology.
#'
#' @examples
#' 
#' ### Example
#' library("laeken")
#' library("data.table")
#' set.seed(1)
#' 
#' data("eusilc")
#' eusilc1 <- eusilc[1:20,]
#' setDT(eusilc1)
#' eusilc1[, id_lv2 := paste0("V", .I)]
#' 
#' dataset1 <- data.table(rbindlist(list(eusilc1, eusilc1)),
#'                        year = c(rep(2010, nrow(eusilc1)),
#'                                 rep(2011, nrow(eusilc1))))
#'                                 
#' dataset1[, half:= .I - 2 * trunc((.I - 1) / 2)]
#' dataset1[, quarter:= .I - 4 * trunc((.I - 1) / 4)]
#' dataset1[age < 0, age:= 0]
#' 
#' PSU <- dataset1[, .N, keyby = "db030"][, N:= NULL]
#' PSU[, PSU:= trunc(runif(nrow(PSU), 0, 5))]
#' dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
#' PSU <- eusilc <- NULL
#' 
#' dataset1[, strata := c("XXXX")]
#' dataset1[, employed := trunc(runif(nrow(dataset1), 0, 2))]
#' dataset1[, fpc := 0]
#' 
#' \dontrun{
#' result <- vardbootstr(boots_count = 500, Y = "employed", H = "strata",
#'                       PSU = "PSU", w_final = "rb050", ID_level1 = "id_lv2",
#'                       Z = NULL, Dom = NULL, dh = 1, fpc = "fpc",
#'                       dataset = dataset1, years = "year",
#'                       subperiods = "half", year1 = 2010,
#'                       year2 = 2011, percentratio = 100,
#'                       confidence = 0.95, method = "netchanges")
#' result}
#'
#' @seealso \code{\link{vardchanges}},
#'          \code{\link{vardannual}}
#'          
#' @keywords vardannual
#' 
#' @import data.table
#' @import laeken
#' 
#' @export vardbootstr


# Development ####
# Test arguments
# Use data from the example

# boots_count <- 500
# Y <- "employed"
# H <- "strata"
# PSU <- "PSU"
# w_final <- "rb050"
# ID_level1 <- "id_lv2"
# Z <- NULL
# Dom <- NULL
# dh <- 1
# fpc <- "fpc"
# dataset <- dataset1
# years <- "year"
# subperiods <- "half"
# year1 <- 2010
# year2 <- 2011
# percentratio <- 100
# confidence <- 0.95
# method <- "netchanges"


vardbootstr <- function(boots_count = 500, Y, H, PSU,
                        w_final, ID_level1, Z = NULL,
                        Dom = NULL, dh = 1, fpc,
                        dataset = NULL, years,
                        subperiods = NULL, year1 = NULL,
                        year2 = NULL, percentratio = 100,
                        confidence = 0.95, method = "cros") {
  . <- NULL
  method <- check_var(vars = method, varn = "method", varntype = "method") 
  boots_count <- check_var(vars = boots_count, varn = "boots_count", varntype = "pinteger")
  if (boots_count < 2) stop("Iteration must be larger than 2!")
  dh <- check_var(vars = dh, varn = "dh", varntype = "pinteger")  
  percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger")  
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")
  
  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)
  
  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                 dif_name = c("n_sk", "COUNT", "sample_size"))
  
  w_final <- check_var(vars = w_final, varn = "w_final",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       isnumeric = TRUE, isvector = TRUE)
  
  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustbedefined = FALSE)
  
  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   ncols = 0, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, mustbedefined = FALSE,
                   duplicatednames = TRUE, grepls = "__")
  
  fpc <- check_var(vars = fpc, varn = "fpc",
                   dataset = dataset, ncols = 1, Ynrow = Ynrow,
                   isnumeric = TRUE, isvector = TRUE)
  
  years <- check_var(vars = years, varn = "years", dataset = dataset,
                     ncols = 1, Yncol = 0, Ynrow = Ynrow, ischaracter = TRUE,
                     dif_name = c(names(H), "n_sk", "COUNT", 
                                  "sample_size", "Nrs", "iteration_nr"))
  
  if (method != "cros") {
    year1 <- check_var(vars = year1, varn = "year1", dataset = NULL, ncols = 1,
                       ischaracter = TRUE, years = years)
    
    year2 <- check_var(vars = year2, varn = "year2", dataset = NULL,  ncols = 1, 
                       ischaracter = TRUE, years = years)
  } else {if (!missing(year1)) if (!is.null(year1)) stop("'year1' must be NULL")
    if (!missing(year2)) if (!is.null(year2)) stop("'year2' must be NULL")
    year1 <- years[, .N, by = yearm][, N := NULL]
    year2 <- years[, .N, by = yearm][, N := NULL] }

  subperiods <- check_var(vars = subperiods, varn = "subperiods",
                          dataset = dataset, ncols = 1, Ynrow = Ynrow,
                          ischaracter = TRUE, years = years,
                          dif_name = c(names(H), names(years), 
                                       "n_sk", "COUNT", "sample_size",
                                       "Nrs", "iteration_nr"))
  subpm <- names(subperiods)
  subn <- data.table(years, subperiods)
  subn <- subn[, .N, by = c(names(subn))]
  subn <- max(subn[, .N, by = names(years)][["N"]])
  
  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE)
  
  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   namesID1 = names(ID_level1),
                   dif_name = c(names(H), names(years), 
                                "n_sk", "COUNT", "sample_size",
                                "Nrs", "iteration_nr"))
  
  dataset <- data.table(Y, H, PSU, ID_level1, w_final, years, fpc)
  if (!is.null(Z)) dataset <- data.table(dataset, Z)
  if (!is.null(Dom)) dataset <- data.table(dataset, Dom)
  if (!is.null(subperiods)) dataset <- data.table(dataset, subperiods)
  
  CI_lower <- CI_upper <- COUNT <- N <- Nrs <- NumberHits <- NULL
  WA <- WR <- absolute_margin_of_error <- confidence_level <- NULL
  cv <- estim <- estimR <- estimR_1 <- estimR_2 <- estim_1 <- NULL
  estim_2 <- iteration_nr <- n_sk <- nrs <- relative_margin_of_error <- NULL
  rse <- sample_size <- se <- sk <- value <- value_1 <- value_2 <- NULL
  var_est <-  var_est2 <- yearm <- NULL

  Y <- names(Y)
  Z <- names(Z)
  H <- names(H)
  PSU <- names(PSU)
  Dom <- names(Dom)
  fpc <- names(fpc)
  years <- names(years)
  ID_level1 <- names(ID_level1)
  subperiods <- names(subperiods)
  if (!is.null(subperiods)) period <- c(years, subperiods)
  dataset[, sk := .N, by = c(period, H, PSU)]
  dataset[, nrs := 1:.N, by = c(period, H, PSU)]

  frames <- dataset[nrs == sk, c(period, H, PSU), with = FALSE]
  
  frames[, COUNT := .N, by = c(H, period)]
  frames[, sample_size := COUNT - dh]
  frames[sample_size == 0, sample_size := 1]
  frames[, n_sk := 1:.N, by = c(H, period)]
  
  samples <- lapply(1:boots_count, function(i) { 
    strat <- copy(frames)
    strat[, iteration_nr := i]
    strat_agg <- strat[, .(n_sk = sample(.N, size = .SD[n_sk == 1, sample_size], replace = TRUE)), by = c(H, period)]
    setkeyv(strat, names(strat_agg))
    strat_agg <- strat_agg[, .(NumberHits = .N), keyby = names(strat_agg)]
    strat <- merge(strat, strat_agg, all.x = TRUE)
    strat <- strat[!is.na(NumberHits)]
    strat[]
  })
  samples <- rbindlist(samples)
  rm(frames)
  samples[, WA := NumberHits]
  setkeyv(samples, c(period, PSU, H, "iteration_nr"))
  setkeyv(dataset, c(period, PSU, H))
  
  # Looks to be en error!
  # y is not unique by key
  samples <- merge(samples, dataset, by = c(period, PSU, H), all.x = TRUE)
  
  samples[, WR := w_final * ((1 - sqrt(sample_size / (COUNT - 1) * (1 - fpc))) +
                               sqrt(sample_size / (COUNT - 1) * (1 - fpc)) * COUNT / sample_size * WA)]
  samples[COUNT == 1, WR := 1]
  
  period2 <- c(period, Dom)
  
  sarYZ <- c(Y, Z)
  aggr <- samples[, lapply(sarYZ, function(x) { sum(WR * get(x)) / boots_count}),
                  keyby = c(period2, "iteration_nr")]
  setnames(aggr, paste0("V", 1:length(sarYZ)), sarYZ)
  rm(samples, dataset)
  
  aggr2 <- copy(aggr)[, (subperiods) := NULL]
  aggr <- rbindlist(list(aggr, aggr2), fill = TRUE)
  aggr <- aggr[, lapply(.SD, sum), keyby = c(period2, "iteration_nr"),
               .SDcols = sarYZ]
  rm(aggr2)
  sarnew <- Y
  cros_estim <- NULL
  if (!is.null(Z)) { aggr[, (paste0("Ratio_", Y, "_", Z)) := lapply(1:length(Y), function(i) {get(Y) / get(Z)})]
    sarnew <- paste0("Ratio_", Y, "_", Z)
    sarYZ <- c(sarYZ, sarnew)
    cros_estim <- aggr[, lapply(1:length(Y), function(i) {sum(get(Y)) / sum(get(Z)) * percentratio}),
                       keyby = c(period2)]
    setnames(cros_estim, names(cros_estim), c(period2, paste0("Ratio_", Y, "_", Z)))
    cros_estim <- melt(cros_estim, id.vars = c(period2))
    setnames(cros_estim, "value", "estimR")
  }
  
  aggr <- melt(aggr, id.vars = c(period2, "iteration_nr"))
  if (!is.null(Z)) aggr[, value := value * percentratio]
  
  cros_result <- aggr[, .(estim = mean(value),
                          var_est = var(value)), keyby = c(period2, "variable")]
  if (!is.null(Z)) {cros_result <- merge(cros_result, cros_estim, keyby = c(period2, "variable"), all.x = TRUE)
  cros_result[!is.na(estimR), estim := estimR]
  cros_result[, estimR := NULL]   }
  
  cros_result[, var_est2 := var_est]
  cros_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  cros_result[, se := sqrt(var_est2)]
  
  cros_result[(estim != 0) & !is.nan(estim), rse := se / estim]
  cros_result[estim == 0 | is.nan(estim), rse := NA]
  cros_result[, cv := rse * 100]
  
  tsad <- qnorm(0.5 * (1 + confidence))
  cros_result[, absolute_margin_of_error := tsad * se]
  cros_result[, relative_margin_of_error := tsad * cv]
  cros_result[, CI_lower := estim - tsad * se]
  cros_result[, CI_upper := estim + tsad * se]
  cros_result[, confidence_level := confidence]
  setnames(cros_result, "var_est", "var")
  cros_result[, var_est2 := NULL]
              
  changes_result <- NULL
  if (method == "netchanges"){
    aggr_changes <- aggr[is.na(get(subperiods)), c(period2, "iteration_nr", "variable", "value"), with = FALSE]
    aggr_changes[, (subperiods) := NULL]
    
    year1[, Nrs := .I]
    year2[, Nrs := .I]
    cros_estim <- cros_estim[is.na(get(subperiods))]
    cros_estim[, (subperiods) := NULL]
    
    year01 <- merge(year1, aggr_changes, by = years, all.x = TRUE, allow.cartesian = TRUE)
    setnames(year01, c(years, "value"), paste0(c(years, "value"), "_1"))
    
    year02 <- merge(year2, aggr_changes, by = years, all.x = TRUE, allow.cartesian = TRUE)
    setnames(year02, c(years, "value"), paste0(c(years, "value"), "_2"))
    
    changes_result <- merge(year01, year02, by = c("iteration_nr", "Nrs", "variable"), all = TRUE)
    changes_result[, value := value_2 - value_1]
    
    yearsn <- c(paste0(years, "_", 1:2), Dom)
    changes_result <- changes_result[, .(estim_2 = mean(value_2),
                                         estim_1 = mean(value_1),
                                         estim = mean(value),
                                         var_est = var(value)), keyby = c(yearsn, "variable")]
    year1[, Nrs := .I]
    year2[, Nrs := .I]
    
    if (!is.null(Z)) {
      year1 <- merge(year1, cros_estim, by = years, all.x = TRUE, allow.cartesian = TRUE)
      setnames(year1, c(years, "estimR"), paste0(c(years, "estimR"), "_1"))
      
      year2 <- merge(year2, cros_estim, by = years, all.x = TRUE, allow.cartesian = TRUE)
      setnames(year2, c(years, "estimR"), paste0(c(years, "estimR"), "_2"))
      changes_estim <- merge(year1, year2, by = c("Nrs", "variable"), all = TRUE)
      changes_estim[, Nrs := NULL]
      
      changes_result  <- merge(changes_result,  changes_estim, keyby = c(yearsn, "variable"), all = TRUE)
      changes_result[!is.na(estimR_1), estim_1 := estimR_1]
      changes_result[!is.na(estimR_2), estim_2 := estimR_2]    
    }
    
    changes_result[, estim := estim_2 - estim_1]
    
    changes_result[, var_est2 := as.numeric(var_est)]
    changes_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
    changes_result[, se := sqrt(var_est2)]
    
    changes_result[(estim != 0) & !is.nan(estim), rse := se / estim]
    changes_result[estim == 0 | is.nan(estim), rse := NA]
    changes_result[, cv := rse * 100]
    
    tsad <- qnorm(0.5 * (1 + confidence))
    changes_result[, absolute_margin_of_error := tsad * se]
    changes_result[, relative_margin_of_error:= tsad * cv]
    changes_result[, CI_lower := estim - tsad * se]
    changes_result[, CI_upper := estim + tsad * se]
    
    changes_result[, confidence_level := confidence]
    setnames(changes_result, "var_est", "var")
    changes_result[, var_est2 := NULL]
    
    significant <- NULL
    changes_result[, significant := "YES"]
    changes_result[CI_lower <= 0 & CI_upper >= 0, significant := "NO"]
  }
  
  
  list(cros_annual_results = cros_result,
       changes_annual_results = changes_result[])
  
}



