#' Linearization of the relative median at-risk-of-poverty gap
#' 
#' @description Estimate the relative median at-risk-of-poverty gap, which is defined as the relative difference between the median equalized disposable income of persons below the At Risk of Poverty Threshold and the At Risk of Poverty Threshold itself (expressed as a percentage of the at-risk-of-poverty threshold) and its linearization.
#'
#' @param Y Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, linearization of the relative median at-risk-of-poverty gap is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, linearization of the relative median at-risk-of-poverty gap is done for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param percentage A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation:
#'     \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.
#' @param order_quant A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation:
#'    \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' return A list with two objects are returned by the function:
#' \itemize{
#'  \item \code{value} - a \code{data.table} containing the estimated relative median at-risk-of-poverty gap (in percentage).
#'  \item \code{lin} - a \code{data.table} containing the linearized variables of the relative median at-risk-of-poverty gap (in percentage).
#'  }
#'
#'
#' @references
#'Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.  \cr
#'Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.  \cr
#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @seealso \code{\link{linarpt}},
#'          \code{\link{linarpr}},
#'          \code{\link{linpoormed}},
#'          \code{\link{varpoord}},
#'          \code{\link{vardcrospoor}},
#'          \code{\link{vardchangespoor}}
#' @keywords Linearization
#'
#'
#' @examples
#' library("data.table")
#' library("laeken")
#' data("eusilc")
#' dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#'
#' # Full population
#' d <- linrmpg(Y = "eqIncome", id = "IDd",
#'              weight = "rb050", Dom = NULL,
#'              dataset = dataset1, percentage = 60,
#'              order_quant = 50L)
#' d$value
#' d$threshold
#'   
#' \dontrun{
#' # By domains
#' dd <- linrmpg(Y = "eqIncome", id = "IDd",
#'               weight = "rb050", Dom = "db040",
#'               dataset = dataset1, percentage = 60,
#'               order_quant = 50L)
#' dd$value}
#'
#' @import data.table
#' @import laeken
#' 
#' @export linrmpg




linrmpg <- function(Y, id = NULL, weight = NULL, sort = NULL,
                    Dom = NULL, period = NULL, dataset = NULL,
                    percentage = 60, order_quant = 50,
                    var_name = "lin_rmpg", checking = TRUE) {

   ## initializations

   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if (checking) {
          percentage <- check_var(vars = percentage, varn = "percentage",
                                  varntype = "numeric0100")

          order_quant <- check_var(vars = order_quant, varn = "order_quant",
                                   varntype = "numeric0100") 

          Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                         ncols = 1, isnumeric = TRUE,
                         isvector = TRUE, grepls = "__")
          Ynrow <- length(Y)

          weight <- check_var(vars = weight, varn = "weight",
                              dataset = dataset, ncols = 1,
                              Ynrow = Ynrow, isnumeric = TRUE,
                              isvector = TRUE)

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

          id <- check_var(vars = id, varn = "id", dataset = dataset,
                           ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                           periods = period)
      }
  
  ## computations
  ind0 <- rep.int(1, length(Y))
  period_agg <- period1 <- NULL
  if (!is.null(period)) { period1 <- copy(period)
                          period_agg <- data.table(unique(period))
                      } else period1 <- data.table(ind = ind0)
  period1_agg <- data.table(unique(period1))

  # Relative median at-risk-of-poverty gap by domain (if requested)

  quantile <- incPercentile(Y = Y,
                            weights = weight,
                            sort = sort,
                            Dom = NULL,
                            period = period,
                            k = order_quant,
                            dataset = NULL,
                            checking = FALSE)
  setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
  if (ncol(quantile)>1) setkeyv(quantile, head(names(quantile), -1))
  threshold <- copy(quantile)
  threshold[, threshold := percentage / 100 * quantile]
  threshold[, quantile := NULL]

  rmpgap_id <- id
  if (!is.null(period)) rmpgap_id <- data.table(period, rmpgap_id)

  if (!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom))

       rmpgap_v <- c()
       rmpgap_m <- copy(rmpgap_id)

       for(i in 1:nrow(Dom_agg)) {
                g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
                var_nams <- do.call(paste, as.list(c(g, sep = "__")))
                ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

                rmpgapl <- lapply(1 : nrow(period1_agg), function(j) {
                      if (!is.null(period)) {
                               rown <- cbind(period_agg[j], Dom_agg[i])
                               setkeyv(rown, names(rown))
                               rown2 <- copy(rown)
                               rown <- merge(rown, quantile, all.x = TRUE)
                          } else { rown <- quantile
                                   rown2 <- Dom_agg[i] }

                      indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                      rmpgap_l <- linrmpgCalc(inco = Y[indj],
                                              ids = rmpgap_id[indj],
                                              wght = weight[indj],
                                              sort = sort[indj],
                                              ind = ind[indj],
                                              percentag = percentage,
                                              order_quants = order_quant,
                                              quant_val = rown[["quantile"]])

                      list(rmpgap = data.table(rown2, rmpgap = rmpgap_l$rmpgap),
                           lin = rmpgap_l$lin)
                  })

              rmpgaps <- rbindlist(lapply(rmpgapl, function(x) x[[1]]))
              rmpgaplin <- rbindlist(lapply(rmpgapl, function(x) x[[2]]))

              setnames(rmpgaplin, names(rmpgaplin), c(names(rmpgap_id), var_nams))
              rmpgap_m <- merge(rmpgap_m, rmpgaplin, all.x = TRUE, by = names(rmpgap_id))
              rmpgap_v <- rbind(rmpgap_v, rmpgaps)
         }
  } else {rmpgap_l <- lapply(1 : nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x = TRUE)
                                       } else rown <- quantile
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           rmpgapl <- linrmpgCalc(inco = Y[indj],
                                                  ids = rmpgap_id[indj],
                                                  wght = weight[indj],
                                                  sort = sort[indj],
                                                  ind = ind0[indj],
                                                  percentag = percentage,
                                                  order_quants = order_quant,
                                                  quant_val = rown[["quantile"]])
                           if (!is.null(period)) {
                               rmpgap_v <- data.table(period_agg[j], rmpgap = rmpgapl$rmpgap)
                             } else rmpgap_v <- data.table(rmpgap = rmpgapl$rmpgap)
                          list(rmpgap_v = rmpgap_v, lin = rmpgapl$lin)
                       })
               rmpgap_v <- rbindlist(lapply(rmpgap_l, function(x) x[[1]]))
               rmpgap_m <- rbindlist(lapply(rmpgap_l, function(x) x[[2]]))
               setnames(rmpgap_m, names(rmpgap_m), c(names(rmpgap_id), var_name))
         }
   rmpgap_m[is.na(rmpgap_m)] <- 0
   setkeyv(rmpgap_m, names(rmpgap_id))
   return(list(quantile = quantile, threshold = threshold, value = rmpgap_v, lin = rmpgap_m))
}

## workhorse
linrmpgCalc <- function(inco, ids, wght, sort, ind, percentag, order_quants, quant_val) {
    wt <- ind * wght
    thres_val <- percentag / 100 * quant_val
    N0 <- sum(wght)                # Estimated whole population size
    N <- sum(wt)                # Estimated (sub)population size
    poor <- (inco <= thres_val) * ind

    inc1 <- inco[poor == 1]
    wght1 <- wght[poor == 1]
    sort1 <- sort[poor == 1]

    rate_val <- sum(wt * poor) / N  # Estimated poverty rate
    rate_val_pr <- 100 * rate_val  # Estimated poverty rate

    poor_people_median <- incPercentile(Y = inc1,
                                        weights = wght1,
                                        sort = sort1,
                                        Dom = NULL,
                                        period = NULL,
                                        k = order_quants,
                                        dataset = NULL,
                                        checking = FALSE)
    poor_people_median <- poor_people_median[[paste0("x", order_quants)]]

#*************************************************************************************
 #**          LINEARIZATION OF THE MEDIAN INCOME BELOW THE POVERTY THRESHOLD         **
 #*************************************************************************************
    h <- sqrt((sum(wght * inco * inco) - sum(wght * inco) * sum(wght * inco) / sum(wght)) / sum(wght)) / exp(0.2 * log(sum(wght)))
   # h=S/N^(1/5)

 #--------------------------------------------------
 #----- LINEARIZATION OF THE POVERTY THRESHOLD -----
 #--------------------------------------------------
    u1 <- (quant_val - inco) / h
    vect_f1 <- exp(-(u1^2) / 2)/sqrt(2 * pi)
    f_quant1 <- sum(vect_f1 * wght)/(N0 * h)

    lin_thres <- - (percentag / 100) * (1 / N0) * ((inco <= quant_val) - order_quants / 100) / f_quant1
 # ---------------------------------------------
 # ----- LINEARIZATION OF THE POVERTY RATE -----
 # ---------------------------------------------
    u2 <- (thres_val - inco) / h
    vect_f2 <- exp(-(u2^2) / 2) / sqrt(2 * pi)
    f_quant2 <- sum(vect_f2 * wt) / (N * h)

    lin_rate <- (1 / N) * ind * ((inco <= thres_val) - rate_val) + f_quant2 * lin_thres
 # --------------------------------------------------------
 # ----- LINEARIZATION OF POOR PEOPLE'S MEDIAN INCOME -----
 # --------------------------------------------------------
     u3 <- (poor_people_median - inco) / h
     vect_f3 <- exp(-(u3^2) / 2) / sqrt(2 * pi)
     f_quant3 <- sum(vect_f3 * wt) / (N * h)

     lin_median <- (0.5 * lin_rate - (1 / N) * ind * ((inco <= poor_people_median) - 0.5 * rate_val)) / f_quant3

 #*****************************************************************************************
 #                   LINEARIZED VARIABLE OF THE RELATIVE MEDIAN GAP (IN %)                *
 #*****************************************************************************************
     lin_gap <- 100 * (poor_people_median * lin_thres / (thres_val * thres_val) - lin_median / thres_val)

     rmpgap <- 100 - 100 * poor_people_median / thres_val

     lin_id <- data.table(ids, lin_gap)

     return(list(rmpgap = rmpgap, lin = lin_id))
}

