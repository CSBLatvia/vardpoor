#' Linearization of the relative median income ratio
#' 
#' @description Estimates the relative median income ratio (defined as the ratio of the median equivalised disposable income of people aged above age  to the median equivalised disposable income of those aged below 65) and computes linearized variable for variance estimation.
#'
#' @param Y Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param age Age variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, linearization of at-risk-of-poverty threshold is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers as numeric vector.
#' @param period Optional variable for survey period. If supplied, linearization of at-risk-of-poverty threshold is done for each survey period. Object convertible to \code{data.table} or variable names as character, column numbers as numeric vector.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param order_quant A numeric value in range \eqn{\left[ 0,100 \right]}{[0,100]} for \eqn{\alpha} in the formula for at-risk-of-poverty threshold computation:
#'       \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{Z(\alpha/100).}
#'For example, to compute the relative median income ratio to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#'
#'
#' @details The implementation strictly follows the Eurostat definition.
#'
#' @return A list with four objects are returned:
#'  \itemize{ 
#'  \item \code{value} - a \code{data.table} containing the estimated relative median income ratio.
#'  \item \code{lin} - a \code{data.table} containing the linearized variables of the relative median income ratio.
#'  }
#'
#' @references
#'Working group on Statistics on Income and Living Conditions (2015) Task 5 - Improvement and optimization of calculation of net change. \emph{LC- 139/15/EN}, Eurostat.  \cr
#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www.statcan.gc.ca/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @keywords Linearization
#' @seealso \code{\link{varpoord}},
#'          \code{\link{vardcrospoor}},
#'          \code{\link{vardchangespoor}}
#'
#' @examples
#' library("laeken")
#' library("data.table")
#' data("eusilc")
#' dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#'  
#' # Full population
#' d <- linrmir(Y = "eqIncome", id = "IDd",  age = "age",
#'              weight = "rb050", Dom = NULL,  
#'              dataset = dataset1, order_quant = 50L)
#  d$value
#'  
#' \dontrun{
#'  # By domains
#'  dd <- linrmir(Y = "eqIncome", id = "IDd", age = "age",
#'                weight = "rb050", Dom = "db040",
#'                dataset = dataset1, order_quant = 50L)
#'  dd}
#'
#' @import data.table
#' @import laeken
#' 
#' @export linrmir



linrmir <- function(Y, id = NULL, age, weight = NULL,
                    sort = NULL, Dom = NULL, period = NULL,
                    dataset = NULL, order_quant = 50,
                    var_name = "lin_rmir", checking = TRUE) {

   ## initializations
   if (min(dim(data.table(var_name)) == 1) != 1) {
       stop("'var_name' must have defined one name of the linearized variable")}

   if (checking) {
          order_quant <- check_var(vars = order_quant, varn = "order_quant",
                                   varntype = "numeric0100") 

          Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                         ncols = 1, isnumeric = TRUE,
                         isvector = TRUE, grepls = "__")
          Ynrow <- length(Y)

          age <- check_var(vars = age, varn = "age",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, isnumeric = TRUE,
                           isvector = TRUE)

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
                      } else period1 <- data.table(ind=ind0)
  period1_agg <- data.table(unique(period1))

  # RMIR by domain (if requested)
  age_under_65s <- data.table(age_under_65s = as.integer(age < 65))
  if (!is.null(Dom)) age_under_65s <- data.table(age_under_65s, Dom)

  quantile <- incPercentile(Y = Y,
                            weights = weight,
                            sort = sort,
                            Dom = age_under_65s,
                            period = period,
                            k = order_quant,
                            dataset = NULL,
                            checking = TRUE)
  quantile_under_65 <- quantile[age_under_65s == 1][, age_under_65s := NULL]
  quantile_over_65 <- quantile[age_under_65s == 0][, age_under_65s := NULL]
  setnames(quantile_under_65, names(quantile_under_65)[ncol(quantile_under_65)], "quantile_under_65")
  setnames(quantile_over_65, names(quantile_over_65)[ncol(quantile_over_65)], "quantile_over_65")
  sk <- length(names(quantile_under_65)) - 1
  if (sk > 0) {
               setkeyv(quantile_under_65, names(quantile_under_65)[1 : sk])
               setkeyv(quantile_over_65, names(quantile_over_65)[1 : sk])
               quantile <- merge(quantile_under_65, quantile_over_65, all = TRUE)
        } else quantile <- data.table(quantile_under_65, quantile_over_65)

  rmir_id <- id
  age_under_65s <- age_under_65s[["age_under_65s"]]
  if (!is.null(period)) rmir_id <- data.table(rmir_id, period)

  if (!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom_agg))

       rmir_v <- c()
       rmir_m <- copy(rmir_id)

       for(i in 1:nrow(Dom_agg)) {
             g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
             var_nams <- do.call(paste, as.list(c(g, sep = "__")))
             ind <- as.integer(rowSums(Dom  ==  Dom_agg[i,][ind0,])  ==  ncol(Dom))

             rmirl <- lapply(1 : nrow(period1_agg), function(j) {
                               if (!is.null(period)) {
                                       rown <- cbind(period_agg[j], Dom_agg[i])
                                       setkeyv(rown, names(rown))
                                       rown2 <- copy(rown)
                                       rown <- merge(rown, quantile, all.x = TRUE)
                                     } else {rown <- quantile[i]
                                             rown2 <- Dom_agg[i] }

                                indj <- (rowSums(period1  ==  period1_agg[j,][ind0,])  ==  ncol(period1))

                                rmir_l <- rmirlinCalc(Y1 = Y[indj],
                                                      ids = rmir_id[indj],
                                                      wght = weight[indj],
                                                      indicator = ind[indj],
                                                      order_quants = order_quant,
                                                      age_under_65 = age_under_65s[indj],
                                                      quant_under_65 = rown[["quantile_under_65"]],
                                                      quant_over_65 = rown[["quantile_over_65"]])

                                list(rmir = data.table(rown2, rmir = rmir_l$rmir_val), lin = rmir_l$lin)
                            })
             rmirs <- rbindlist(lapply(rmirl, function(x) x[[1]]))
             rmirlin <- rbindlist(lapply(rmirl, function(x) x[[2]]))

             setnames(rmirlin, names(rmirlin), c(names(rmir_id), var_nams))
             rmir_m <- merge(rmir_m, rmirlin, all.x = TRUE, by = names(rmir_id))
             rmir_v <- rbind(rmir_v, rmirs)
         }
     } else { rmirl <- lapply(1:nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         rown <- merge(rown, quantile, all.x = TRUE,
                                                        by = names(rown))
                                       } else rown <- quantile
                           ind2 <- (rowSums(period1  ==  period1_agg[j,][ind0,])  ==  ncol(period1))

                           rmir_l <- rmirlinCalc(Y1 = Y[ind2],
                                                 ids = rmir_id[ind2],
                                                 wght = weight[ind2],
                                                 indicator = ind0[ind2],
                                                 order_quants = order_quant,
                                                 age_under_65 = age_under_65s[ind2],
                                                 quant_under_65 = rown[["quantile_under_65"]],
                                                 quant_over_65 = rown[["quantile_over_65"]])
                          if (!is.null(period)) {
                                   rmirs <- data.table(period_agg[j], rmir = rmir_l$rmir_val)
                             } else rmirs <- data.table(rmir = rmir_l$rmir_val)
                          list(rmir = rmirs, lin = rmir_l$lin)
                       })
               rmir_v <- rbindlist(lapply(rmirl, function(x) x[[1]]))
               rmir_m <- rbindlist(lapply(rmirl, function(x) x[[2]]))
               setnames(rmir_m, names(rmir_m), c(names(rmir_id), var_name))
            }
   rmir_m[is.na(rmir_m)] <- 0
   setkeyv(rmir_m, names(rmir_id))
   return(list(value = rmir_v, lin = rmir_m))
}



## workhorse
rmirlinCalc <- function(Y1, ids, wght, indicator, order_quants, age_under_65, quant_under_65, quant_over_65) {

    dom1 <- (age_under_65 == 1) * indicator
    dom2 <- (age_under_65 == 0) * indicator

   # Size of the domains
    N1 <- sum(wght * dom1)
    N2 <- sum(wght * dom2)

    rmir_val <- quant_over_65 / quant_under_65  # Estimated relative median income ratio

    # Bandwith parameter - h=S/N^(1/5) (calculated over the whole population)

    h <- sqrt((sum(wght * Y1 * Y1) - sum(wght * Y1) * sum(wght * Y1) / sum(wght)) / sum(wght)) / exp(0.2 * log(sum(wght)))


    #---- 1. Linearization of the median income of people aged below 65 ----

    u1 <- (quant_under_65 - Y1) / h
    vect_f1 <- exp(-(u1^2) / 2) / sqrt(2 * pi)
    f_quant1 <- sum(vect_f1 * wght * dom1) / (N1 * h)   # Estimate of F'(quantile)

    lin_quant_under_65 <- - (1 / N1) * dom1 * ((Y1 <= quant_under_65) - order_quants / 100) / f_quant1  # Linearized variable

    #---- 2. Linearization of the median income of people aged above 65 -----

    u2 <- (quant_over_65 - Y1) / h
    vect_f2 <- exp(-(u2^2) / 2) / sqrt(2 * pi)
    f_quant2 <- sum(vect_f2 * wght * dom2) / (N2 * h)   # Estimate of F'(quantile)

    lin_quant_over_65 <- -(1 / N2) * dom2 * ((Y1 <= quant_over_65) - order_quants / 100) / f_quant2  # Linearized variable

   #********************************************************************************
   #         3. Linearization of the relative median income ratio                  *
   #********************************************************************************
    lin <- (quant_under_65 * lin_quant_over_65 - quant_over_65 * lin_quant_under_65) / (quant_under_65 * quant_under_65)

    lin_id <- data.table(ids, lin)
    return(list(rmir_val = rmir_val, lin = lin_id))
}
