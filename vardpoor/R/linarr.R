#' Linearization of the aggregate replacement ratio
#' 
#' @description Estimates the aggregate replacement ratio (defined as the gross median individual pension income of the population aged 65-74 relative to the gross median individual earnings from work of the population aged 50-59, excluding other social benefits) and computes linearized variable for variance estimation.
#' 
#' @param Y Numerator variable (for gross pension income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Y_den Denominator variable (for example gross individual earnings). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param age Age variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param pl085 Retirement variable (Number of months spent in retirement or early retirement). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param month_at_work Variable for total number of month at work (sum of the number of months spent at full-time work as employee, number of months spent at part-time work as employee, number of months spent at full-time work as self-employed (including family worker), number of months spent at part-time work as self-employed (including family worker)).  One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, linearization of at-risk-of-poverty threshold is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers as numeric vector.
#' @param period Optional variable for survey period. If supplied, linearization of at-risk-of-poverty threshold is done for each survey period. Object convertible to \code{data.table} or variable names as character, column numbers as numeric vector.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param order_quant A numeric value in range \eqn{\left[ 0,100 \right]}{[0,100]} for \eqn{\alpha} in the formula #'for at-risk-of-poverty threshold computation:
#'            \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'For example, to compute at-risk-of-poverty threshold equal to some percentage of median income, \eqn{\alpha} #'should be set equal to 50.
#' @param var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' @details The implementation strictly follows the Eurostat definition.
#' 
#' @return A list with four objects are returned:
#'  \itemize{  
#'     \item \code{value} - a \code{data.table} containing the estimated the aggregate replacement ratio.
#'     \item \code{lin} - a \code{data.table} containing the linearized variables of the aggregate replacement ratio.
#'   }
#'
#' @references
#' Working group on Statistics on Income and Living Conditions (2015) Task 5 - Improvement and optimization of calculation of net change. \emph{LC- 139/15/EN}, Eurostat.  \cr
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{http://www.statcan.gc.ca/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @seealso \code{\link{varpoord}},
#'          \code{\link{vardcrospoor}},
#'           \code{\link{vardchangespoor}}
#' @keywords Linearization
#'
#' @examples
#' library("data.table")
#' library("laeken")
#' data("eusilc")
#' dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#' dataset1$pl085 <- 12 * trunc(runif(nrow(dataset1), 0, 2))
#' dataset1$month_at_work <- 12 * trunc(runif(nrow(dataset1), 0, 2))
#'     
#' # Full population
#' d <- linarr(Y = "eqIncome", Y_den = "eqIncome",
#'             id = "IDd", age = "age",  
#'             pl085 = "pl085", month_at_work = "month_at_work",
#'             weight = "rb050",  Dom = NULL,
#'             dataset = dataset1, order_quant = 50L)
#' d$value
#'     
#' \dontrun{
#' # By domains
#' dd <- linarr(Y = "eqIncome", Y_den = "eqIncome",
#'              id = "IDd", age = "age",  
#'              pl085 = "pl085", month_at_work = "month_at_work",
#'              weight = "rb050",  Dom = "db040",
#'              dataset = dataset1, order_quant = 50L)
#'  dd} 
#' 
#' @import data.table
#' @import laeken
#' @export linarr



linarr <- function(Y, Y_den, id = NULL, age, pl085, month_at_work,
                   weight = NULL,  sort = NULL, Dom = NULL,
                   period = NULL, dataset = NULL, order_quant = 50,
                   var_name = "lin_arr", checking = TRUE) {

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

        Y_den <- check_var(vars = Y_den, varn = "Y_den",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, isnumeric = TRUE,
                           isvector = TRUE)

        weight <- check_var(vars = weight, varn = "weight",
                            dataset = dataset, ncols = 1,
                            Ynrow = Ynrow, isnumeric = TRUE,
                            isvector = TRUE)

        age <- check_var(vars = age, varn = "age",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, isnumeric = TRUE,
                         isvector = TRUE)

        pl085 <- check_var(vars = pl085, varn = "pl085",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, isnumeric = TRUE,
                           isvector = TRUE)

        month_at_work <- check_var(vars = month_at_work, varn = "month_at_work",
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

   dataset <- NULL

   ## computations
   ind0 <- rep.int(1, length(Y))
   period_agg <- period1 <- NULL
   if (!is.null(period)) { period1 <- copy(period)
                           period_agg <- data.table(unique(period))
                         } else period1 <- data.table(ind = ind0)
   period1_agg <- data.table(unique(period1))

   # RMI by domain (if requested)
   age_65_74pl <- data.table(age_65_74pl = as.integer(65 <= age & age <= 74 & pl085 == 12))
   age_50_59mo <- data.table(age_50_59mo = as.integer(50 <= age & age <= 59 & month_at_work == 12))


   if (!is.null(Dom)) { age_65_74pl <- data.table(age_65_74pl, Dom)
                        age_50_59mo <- data.table(age_50_59mo, Dom) }

   quantile1 <- incPercentile(Y = Y,
                              weights = weight,
                              sort = sort,
                              Dom = age_65_74pl,
                              period = period,
                              k = order_quant,
                              dataset = NULL,
                              checking = FALSE)
   quantile2 <- incPercentile(Y = Y_den,
                              weights = weight,
                              sort = sort,
                              Dom = age_50_59mo,
                              period = period,
                              k = order_quant,
                              dataset = NULL,
                              checking = FALSE)

   quantile1 <- quantile1[age_65_74pl == 1][, age_65_74pl := NULL]
   quantile2 <- quantile2[age_50_59mo == 1][, age_50_59mo := NULL]
   setnames(quantile1, names(quantile1)[ncol(quantile1)], "quantile_65_74pl")
   setnames(quantile2, names(quantile2)[ncol(quantile2)], "quantile_50_59mo")
   sk <- length(names(quantile2)) - 1
   if (sk > 0) {
               quantile <- merge(quantile1, quantile2, all = TRUE,
                                 by = names(quantile1)[1 : sk])
        } else quantile <- data.table(quantile1, quantile2)

   arr_id <- id
   quantile1 <- quantile2 <- NULL
   age_65_74pl <- age_65_74pl[["age_65_74pl"]]
   age_50_59mo <- age_50_59mo[["age_50_59mo"]]
   if (!is.null(period)) arr_id <- data.table(arr_id, period)

   if (!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))

        arr_v <- c()
        arr_m <- copy(arr_id)
        for(i in 1:nrow(Dom_agg)) {
              g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
              var_nams <- do.call(paste, as.list(c(g, sep = "__")))
              ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

              arrl <- lapply(1:nrow(period1_agg), function(j) {
                              if (!is.null(period)) {
                                       rown <- cbind(period_agg[j], Dom_agg[i])
                                       setkeyv(rown, names(rown))
                                       rown2 <- copy(rown)
                                       rown <- merge(rown, quantile, all.x = TRUE)
                                     } else {rown <- quantile[i]
                                             rown2 <- Dom_agg[i] }

                               indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                               arr_l <- arrlinCalc(Y_num = Y[indj],
                                                   Y_den = Y_den[indj],
                                                   ids = arr_id[indj],
                                                   wght = weight[indj],
                                                   indicator = ind[indj],
                                                   order_quants = order_quant,
                                                   age_65_74pl = age_65_74pl[indj],
                                                   age_50_59mo = age_50_59mo[indj],
                                                   quant_65_74pls = rown[["quantile_65_74pl"]],
                                                   quant_50_59mon = rown[["quantile_50_59mo"]])

                      list(arr = data.table(rown2, arr = arr_l$arr_val), lin = arr_l$lin)
                      })
                 arrs <- rbindlist(lapply(arrl, function(x) x[[1]]))
                 arrlin <- rbindlist(lapply(arrl, function(x) x[[2]]))

                 setnames(arrlin, names(arrlin), c(names(arr_id), var_nams))
                 arr_m <- merge(arr_m, arrlin, all.x = TRUE, by=names(arr_id))
                 arr_v <- rbind(arr_v, arrs)
           }
     } else { arrl <- lapply(1:nrow(period1_agg), function(j) {

             if (!is.null(period)) { rown <- period_agg[j]
                                     rown <- merge(rown, quantile, all.x = TRUE,
                                                   by = names(rown))
                                 } else rown <- quantile
                           ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           arr_l <- arrlinCalc(Y_num = Y[ind2],
                                               Y_den = Y_den[ind2],
                                               ids = arr_id[ind2],
                                               wght = weight[ind2],
                                               indicator = ind0[ind2],
                                               order_quants = order_quant,
                                               age_65_74pl = age_65_74pl[ind2],
                                               age_50_59mo = age_50_59mo[ind2],
                                               quant_65_74pls = rown[["quantile_65_74pl"]],
                                               quant_50_59mon = rown[["quantile_50_59mo"]])

                          if (!is.null(period)) {
                                   arrs <- data.table(period_agg[j], arr = arr_l$arr_val)
                             } else arrs <- data.table(arr = arr_l$arr_val)
                          list(arr = arrs, lin = arr_l$lin)
                       })
               arr_v <- rbindlist(lapply(arrl, function(x) x[[1]]))
               arr_m <- rbindlist(lapply(arrl, function(x) x[[2]]))
               setnames(arr_m, names(arr_m), c(names(arr_id), var_name))
            }
     arr_m[is.na(arr_m)] <- 0
     setkeyv(arr_m, names(arr_id))
     return(list(value = arr_v, lin = arr_m))
}



## workhorse
arrlinCalc <- function(Y_num, Y_den, ids, wght, indicator, order_quants,
                       age_65_74pl, age_50_59mo, quant_65_74pls, quant_50_59mon) {

    dom1 <- (age_65_74pl == 1) * indicator
    dom2 <- (age_50_59mo == 1) * indicator

   # Size of the domains
    N1 <- sum(wght * dom1)
    N2 <- sum(wght * dom2)

    arr_val <- quant_65_74pls / quant_50_59mon  # Estimated aggregate replacement ratio

    # Bandwith parameter - h=S/N^(1/5) (calculated over the whole population)

    h1 <- sqrt((sum(wght * Y_num * Y_num) - sum(wght * Y_num) * sum(wght * Y_num) / sum(wght)) / sum(wght)) / exp(0.2 * log(sum(wght)))
    h2 <- sqrt((sum(wght * Y_den * Y_den) - sum(wght * Y_den) * sum(wght * Y_den) / sum(wght)) / sum(wght)) / exp(0.2 * log(sum(wght)))

    #---- 1. Linearization of the median income of people aged below 65 ----

    u1 <- (quant_65_74pls - Y_num) / h1
    vect_f1 <- exp(-(u1^2) / 2) / sqrt(2 * pi)
    f_quant1 <- sum(vect_f1 * wght * dom1) / (N1 * h1)   # Estimate of F'(quantile)

    lin_quant_65_74pl <- -(1 / N1) * dom1 * ((Y_num <= quant_65_74pls) - order_quants / 100) / f_quant1  # Linearized variable

    #---- 2. Linearization of the median income of people aged above 65 -----

    u2 <- (quant_50_59mon - Y_den) / h2
    vect_f2 <- exp(-(u2^2)/2) / sqrt(2 * pi)
    f_quant2 <- sum(vect_f2 * wght * dom2) / (N2 * h2)   # Estimate of F'(quantile)

    lin_quant_50_59mon <- -(1 / N2) * dom2 * ((Y_den <= quant_50_59mon) - order_quants / 100) / f_quant2  # Linearized variable

   #********************************************************************************
   #         3. Linearization of the relative median income ratio                  *
   #********************************************************************************
    lin <- (quant_50_59mon * lin_quant_65_74pl - quant_65_74pls * lin_quant_50_59mon) / (quant_50_59mon * quant_50_59mon)

    lin_id <- data.table(ids, lin)
    return(list(arr_val = arr_val, lin = lin_id))
}
