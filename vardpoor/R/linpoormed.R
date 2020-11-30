#' Linearization of the median income of individuals below the At Risk of Poverty Threshold
#'
#' @description Estimation of the median income of individuals below At Risk of Poverty Threshold and computation of linearized variable for variance estimation. The At Risk of Poverty Threshold is estimated for the whole population always. The median income is estimated for the whole population or for each domain.
#'
#'  
#' @param Y Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, linearization of the median income of persons below a poverty threshold is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, linearization of the median income of persons below a poverty threshold is done for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param percentage A numeric value in range \eqn{[0,100]} for \eqn{p} in the formula for poverty threshold computation: \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).} For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.
#' @param order_quant A numeric value in range \eqn{[0,100]} for \eqn{\alpha} in the formula for poverty threshold computation: \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100)}. For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#'  
#' @return  A list with two objects are returned by the function:
#' \itemize{    
#'    \item \code{value} - a \code{data.table} containing the estimated median income of individuals below the At Risk of Poverty Threshold.
#'  \item \code{lin} - a \code{data.table} containing the linearized variables of the median income below the At Risk of Poverty Threshold.
#' }
#'
#' @references
#'Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap.  \emph{EU-SILC 131-rev/04}, Eurostat.  \cr
#'Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.  \cr
#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @seealso \code{\link{linarpt}},
#'          \code{\link{linrmpg}},
#'          \code{\link{varpoord}},
#'          \code{\link{vardcrospoor}},
#'          \code{\link{vardchangespoor}}
#'  
#' @keywords Linearization
#'
#' @examples
#' library("laeken")
#' library("data.table")
#' data("eusilc")
#' dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#'  
#' # Full population
#' d <- linpoormed(Y = "eqIncome", id = "IDd",
#'                 weight = "rb050", Dom = NULL,
#'                 dataset = dataset1, percentage = 60,
#'                 order_quant = 50L)
#'   
#' \dontrun{
#' # Domains by location of houshold
#' dd <- linpoormed(Y = "eqIncome", id = "IDd",
#'                  weight = "rb050", Dom = "db040",
#'                  dataset = dataset1, percentage = 60,
#'                  order_quant = 50L)
#' dd}
#'
#' @import data.table
#' @import laeken
#' @export linpoormed
  

linpoormed <- function(Y, id = NULL, weight = NULL,
                       sort = NULL, Dom = NULL, period = NULL,
                       dataset = NULL, percentage = 60,
                       order_quant = 50, var_name = "lin_poormed",
                       checking = TRUE) {

   ## initializations
   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
       stop("'var_name' must be defined name of the linearized variable")}

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

   # Poor median people by domain (if requested)

   quantile <- incPercentile(Y = Y,
                             weights = weight,
                             sort = sort,
                             Dom = NULL,
                             period = period,
                             k = order_quant,
                             dataset = NULL,
                             checking = FALSE)

   setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
   if (ncol(quantile) > 1) setkeyv(quantile, head(names(quantile), -1))
   threshold <- copy(quantile)
   threshold[, threshold := percentage / 100 * quantile]
   threshold[, quantile := NULL]

   poor_med_id <- id
   if (!is.null(period))  poor_med_id <- data.table(poor_med_id, period)
   if(!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom))

        poor_med_v <- c()
        poor_med_m <- copy(poor_med_id)
        for(i in 1 : nrow(Dom_agg)) {
              g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
              var_nams <- do.call(paste, as.list(c(g, sep = "__")))
              ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

              poor_medl <- lapply(1 : nrow(period1_agg), function(j) {
                      if (!is.null(period)) {
                               rown <- cbind(period_agg[j], Dom_agg[i])
                               setkeyv(rown, names(rown))
                               rown2 <- copy(rown)
                               rown <- merge(rown, quantile, all.x = TRUE)
                          } else { rown <- quantile
                                   rown2 <- Dom_agg[i] }

                       indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                       poormed_l <- linpoormedCalc(inco = Y[indj],
                                                   ids = poor_med_id[indj],
                                                   wght = weight[indj],
                                                   sort = sort[indj],
                                                   ind = ind[indj],
                                                   percentag = percentage,
                                                   order_quants = order_quant,
                                                   quant_val = rown[["quantile"]])
                      list(poor_people_median = data.table(rown2, poor_people_median = poormed_l$poor_people_median),
                           lin = poormed_l$lin)
                 })

              poor_people_med <- rbindlist(lapply(poor_medl, function(x) x[[1]]))
              poor_people_medlin <- rbindlist(lapply(poor_medl, function(x) x[[2]]))

              setnames(poor_people_medlin, names(poor_people_medlin), c(names(poor_med_id), var_nams))
              poor_med_m <- merge(poor_med_m, poor_people_medlin,
                                  all.x = TRUE, by = names(poor_med_id))
              poor_med_v <- rbind(poor_med_v, poor_people_med)
           }

     } else { poormed_l <- lapply(1:nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         rown <- merge(rown, quantile, all.x = TRUE,
                                                         by = names(rown))
                                       } else rown <- quantile
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           poor_medl <- linpoormedCalc(inco = Y[indj],
                                                       ids = poor_med_id[indj],
                                                       wght = weight[indj],
                                                       sort = sort[indj],
                                                       ind = ind0[indj],
                                                       percentag = percentage,
                                                       order_quants = order_quant,
                                                       quant_val = rown[["quantile"]])
                           if (!is.null(period)) {
                               poor_med_v <- data.table(period_agg[j], poor_people_median = poor_medl$poor_people_median)
                             } else poor_med_v <- data.table(poor_people_median = poor_medl$poor_people_median)
                          list(poor_med_v = poor_med_v, lin = poor_medl$lin)
                       })
               poor_med_v <- rbindlist(lapply(poormed_l, function(x) x[[1]]))
               poor_med_m <- rbindlist(lapply(poormed_l, function(x) x[[2]]))
               setnames(poor_med_m, names(poor_med_m), c(names(poor_med_id), var_name))
            }
  poor_med_m[is.na(poor_med_m)] <- 0
  setkeyv(poor_med_m, names(poor_med_id))
  return(list(quantile = quantile, threshold = threshold, value = poor_med_v, lin = poor_med_m))
}


## workhorse
linpoormedCalc <- function(inco, ids, wght, sort, ind, percentag, order_quants, quant_val) {
    wt <- ind * wght
    thres_val <- percentag / 100 * quant_val
    N0 <- sum(wght)             # Estimated whole population size
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
    vect_f1 <- exp( -(u1^2) / 2) / sqrt(2 * pi)
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
     vect_f3 <- exp(- (u3^2) / 2) / sqrt(2 * pi)
     f_quant3 <- sum(vect_f3 * wt) / (N * h)

     lin_median <- (0.5 * lin_rate - (1 / N) * ind * ((inco <= poor_people_median) - 0.5 * rate_val)) / f_quant3

     lin_id <- data.table(ids, lin_median)

     return(list(poor_people_median = poor_people_median, lin = lin_id))
}
