#' Linearization of the Quintile Share Ratio
#'
#' @description  Estimate the Quintile Share Ratio, which is defined as the ratio of the sum of equalized disposable income received by the top 20\% to the sum of equalized disposable income received by the bottom 20\%, and its linearization.
#'
#'
#' @param Y Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, linearization of the income quantile share ratio is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, linearization of the income quantile share ratio is done for each time period. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param alpha a numeric value in range \eqn{[0,100]} for the order of the Quintile Share Ratio.
#' @param var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#'
#' @return  A list with two objects are returned by the function:
#' \itemize{
#'  \item \code{value} - a \code{data.table} containing the estimated Quintile Share Ratio by G. Osier and Eurostat papers.
#'  \item \code{lin} - a \code{data.table} containing the linearized variables of the Quintile Share Ratio by G. Osier paper.
#'  }
#'
#' @references
#'Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap. \emph{EU-SILC 131-rev/04}, Eurostat.  \cr
#'Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.  \cr
#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @seealso \code{\link{incPercentile}},
#'          \code{\link{varpoord}},
#'          \code{\link{vardcrospoor}},
#'          \code{\link{vardchangespoor}}
#'          
#' @keywords Linearization
#'
#' @examples
#' library("data.table")
#' library("laeken")
#' data("eusilc")
#' dataset1 <- data.table(IDd = paste0("V", 1 : nrow(eusilc)), eusilc)
#'
#' # Full population
#' dd <- linqsr(Y = "eqIncome", id = "IDd",
#'              weight = "rb050", Dom = NULL,
#'              dataset = dataset1, alpha = 20)
#' dd$value
#'  
#' \dontrun{
#' # By domains
#' dd <- linqsr(Y = "eqIncome", id = "IDd",
#'              weight = "rb050", Dom = "db040",
#'              dataset = dataset1, alpha = 20)
#' dd$value}
#'
#' @import data.table
#' @import laeken
#' @export linqsr


linqsr <- function(Y, id = NULL, weight = NULL,
                   sort = NULL, Dom = NULL, period = NULL,
                   dataset = NULL, alpha = 20,
                   var_name = "lin_qsr",
                   checking = TRUE) {

   ## initializations
   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
       stop("'var_name' must have defined name of the linearized variable")}

  if (checking) {
         alpha <- check_var(vars = alpha, varn = "alpha",
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

  # QSR by domain (if requested)

  QSR_id <- id
  if (!is.null(period)) QSR_id <- data.table(QSR_id, period)

  if (!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))

        QSR_v <- c()
        QSR_m <- copy(QSR_id)

        for(i in 1:nrow(Dom_agg)) {
              g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
              var_nams <- do.call(paste, as.list(c(g, sep = "__")))

              ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

              QSR_l <- lapply(1:nrow(period1_agg), function(j) {
                               indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))
                               QSR_l <- linQSRCalc(income = Y[indj],
                                                   ids = QSR_id[indj],
                                                   weights = weight[indj],
                                                   sort = sort[indj],
                                                   ind = ind[indj],
                                                   alpha = alpha)
                               if (!is.null(period)) {
                                     list(QSR = data.table(period_agg[j], Dom_agg[i], QSR_l$QSR), lin = QSR_l$lin)
                                   } else list(QSR = data.table(Dom_agg[i], QSR_l$QSR), lin = QSR_l$lin)
                         })
                 QSRs <- rbindlist(lapply(QSR_l, function(x) x[[1]]))
                 QSRlin <- rbindlist(lapply(QSR_l, function(x) x[[2]]))

                 setnames(QSRlin, names(QSRlin), c(names(QSR_id), var_nams))
                 QSR_m <- merge(QSR_m, QSRlin, all.x = TRUE, by = names(QSR_id))
                 QSR_v <- rbind(QSR_v, QSRs)
           }
    } else { QSRl <- lapply(1:nrow(period1_agg), function(j) {

                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           QSR_l <- linQSRCalc(income = Y[indj],
                                               ids = QSR_id[indj],
                                               weights = weight[indj],
                                               sort = sort[indj],
                                               ind = ind0[indj],
                                               alpha = alpha)
                           if (!is.null(period)) {
                                       list(QSR = data.table(period_agg[j], QSR_l$QSR), lin = QSR_l$lin)
                                  } else list(QSR = data.table(QSR_l$QSR), lin = QSR_l$lin)
                       })
             QSR_v <- rbindlist(lapply(QSRl, function(x) x[[1]]))
             QSR_m <- rbindlist(lapply(QSRl, function(x) x[[2]]))

             setnames(QSR_m, names(QSR_m), c(names(QSR_id), var_name))
           }
  QSR_m[is.na(QSR_m)] <- 0
  setkeyv(QSR_m, names(QSR_id))
  return(list(value = QSR_v, lin = QSR_m))
}


linQSRCalc<-function(income, ids, weights = NULL, sort = NULL, ind = NULL, alpha) {
#--------------------------------------------------------------------------------
#----- COMPUTATION OF ESTIMATED VALUES OF THE NUMERATOR AND THE DENOMINATOR -----
#--------------------------------------------------------------------------------
   if (is.null(ind)) ind <- data.frame(ind = rep.int(1, length(ids)))

   alpha2 <- 100 - alpha

   if (sum(weights) > 0 & sum(ind) > 0) {

        quantile <- incPercentile(Y = income, weights = weights,
                                  sort = sort, Dom = data.table(ind),
                                  period = NULL, k = c(alpha, alpha2),
                                  dataset = NULL, checking = FALSE)
        quant_inf <- quantile[ind == 1][[paste0("x", alpha)]]
        quant_sup <- quantile[ind == 1][[paste0("x", alpha2)]]

        wght <- weights * ind
        v <- weights * income * ind

        indinf <- (income <= quant_inf)
        indsup <- (income > quant_sup)

        num_eu <- sum(v * indsup) / sum(wght[indsup]) # Numerator
        den_eu <- sum(v * indinf) / sum(wght[indinf]) # Denominator

        num <- sum(v * indsup) # Numerator
        den <- sum(v * indinf) # Denominator

        QSR <- num / den
        QSR_eu <- num_eu / den_eu

   #**********************************************************************
   #*          LINEARIZATION OF THE INCOME QUANTILE SHARE RATIO          *
   #**********************************************************************

   #----------------------------------------------
   #----- LINEARIZATION OF THE TWO QUANTILES -----
   #----------------------------------------------

       N <- sum(wght) # Estimated (sub)population size
       h <- sqrt((sum(wght * income * income) - sum(wght * income) * sum(wght * income) / sum(wght)) / sum(wght)) / exp(0.2 * log(sum(wght)))
       # h=S/N^(1/5)

       # 1. Linearization of the bottom quantile

       u1 <- (quant_inf - income) / h;
       vect_f1 <- exp(-(u1^2) / 2) / sqrt(2 * pi)
       f_quant1 <- sum(vect_f1 * wght) / (N * h)

       lin_inf <- -(1 / N) * ((income <= quant_inf) - alpha / 100) / f_quant1


       # 2. Linearization of the top quantile

       u2 <- (quant_sup - income) / h
       vect_f2 <- exp( - (u2^2) / 2) / sqrt(2 * pi)
       f_quant2 <- sum(vect_f2 * wght) / (N * h)

       lin_sup <- - (1 / N) * ((income <= quant_sup) - alpha2 / 100) / f_quant2


       # 3. Linearization of the total income for the top quantile

       u3 <- (quant_sup - income) / h
       vect_f3 <- exp(- (u3^2) / 2) / sqrt(2 * pi)
       f_quant3 <- sum(vect_f3 * v) / h

       lin_num <- ind * (income - income * (income <= quant_sup) - f_quant3 * lin_sup)


       # 4. Linearization of the total income for the bottom quantile

       u4 <- (quant_inf - income) / h
       vect_f4 <- exp( - (u4^2) / 2) / sqrt(2 * pi)
       f_quant4 <- sum(vect_f4 * v) / h

       lin_den <- ind * (income * (income <= quant_inf) + f_quant4 * lin_inf)

      #****************************************************************************
      #                 LINEARIZED VARIABLE OF THE QUANTILE SHARE RATIO
      #****************************************************************************

      lin <- (den * lin_num - num * lin_den) / (den * den)
    } else { QSR <- lin <- 0
             QSR_eu <- NaN }

    if (is.nan(QSR)) QSR <- lin <- 0

   lin_id <- data.table(ids, lin = lin)
   QSR <- data.table(QSR = QSR, QSR_eu = QSR_eu)
   return(list(QSR = QSR, lin = lin_id))
 }

