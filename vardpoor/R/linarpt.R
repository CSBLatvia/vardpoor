#' Linearization of at-risk-of-poverty threshold
#' 
#' @description Estimates the at-risk-of-poverty threshold (defined as percentage (usually 60\%) of equalised disposable income after social transfers quantile (usually median)) and computes linearized variable for variance estimation.
#'
#' @param Y Study variable (for example equalised disposable income after social transfers). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param id Optional variable for unit ID codes. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weight Optional weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, linearization of at-risk-of-poverty threshold is done for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers as numeric vector.
#' @param period Optional variable for survey period. If supplied, linearization of at-risk-of-poverty threshold is done for each survey period. Object convertible to \code{data.table} or variable names as character, column numbers as numeric vector.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param percentage A numeric value in range \eqn{\left[ 0,100 \right]}{[0,100]} for \eqn{p} in the formula for at-risk-of-poverty threshold computation:
#' \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#' For example, to compute poverty threshold equal to 60\% of some income quantile, \eqn{p} should be set equal to 60.
#' @param order_quant A numeric value in range \eqn{\left[ 0,100 \right]}{[0,100]} for \eqn{\alpha} in the formula for at-risk-of-poverty threshold computation:
#'  \deqn{\frac{p}{100} \cdot Z_{\frac{\alpha}{100}}.}{p/100 * Z(\alpha/100).}
#'  For example, to compute poverty threshold equal to some percentage of median income, \eqn{\alpha} should be set equal to 50.
#' @param var_name A character specifying the name of the linearized variable.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' 
#' @details The implementation strictly follows the Eurostat definition.
#' 
#' @return A list with three objects are returned:
#' \itemize{
#'   \item \code{quantile} - a \code{data.table} containing the estimated value of the quantile used for at-risk-of-poverty threshold estimation.
#'   \item \code{value} - a \code{data.table} containing the estimated at-risk-of-poverty threshold (in percentage).
#'   \item \code{lin} - a \code{data.table} containing the linearized variables of the at-risk-of-poverty threshold (in percentage).
#' }
#'
#'
#' @references
#' Working group on Statistics on Income and Living  Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap. \emph{EU-SILC 131-rev/04}, Eurostat.  \cr
#' Guillaume Osier (2009). Variance estimation for complex indicators of poverty and inequality. \emph{Journal of the European Survey Research Association}, Vol.3, No.3, pp. 167-195, ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.  \cr
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators: linearization and residual techniques. Survey Methodology, 25, 193-203, URL \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/1999002/article/4882-eng.pdf}.  \cr
#'
#' @seealso \code{\link{linarpr}}, \code{\link{incPercentile}},
#'           \code{\link{varpoord}} , \code{\link{vardcrospoor}},
#'           \code{\link{vardchangespoor}}
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
#' d1 <- linarpt(Y = "eqIncome", id = "IDd",
#'               weight = "rb050", Dom = NULL,
#'               dataset = dataset1, percentage = 60,
#'               order_quant = 50L)
#' d1$value
#' 
#' \dontrun{
#' # By domains
#' d2 <- linarpt(Y = "eqIncome", id = "IDd",
#'               weight = "rb050", Dom = "db040",
#'               dataset = dataset1, percentage = 60,
#'               order_quant = 50L)
#' d2$value}
#'  
#' @import data.table
#' @import laeken
#' 
#' @export linarpt


linarpt <- function(Y, id = NULL, weight = NULL, sort = NULL,
                    Dom = NULL, period=NULL, dataset = NULL,
                    percentage = 60, order_quant = 50,
                    var_name = "lin_arpt", checking = TRUE) {

   ## initializations
   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
        stop("'var_name' must have defined name of the linearized variable")}

   if (checking) {
       percentag <- check_var(vars = percentage, varn = "percentage",
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
   dataset <- NULL

   ## computations
   ind0 <- rep.int(1, length(Y))
   period_agg <- period1 <- NULL
   if (!is.null(period)) { period1 <- copy(period)
                           period_agg <- data.table(unique(period))
                       } else period1 <- data.table(ind = ind0)
   period1_agg <- data.table(unique(period1))

   # ARPT by domain (if requested)

   quantile <- incPercentile(Y = Y,
                             weights = weight,
                             sort = sort,
                             Dom = Dom,
                             period = period,
                             k = order_quant,
                             dataset = NULL,
                             checking = FALSE)

   quantile <- data.table(quantile)
   setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
   if (ncol(quantile) > 1) setkeyv(quantile, head(names(quantile), -1))
   threshold <- copy(quantile)
   threshold[, threshold := percentage / 100 * quantile]
   threshold[, quantile := NULL]

   arpt_id <- id
   if (!is.null(period)) arpt_id <- data.table(arpt_id, period)

   if(!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom_agg))

       arpt_m <- copy(arpt_id)
       for(i in 1 : nrow(Dom_agg)) {
             g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
             var_nams <- do.call(paste, as.list(c(g, sep = "__")))
             ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))
             arpt_l <- lapply(1:nrow(period1_agg), function(j) {
                              if (!is.null(period)) {
                                      rown <- cbind(period_agg[j], Dom_agg[i])
                                      } else rown <- Dom_agg[i]

                              setkeyv(rown, names(rown))
                              rown2 <- copy(rown)
                              rown <- merge(rown, quantile, all.x = TRUE)
                              ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                              arptl <- arptlinCalc(inco = Y[ind2],
                                                   ids = arpt_id[ind2],
                                                   wght = weight[ind2],
                                                   indicator = ind[ind2],
                                                   order_quants = order_quant,
                                                   quant_val = rown[["quantile"]],
                                                   percentag = percentage)
                               })
           arptl <- rbindlist(arpt_l)
           setnames(arptl, names(arptl), c(names(arpt_id), var_nams))
           arpt_m <- merge(arpt_m, arptl, all.x = TRUE, by = names(arpt_id))
        }
    } else { arptl <- lapply(1 : nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x = TRUE)
                                       } else rown <- quantile
                           ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           arptl <- arptlinCalc(inco = Y[ind2],
                                                ids = arpt_id[ind2],
                                                wght = weight[ind2],
                                                indicator = ind0[ind2],
                                                order_quants = order_quant,
                                                quant_val = rown[["quantile"]],
                                                percentag = percentage)
    
                       })
               arpt_m <- rbindlist(arptl)
               setnames(arpt_m, names(arpt_m), c(names(arpt_id), var_name))
            }
    arpt_m[is.na(arpt_m)] <- 0
    setkeyv(arpt_m, names(arpt_id))
    return(list(quantile = quantile, value = threshold, lin = arpt_m))
 }

    ## workhorse
arptlinCalc <- function(inco, ids, wght, indicator,
                        order_quants, quant_val, percentag) {
    wt <- wght * indicator
    N <- sum(wt); # Estimated (sub)population size
    h <- bandwith_plug(y = inco, w = wt)

    f_quant <- gaussian_kern(inco = inco, wt = wt,
                             quant_val = quant_val, hh = h)

 #****************************************************************************************
 #*                    LINEARIZED VARIABLE OF THE POVERTY THRESHOLD                      *
 #****************************************************************************************
    lin <- - (percentag / 100) * (1 / N) * indicator * ((inco <= quant_val) - order_quants / 100) / f_quant
    lin_id <- data.table(ids, lin)
    return(lin_id)
}


bandwith_plug <- function(y, w) {
  N <- sum(w)
  # h=S/N^(1/5)
  1 / N * sqrt(N * sum(w * y ^ 2) - (sum(y * w)) ^ 2) * N ^ (-0.2)
}


gaussian_kern <- function(inco, wt, quant_val, hh){
  N <- sum(wt); # Estimated (sub)population size
  u <- (quant_val - inco) / hh
  vect_f <- exp(-(u^2) / 2) / sqrt(2 * pi)
  f_quant <- sum(vect_f * wt) / (N * hh) # Estimate of F'(quantile)
  return(f_quant)
}

