#' Estimation of weighted percentiles
#' 
#' @description The function computes the estimates of weighted percentiles.
#' 
#' @param Y Study variable (for example equalized disposable income). One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param weights Optional weight variable. One dimensional object convert to one-column \code{data.table} or variable name as character, column number.
#' @param sort Optional variable to be used as tie-breaker for sorting. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, the estimates of percentiles are computed for each domain. An object convertable to \code{data.table} or variable names as character vector, column numbers.
#' @param period Optional variable for survey period. If supplied, linearization of at-risk-of-poverty threshold is done for each survey period. Object convertible to \code{data.table} or variable names as character, column numbers as numeric vector.
#' @param k A vector of values between 0 and 100 specifying the percentiles to be computed (0 gives the minimum, 100 gives the maximum).
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' @return A data.table containing the estimates of weighted income percentiles specified by \code{k}.
#'
#' @references
#'  Working group on Statistics on Income and Living Conditions (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay gap. \emph{EU-SILC 131-rev/04}, Eurostat.
#'
#' @seealso \code{\link{linarpt}}, \code{\link{linarpr}}, \code{\link{linqsr}}
#' @keywords Linearization
#' 
#' @examples
#' library("laeken")
#' data("eusilc")
#' incPercentile(Y = "eqIncome", weights = "rb050", Dom = "db040", dataset = eusilc)
#'
#' @import data.table
#' @import utils
#' @import laeken
#' @export incPercentile


incPercentile <- function(Y, weights = NULL, sort = NULL,
                          Dom = NULL, period = NULL,
                          k = c(20, 80), dataset = NULL,
                          checking = TRUE) {

   ## initializations
   if (checking) {

         Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                        ncols = 1, isnumeric = TRUE,
                        isvector = TRUE, grepls = "__")
         Ynrow <- length(Y)

         weights <- check_var(vars = weights, varn = "weights",
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
     }
   dataset <- NULL
   namesDom <- names(Dom)

   if (!is.null(period)) {
       if (!is.null(Dom)) { Dom <- data.table(period, Dom)
                 } else Dom <- period }

   # Percentiles by domain (if requested)

   N <- NULL
   if(!is.null(Dom)) {
        Dom_app <- do.call("paste", c(as.list(Dom), sep = "__"))
        q1 <- lapply(split(Dom[, .I], Dom_app), function(i) {
               Yind <- Y[i]
               weightsind <- weights[i]
               sortind <- sort[i]
               order <- if(is.null(sortind)) order(Yind) else order(Yind, sortind)
               Yind <- Yind[order]
               weightsind <- weightsind[order]  # also works if 'weights' is NULL
               percentile <- weightedQuantile(Yind, weightsind, probs = k / 100,
                                              sorted = FALSE, na.rm = FALSE)
               q <- data.table(Dom[i][1], t(percentile))})
        q <- rbindlist(q1)
        setnames(q, names(q)[ncol(Dom) + 1 : length(k)], paste0("x", k))
        if (!is.null(period) & !is.null(namesDom)) {
              q1 <-  q[, .N, keyby = namesDom][, N := NULL]
              q2 <- q[, .N, by = names(period)][, N := NULL]
              qrs <- rbindlist(lapply(1:nrow(q2), function(i) {
                                   data.table(q2[i], q1) }))
              qrs[, (c(paste0("x", k))) := 0]
              qrs <- rbind(q, qrs)
              q <- qrs[, lapply(.SD, sum), keyby = names(Dom), .SDcols = paste0("x", k)]
            }
         setkeyv(q, names(Dom))
     } else {  order <- if(is.null(sort)) order(Y) else order(Y, sort)
               Y <- Y[order]
               weights <- weights[order]  # also works if 'weights' is NULL
               percentile <- weightedQuantile(Y, weights, probs = k / 100,
                                              sorted = TRUE, na.rm = FALSE)
               q <- data.table(t(percentile))
               setnames(q, names(q)[1 : length(k)], paste0("x", k))
     }

   ## return results
   return(q[])
}

