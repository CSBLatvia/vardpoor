#'The estimation of the simple random sampling.
#'
#' @description Computes the estimation of the simple random sampling.
#'
#' @param Y The variables of interest.
#' @param w Weight variable. One dimensional object convertible to one-column \code{data.frame}.
#' 
#' @return  A list with objects are returned by the function:
#' \itemize{
#'  \item \code{S2p} - a \code{data.table} containing the values of the variance estimation of the population.
#'  \item \code{varsrs} - a \code{data.table} containing the values of the variance estimation of the simple random sampling.
#'  }
#'  
#' @references
#'Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://wayback.archive-it.org/12090/20231228140953/https://cros-legacy.ec.europa.eu/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en}
#'
#' @seealso \code{\link{vardom}},
#'          \code{\link{vardomh}},
#'          \code{\link{varpoord}}
#'          
#' @keywords variance
#' 
#' @examples
#' Ys <- matrix(rchisq(10, 3), 10, 1)
#' ws <- c(rep(2, 5), rep(3, 5))
#' var_srs(Ys, ws)
#'
#' @import data.table
#' @import surveyplanning
#' 
#' @export var_srs



 
var_srs <- function(Y, w = rep(1, length(Y))){
 
  ### Checking
  # Y
  Y <- data.table(Y, check.names = TRUE)
  n <- nrow(Y)   
  if (anyNA(Y)) print("'Y' has missing values")
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numerical")
    
  # w 
  w <- data.frame(w)
  if (anyNA(w)) stop("'w' has missing values")
  if (nrow(w) != n) stop("'w' must be equal with 'Y' row count")
  if (ncol(w) != 1) stop("'w' must be vector or 1 column data.frame, matrix, data.table")
  w <- w[, 1]
  if (!is.numeric(w)) stop("'w' must be numeric")
    
  ### Calculation

  # N
  Nn <- sum(w)
  konst <- Nn^2 * (1 - n / Nn) / n
  s2p <- Y[, lapply(.SD, function(x) s2(x, w))]

  varsrs <- konst * s2p
  return(list(S2p = s2p, varsrs = varsrs))
}

