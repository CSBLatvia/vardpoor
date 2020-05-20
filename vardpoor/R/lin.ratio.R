#' Linearization of the ratio estimator
#' 
#' @description Computes linearized variable for the ratio estimator.
#' 
#' 
#' @param Y Matrix of numerator variables. Any object convertible to \code{data.table} with numeric values, \code{NA} values are not allowed.
#' @param Z Matrix of denominator variables. Any object convertible to \code{data.table} with numeric values, \code{NA} values are not allowed.
#' @param weight Weight variable. One dimensional object convertible to one-column \code{data.table}.
#' @param Dom Optional variables used to define population domains. If supplied, the linearized variables are computed for each domain. An object convertible to \code{data.table}.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param percentratio Positive integer value. All linearized variables are multiplied with percentratio value, by default - 1.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#'
#' @return The function returns the \code{data.table} of the linearized variables for the ratio estimator.
#' 
#' @references
#' Carl-Erik Sarndal, Bengt Swensson, Jan Wretman. Model Assisted Survey Sampling. Springer-Verlag, 1992, p.178.
#'
#' @keywords survey
#' 
#' @examples
#' library("data.table")
#' Y <- data.table(Y = rchisq(10, 3))
#' Z <- data.table(Z = rchisq(10, 3))
#' weights <- rep(2, 10)
#' data.table(Y, Z, weights,
#'            V1 = lin.ratio(Y, Z, weights, percentratio = 1),
#'            V10 = lin.ratio(Y, Z, weights, percentratio = 10),
#'            V100 = lin.ratio(Y, Z, weights, percentratio = 100))
#'
#' @seealso \code{\link{domain}}, 
#'          \code{\link{vardom}},
#'          \code{\link{vardomh}},
#'          \code{\link{vardcros}},
#'          \code{\link{vardchanges}},
#'          \code{\link{vardannual}}
#'
#' @import data.table
#' @export lin.ratio


lin.ratio <- function(Y, Z, weight, Dom = NULL, dataset = NULL, percentratio = 1, checking = TRUE) {

  if (checking) {
       percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger") 

       Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                      check.names = TRUE, isnumeric = TRUE, grepls = "__")
       Ynrow <- nrow(Y)
       Yncol <- ncol(Y)

       Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                      check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                      isnumeric = TRUE, mustbedefined = FALSE)

       weight <- check_var(vars = weight, varn = "weight",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE)
   }

  if (!is.null(Dom)) Yd <- domain(Y, Dom) else Yd <- Y
  if (!is.null(Dom)) Zd <- domain(Z, Dom) else Zd <- Z

  Y_est <- colSums(Yd * weight)
  Z_est <- colSums(Zd * weight)
  R_est <- Y_est / Z_est

  percentratio <- as.integer(percentratio)
  U <- percentratio * t((1 / Z_est) * t(Yd - t(R_est * t(Zd))))
  return(data.table(U))
}
