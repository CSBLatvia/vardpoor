#' Residual estimation of calibration
#'
#' @description Computes the estimation residuals of calibration.
#'
#' @param Y Matrix of the variable of interest.
#' @param X Matrix of the auxiliary variables for the calibration estimator. This is the matrix of the sample calibration variables.
#' @param weight Weight variable. One dimensional object convertible to one-column \code{data.frame}.
#' @param q Variable of the positive values accounting for heteroscedasticity. One dimensional object convertible to one-column \code{data.frame}.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#'
#' @return A list with objects are returned by the function:
#' \itemize{
#'  \item \code{residuals} - a numeric \code{data.table} containing the estimated residuals of calibration.
#'  \item \code{betas} - a numeric \code{data.table} containing the estimated coeffients of calibration.
#' }
#' 
#' @details
#' The function implements the following estimator:
#'    \deqn{e_k=Y_k-X_k^{'}B }
#'where
#'\deqn{\hat{B} = \left(\sum_{s} weight_k q_k X_k X^{'}_{k} \right)^{-1} \left(\sum_{s} weight_k q_k X_k Y_k \right)}.
#'
#' @references
#'Sixten Lundstrom and Carl-Erik Sarndal. Estimation in the presence of Nonresponse and Frame Imperfections. Statistics Sweden, 2001, p. 43-44.
#'
#' @seealso \code{\link{domain}},   \code{\link{lin.ratio}},    \code{\link{linarpr}},
#'          \code{\link{linarpt}},  \code{\link{lingini}},      \code{\link{lingini2}},
#'          \code{\link{lingpg}},   \code{\link{linpoormed}},   \code{\link{linqsr}},
#'          \code{\link{linrmpg}},  \code{\link{vardom}},       \code{\link{vardomh}},
#'          \code{\link{varpoord}}, \code{\link{variance_est}}, \code{\link{variance_othstr}}
#'
#' @keywords survey
#' 
#' 
#' @examples
#' Y <- matrix(rchisq(10, 3), 10, 1)
#' X <- matrix(rchisq(20, 3), 10, 2)
#' w <- rep(2, 10)
#' q <- rep(1, 10)
#' residual_est(Y, X, w, q)
#' 
#' ### Test2
#' Y <- matrix(rchisq(10, 3), 10, 1)
#' X <- matrix(c(rchisq(10, 2), rchisq(10, 2) + 10), 10, 2)
#' w <- rep(2, 10)
#' q <- rep(1, 10)
#' residual_est(Y, X, w, q)
#' as.matrix(lm(Y ~ X - 1, weights = w * q)$residuals)
#' 
#' @export residual_est
#' 
#' @import data.table
#' @import MASS
#' @import stats
#' @import utils


residual_est <- function (Y, X, weight, q, dataset = NULL, checking = TRUE) {

  if (checking) {
         Y <- check_var(vars = Y, varn = "Y_residual", dataset = dataset,
                        check.names = TRUE, isnumeric = TRUE)}
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)
  Y <- as.data.frame.matrix(Y)

  if (checking) {
         X <- check_var(vars = X, varn = "X", dataset = dataset,
                        check.names = TRUE, Ynrow = Ynrow, 
                        isnumeric = TRUE, grepls = "__")

         weight <- check_var(vars = weight, varn = "weight", dataset = dataset,
                             ncols = 1, Ynrow = Ynrow, isnumeric = TRUE,
                             isvector = TRUE)

         q <- check_var(vars = q, varn = "q", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, isnumeric = TRUE,
                        isvector = TRUE)
    }
  X <- as.matrix(X)

  ee <- as.data.frame(matrix(NA, Ynrow, Yncol))
  ws <- weight * q

  B <- t(X * ws)
  matr <- ginv(B %*% X) %*% B
  B <- ws <- q <- weight <- NULL

  betas <- c()
  for (i in 1:ncol(Y)) {
          beta <- matr %*% Y[, i]
          ee[, i] <- Y[, i] - X %*% beta
          betas <- rbind(betas, data.frame(Yname = names(Y)[i], t(beta)))   
         }
  names(ee) <- names(Y)
  Y <- X <- beta <- NULL

  return(list(residuals = data.table(ee),
                  betas = data.table(betas)))
}

