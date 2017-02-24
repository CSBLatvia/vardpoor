
residual_est <- function (Y, X, weight, q, dataset = NULL) {
 
  # Y
  Y <- check_var(vars = Y, varn = "Y_residual", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE)
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)
  Y <- as.data.frame.matrix(Y)
 
  # X
  X <- check_var(vars = X, varn = "X", dataset = dataset,
                 check.names = TRUE, ncols = 0, Yncol = 0,
                 Ynrow = Ynrow, Xnrow = 0, isnumeric = TRUE,
                 grepls = "__")
  X <- as.matrix(X)

  weight <- check_var(vars = weight, varn = "weight", dataset = dataset,
                       ncols = 1, Yncol = 0, Ynrow = Ynrow,
                       isnumeric = TRUE, isvector = TRUE)

  q <- check_var(vars = q, varn = "q", dataset = dataset,
                 ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 isnumeric = TRUE, isvector = TRUE)

  ee <- as.data.frame(matrix(NA, Ynrow, Yncol))
  ws <- weight * q
 
  B <- t(X * ws)
  matr <- ginv(B %*% X) %*% B
  B <- ws <- q <- weight <- NULL

  for (i in 1:ncol(Y)) {
          beta <- matr %*% Y[, i]
          ee[, i] <- Y[, i] - X %*% beta
         }
  names(ee) <- names(Y)
  Y <- X <- beta <- NULL

  return(ee)
}

