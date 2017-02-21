
residual_est <- function (Y, X, weight, q) {
 
  # Y
  Y <- data.table(Y, check.names = TRUE)
  if(anyNA(Y)) print("'Residual_est': 'Ys' has missing values", call. = FALSE)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric", call. = FALSE)
  n <- nrow(Y)
  m <- ncol(Y)
  Y <- as.data.frame.matrix(Y)
 
  # X
  X <- check_var(vars = X, varn = "X", dataset = NULL,
                 check.names = TRUE, ncols = 0, Yncol = 0,
                 Ynrow = n, Xnrow = 0, isnumeric = TRUE,
                 grepls = "__")
  X <- as.matrix(X)

  weight <- check_var(vars = weight, varn = "weight", dataset = NULL,
                       ncols = 1, Yncol = 0, Ynrow = Ynrow,
                       isnumeric = TRUE, asvector = TRUE)

  q <- check_var(vars = q, varn = "q", dataset = NULL, ncols = 1,
                 Yncol = 0, Ynrow = Ynrow, isnumeric = TRUE,
                 asvector = TRUE)

  ee <- as.data.frame(matrix(NA, n, m))
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

