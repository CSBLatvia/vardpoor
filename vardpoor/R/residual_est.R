
residual_est <- function (Y, X, weight, q) {
 
  # Y
  Y <- data.table(Y, check.names = TRUE)
  if(anyNA(Y)) print("'Residual_est': 'Ys' has missing values", call. = FALSE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric", call. = FALSE)
  Y <- as.data.frame.matrix(Y)
 
  # X
  X <- data.table(X, check.names = TRUE)
  if(anyNA(X)) stop("'X' has missing values", call. = FALSE)
  if (nrow(X) != n) stop("'X' and 'Y' must be equal row count", call. = FALSE)
  X <- as.matrix(X)
  if (is.numeric(class(X[, 1]))) stop("'X' must be numeric", call. = FALSE)

  # weight
  weight <- data.frame(weight)
  if(anyNA(weight)) stop("'weight' has missing values", call. = FALSE)
  if (nrow(weight) != n) stop("'weight' length must be equal with 'Y' row count!", call. = FALSE)
  if (ncol(weight) != 1) stop("'weight' must be a vector or 1 column data.frame, matrix, data.table", call. = FALSE)
  weight <- weight[, 1]
  if (!is.numeric(weight)) stop("'weight' must be numeric", call. = FALSE)

  # q
  q <- data.frame(q)
  if(anyNA(q)) stop("'q' has missing values", call. = FALSE)
  if (nrow(q) != n) stop("'q' length must be equal with Y' row count!", call. = FALSE)
  if (ncol(q) != 1) stop("'q' must be a vector or 1 column data.frame, matrix, data.table", call. = FALSE)
  q <- q[, 1]
  if (!is.numeric(q)) stop("'q' must be numeric", call. = FALSE)

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

