
residual_est <- function (Y, X, weight, q) {
 
  # Y
  Y <- as.data.frame.matrix(data.table(Y, check.names = TRUE))
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric")
  if(any(is.na(Y))) print("'Residual_est': 'Ys' has missing values", call. = FALSE)
 
  # X
  X <- data.table(X, check.names = TRUE)
  if (nrow(X) != n) stop("'X' and 'Y' must be equal row count")
  if (!all(sapply(X, is.numeric))) stop("'X' must be numeric")
  X1 <- X[, lapply(.SD, function(x) sum(!is.na(x)))]
  if (!all(X1 == n)) stop("X has missing values")
  X <- as.matrix(X)

  # weight
  weight <- data.frame(weight)
  if (nrow(weight) != n) stop("'weight' length must be equal with 'Y' row count!")
  if (ncol(weight) != 1) stop("'weight' must be a vector or 1 column data.frame, matrix, data.table")
  weight <- weight[, 1]
  if (!is.numeric(weight)) stop("'weight' must be numeric")
  if(any(is.na(weight))) stop("'weight' has missing values")

  # q
  q <- data.frame(q)
  if (nrow(q) != n) stop("'q' length must be equal with Y' row count!")
  if (ncol(q) != 1) stop("'q' must be a vector or 1 column data.frame, matrix, data.table")
  q <- q[, 1]
  if (!is.numeric(q)) stop("'q' must be numeric")
  if(any(is.na(q))) stop("'q' has missing values")  

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

