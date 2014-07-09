
residual_est <- function (Y, X, weight, q) {
 
  # Y
  Y <- as.data.frame.matrix(data.table(Y, check.names=TRUE))
  n <- nrow(Y)
  m <- ncol(Y)
  if(any(is.na(Y))) print("'Residual_est': 'Ys' has unknown values", call. = FALSE)
 

  # X
  X <- as.matrix(X)
  if (nrow(X) != n) stop("'X' and 'Y' must be equal row count")
  if (is.na(X)) stop("X has unknown values")
  nX1 <- X1 <- X2 <- NULL

  # weight
  weight <- data.frame(weight)
  if (nrow(weight) != n) stop("'weight' length must be equal with 'Y' row count!")
  if (ncol(weight) != 1) stop("'weight' must be vector or 1 column data.frame, matrix, data.table")
  weight <- weight[, 1]
  if (!is.numeric(weight)) stop("'weight' must be numerical")
  if(any(is.na(weight))) stop("'weight' has unknown values")

  # q
  q <- data.frame(q)
  if (nrow(q) != n) stop("'q' length must be equal with Y' row count!")
  if (ncol(q) != 1) stop("'q' must be a vector or 1 column data.frame, matrix, data.table")
  q <- q[, 1]
  if (!is.numeric(q)) stop("'q' must be numerical")
  if(any(is.na(q))) stop("'q' has unknown values")  

  ee <- as.data.frame(matrix(NA, n, m))
  ws <- weight * q

  B <- t(X[, kolonnas] * ws)

  for (i in 1:ncol(Y)) {
          beta <- ginv(B %*% X[, kolonnas]) %*% B %*% Y[, i]
          ee[ind, i] <- Y[, i] - X[, kolonnas] %*% beta
         }

  colnames(ee) <- colnames(Y)

  return(ee)
}

