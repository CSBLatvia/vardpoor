
lin.ratio <- function(Y, Z, weight) {
  # Y
  Y <- as.matrix(Y)
  n <- nrow(Y)
  m <- ncol(Y)
  if (any(is.na(Y))) stop("'Y' has unknown values")

  # Z
  Z <- as.matrix(Z)
  if (nrow(Z)!= n) stop("'Y' and 'Z' have different row count")
  if (ncol(Z)!= m) stop("'Y' and 'Z' have different column count")
  if (any(is.na(Z))) stop("'Z' has unknown values")

  # weight
  weight <- as.vector(weight)
  if (!is.numeric(weight)) stop("'weight' should be numerical")
  if (length(weight) != n) stop("'weight' length is not equal with 'Y' row count")
  if (any(is.na(weight))) stop("'weight' has unknown value")
 
  Z_nov <- colSums(Z * weight)
  Y_nov <- colSums(Y * weight)
  R_nov <- Y_nov / Z_nov
  U <- t((1 / Z_nov) * t(Y - t(R_nov * t(Z))))
  U
}
