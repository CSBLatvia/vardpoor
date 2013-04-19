residual_est <- function (Ys, Xs, weight, q) {
  
  # Ys
  Ys <- as.matrix(Ys)
  n <- nrow(Ys)
  m <- ncol(Ys)
  if(any(is.na(Ys))) print("'Residual_est': 'Ys' has unknown values", call. = FALSE)
 
  # Xs
  Xs <- as.matrix(Xs)
  if(nrow(Xs) !=n) stop("Xs and Ys have different row count!")
  if(any(is.na(Xs))) stop("Xs has unknown values")

  # weight
  weight <- as.vector(weight)
  if (length(weight) != n) stop("'weight' length îs not equal with 'Ys' row count!")
  if(any(is.na(weight))) stop("'weight' has unknown values")

  # q
  q <- as.vector(q)
  if (length(q) != n) stop("'q' length is not equal with 'Ys' row count!")
  if(any(is.na(q))) stop("'q' has unknown values")  

  ee <- as.data.frame(matrix(NA, n, m))
  ws <- weight * q
  B <- t(Xs * ws)
  
  for (i in 1:ncol(Ys)) {
    beta <- ginv(B %*% Xs) %*% B %*% Ys[,i]
    ee[,i] <- Ys[,i] - Xs %*% beta }
 
  colnames(ee) <- colnames(Ys)
  ee
}
