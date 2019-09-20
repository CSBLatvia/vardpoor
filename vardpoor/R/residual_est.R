
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

