
var_srs <- function(Y, w = rep(1, length(Y))){
 
  ### Checking
  # Y
  Y <- data.table(Y, check.names=TRUE)
  n <- nrow(Y)   
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numerical")
  if (any(is.na(Y))) print("'Y' has missing values")
  if (is.null(colnames(Y))) stop("'Y' must have column names")
    
  # w 
  w <- data.frame(w)
  if (nrow(w) != n) stop("'w' must be equal with 'Y' row count")
  if (ncol(w) != 1) stop("'w' must be vector or 1 column data.frame, matrix, data.table")
  w <- w[, 1]
  if (!is.numeric(w)) stop("'w' must be numeric")
  if (any(is.na(w))) stop("'w' has missing values")
    
  ### Calculation

  # N
  Nn <- sum(w)
  konst <- Nn^2 * (1 - n/Nn) / n
  s2p <- Y[, lapply(.SD, function(x) s2(x, w))]

  varsrs <- konst * s2p
  return(list(S2p = s2p, varsrs = varsrs))
}

