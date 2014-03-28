
residual_est <- function (Y, X, weight, q, period=NULL, ind_gr=NULL) {
 
  # Y
  Y <- as.data.frame.matrix(data.table(Y, check.names=TRUE))
  n <- nrow(Y)
  m <- ncol(Y)
  if(any(is.na(Y))) print("'Residual_est': 'Ys' has unknown values", call. = FALSE)
 
  # ind_gr
  if (is.null(ind_gr)) ind_gr <- rep.int(1, n)
  ind_gr <- data.table(ind_gr)
  if (nrow(ind_gr) != n) stop("'ind_gr' length must be equal with Y' row count!")
  if (ncol(ind_gr) != 1) stop("'ind_gr' must be a vector or 1 column data.frame, matrix, data.table")
  if(any(is.na(ind_gr))) stop("'ind_gr' has unknown values")  

  # period     
  if (!is.null(period)) {
       period <- data.table(period)
       if (any(duplicated(names(period)))) 
                 stop("'period' are duplicate column names: ", 
                      paste(names(period)[duplicated(names(period))], collapse = ","))
       if (nrow(period) != n) stop("'period' must be the same length as 'inc'")
       if(any(is.na(period))) stop("'period' has unknown values")  
   }   

  if (!is.null(period)) ind_gr <- data.table(period, ind_gr, check.names=TRUE)

  # X
  X <- as.matrix(X)
  if (nrow(X) != n) stop("'X' and 'Y' must be equal row count")
  X1 <- data.table(X, check.names=T)
  nX1 <- names(X1)
  X2 <- data.table(ind_gr, X1)
  X1 <- X2[, .N, keyby=names(ind_gr)][[ncol(ind_gr)+1]]
  X2 <- X2[,lapply(.SD, function(x) sum(!is.na(x))), keyby=names(ind_gr), .SDcols=nX1]
  X2 <- X2[, !(names(X2) %in% names(ind_gr)), with=F]
  if (!all(X2==0 | X1==X2)) stop("X has unknown values")
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


  indgr <- data.table(unique(ind_gr))
  setnames(indgr, names(indgr), names(ind_gr))

  ee <- as.data.frame(matrix(NA, n, m))
  ws <- weight * q

  for(k in 1:nrow(indgr)) {
      D <- indgr[k,][rep(1,n),]
      ind <- (rowSums(ind_gr == D) == ncol(ind_gr))
      kolonnas <- colSums(!is.na(X[ind,]))==nrow(X[ind,])

      B <- t(X[ind, kolonnas] * ws[ind])

      for (i in 1:ncol(Y)) {
          beta <- ginv(B %*% X[ind, kolonnas]) %*% B %*% Y[ind, i]
          ee[ind, i] <- Y[ind, i] - X[ind, kolonnas] %*% beta
         }
   }
  colnames(ee) <- colnames(Y)

  return(ee)
}

