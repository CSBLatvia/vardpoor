
library(data.table)
Y <- rchisq(10, 3)
w_final <- rep(2, 10)
PSU <- 1:length(Y)
H <- rep("Strata_1", 10)
id_sort <- 1:10
dataset=NULL
period=rep(1,10)
N_h=NULL
fh_zero=FALSE
PSU_level=TRUE

#variance_sys_est <- function(Y, H, PSU, w_final, N_h=NULL, fh_zero=FALSE, id_sort=NULL, period=NULL, dataset=NULL) {


  ### Checking

    if (!is.logical(fh_zero)) stop("'fh_zero' must be the logical value")
    if (!is.logical(PSU_level)) stop("'PSU_level' must be the logical value")

    if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset))!=1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset))==1) Y <- dataset[, Y, with=FALSE] 
      if(!is.null(H)) {
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) H <- dataset[, H, with=FALSE] }
      if(!is.null(PSU)) {
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) PSU <- dataset[, PSU, with=FALSE] }
      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset))!=1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset))==1) w_final <- dataset[, w_final, with=FALSE] }
       if (!is.null(period)) {
            if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
            if (min(period %in% names(dataset))==1) period <- dataset[, period, with=FALSE] }
       if (!is.null(id_sort)) {
            if (min(id_sort %in% names(dataset))!=1) stop("'id_sort' does not exist in 'dataset'!")
            if (min(id_sort %in% names(dataset))==1) id_sort <- dataset[, id_sort, with=FALSE] }
      }

  # Y
  Y <- data.table(Y, check.names=TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric values")
  if (any(is.na(Y))) print("'Y' has unknown values")
  if (is.null(names(Y))) stop("'Y' must be the column names")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")
  H[, (names(H)):=lapply(.SD, as.character)]
  
  # PSU
  PSU <- data.table(PSU)
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  PSU[, (names(PSU)):=lapply(.SD, as.character)]
  
  # w_final
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[, 1]
  if (!is.numeric(w_final)) stop("'w_final' must be numerical")
  if (any(is.na(w_final))) stop("'w_final' has unknown values")

  # id_sort
  if (is.null(id_sort)) id_sort <- 1:n
  id_sort <- data.frame(id_sort)
  if (nrow(id_sort) != n) stop("'id_final' must be equal with 'Y' row count")
  if (ncol(id_sort) != 1) stop("'id_final' must be vector or 1 column data.frame, matrix, data.table")
  id_sort <- id_sort[, 1]
  if (!is.numeric(id_sort)) stop("'id_sort' must be numerical")
  if (any(is.na(id_sort))) stop("'id_sort' has unknown values")

  # period     
  if (!is.null(period)) {
       period <- data.table(period)
       if (any(duplicated(names(period)))) 
                 stop("'period' are duplicate column names: ", 
                      paste(names(period)[duplicated(names(period))], collapse = ","))
       if (nrow(period) != n) stop("'period' must be the same length as 'inc'")
       if(any(is.na(period))) stop("'period' has unknown values")  
  }   
  np <- sum(ncol(period))
  
  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (ncol(N_h) != np+2) stop(paste0("'N_h' should be ",toString(np+2)," columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numerical")
      if (any(is.na(N_h))) stop("'N_h' has unknown values") 
      if (is.null(names(N_h))) stop("'N_h' must be colnames")
      if (all(names(H) %in% names(N_h))) {N_h[, (names(H)):=lapply(.SD, as.character), .SDcols=names(H)]
             } else stop("All strata titles of 'H' have not in 'N_h'")
      if (is.null(period)) {
             if (names(H) != names(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
             if (any(is.na(merge(unique(H), N_h, by=names(H), all.x=TRUE)))) stop("'N_h' is not defined for all stratas")
             if (any(duplicated(N_h[, head(names(N_h),-1), with=FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(names(pH) != names(N_h)[c(1:(1+np))])) stop("Strata titles for 'period' with 'H' and 'N_h' is not equal")
                nperH <- names(period)
                if (pH[, class(get(nperH))]!=N_h[, class(get(nperH))]) 
                                                       stop("Period class for 'period' and 'N_h' is not equal ")
                if (any(is.na(merge(unique(pH), N_h, by=names(pH), all.x=TRUE)))) stop("'N_h' is not defined for all stratas and periods")
                if (any(duplicated(N_h[, head(names(N_h),-1), with=FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                }
    setnames(N_h, names(N_h)[ncol(N_h)], "N_h")
    setkeyv(N_h, names(N_h)[c(1:(1+np))])
  } else {
    Nh <- data.table(H, w_final)
    if (!is.null(period)) Nh <- data.table(period, Nh)
    N_h <- Nh[, .(N_h = sum(w_final, na.rm=TRUE)), keyby=c(names(Nh)[1:(1+np)])]
  }
  pH <- NULL  


  ### Calculation
  

  # z_hi
  .SD <- .N <- NULL
  hpY <- data.table(H, id_sort=id_sort, PSU, w_final, Y*w_final)
  if (!is.null(period)) hpY <- data.table(period, hpY)

  sarak <- names(hpY)[c(1:(1+np), 3+np)]
  namY <- names(Y)

  setkeyv(hpY, names(hpY)[c(1:(2+np))]) 

  # n_h
  n_h <- data.table(hpY[, c(1:(1+np),4+np), with=FALSE])
  n_h <- n_h[, .(n_h=.N, psh=sum(1/w_final)), keyby=c(names(n_h)[1:(1+np)])]
  n_h[, pshk:=psh/n_h]
  n_h[, nhc:=ifelse(n_h>2,1/(2*n_h*(n_h-1)),NA)]


  hpY[, (paste0("lag_", names(Y))) := lapply(.SD, function(x) shift(x, 1)),
                            by=c(names(hpY)[1:(1+np)]), .SDcols=namY]

  laY <- paste0("lag_", names(Y)[1])
  hpY2 <- hpY[!is.na(get(laY))]

  hpY2 <- hpY2[, lapply(nY, function(e) 
                               sum((get(e)-get(paste0("lag_",e)))^2)),
                               by=c(names(hpY2)[1:(1+np)])]
  setnames(hpY2, names(hpY2)[(2+np):ncol(hpY2)], names(Y))


  # f_h
  F_h <- merge(N_h, n_h, by = names(hpY)[c(1:(1+np))], sort=TRUE)
  F_h[, f_h:=n_h/N_h]
  f_h <- F_h[,"pshk", with=FALSE]

  # var_h
  var_h <- matrix((1 - f_h*(1-fh_zero)) * n_h$nhc) * hpY2[, names(Y), with=FALSE]
  var_h2 <- copy(var_h)
  if (np>0) var_h2 <- data.table(hpY2[, c(1:np), with=FALSE], var_h)

  # Variance_est 

  if (np==0) {variance_sys_est <- data.table(t(colSums(var_h, na.rm=TRUE)))
           } else   variance_sys_est <- var_h2[, lapply(.SD, sum, na.rm=TRUE), 
                                        keyby = c(names(var_h2)[c(1:np)]),
                                       .SDcols = names(var_h)]


  return(variance_sys_est)
}



