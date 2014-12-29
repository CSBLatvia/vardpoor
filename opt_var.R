##################
##################
##################
library("data.table")
library("reshape2")

#H <- data.table(H=1:3)
#Yh <- data.table(Yh=10*1:3, Yh=10*4:6)
#S2h <- data.table(S2h=10*runif(3), S2h2=10*runif(3))
#n <- 100
#poph <- data.table(Nh=8*1:3)
#Rh <- data.table(Rh=rep(1,3))
#deffh <- data.table(deffh=rep(2,3), deffh2=rep(3,3))
#Dom <- data.table(dd=c(1,1,1))
#dataset <- NULL
#confidence = .95

#n - sample size
#pop - population size
#s2 - S^2 estimation
#RR - respondence level
#deff - design effect

#
#opt_var(Yh, H, S2h, n, poph, Rh, deffh, dataset = NULL)

opt_var <- function(H, S2h, n, poph, Rh=NULL, deffh=NULL, dataset = NULL) {
 
  ### Checking
  if(abs(n - round(n)) >= .Machine$double.eps || n < 0 ) stop("'n' must be a integer value greater than 0")  

  if(!is.null(dataset)) {
      dataset <- data.frame(dataset)
      if(!is.null(H)) {
          aH <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- as.data.frame(dataset[, aH], stringsAsFactors=FALSE)
                                names(H) <- aH }}

      if(!is.null(S2h)) {
          aS2h <- S2h
          if (min(S2h %in% names(dataset))!=1) stop("'S2h' does not exist in 'dataset'!")
          if (min(S2h %in% names(dataset))==1) {
                                S2h <- data.frame(dataset[, S2h], check.names=FALSE)
                                names(S2h) <- aS2h }
      }
      if(!is.null(poph)) {
          apoph<- poph  
          if (min(poph %in% names(dataset))!=1) stop("'poph' does not exist in 'dataset'!")
          if (min(poph %in% names(dataset))==1) {
                                poph <- as.data.frame(dataset[, apoph], stringsAsFactors=FALSE)
                                names(poph) <- apoph }}

      if(!is.null(Rh)) {
          aRh<- Rh  
          if (min(Rh %in% names(dataset))!=1) stop("'Rh' does not exist in 'dataset'!")
          if (min(Rh %in% names(dataset))==1) {
                                Rh <- as.data.frame(dataset[, aRh], stringsAsFactors=FALSE)
                                names(Rh) <- aRh }}

      if(!is.null(deffh)) {
          adeffh <- deffh  
          if (min(deffh %in% names(dataset))!=1) stop("'deffh' does not exist in 'dataset'!")
          if (min(deffh %in% names(dataset))==1) {
                                deffh <- data.frame(dataset[, adeffh])
                                names(deffh) <- adeffh }}
      }

  # S2h
  S2h <- data.table(S2h, check.names=TRUE)
  m <- ncol(S2h)
  if (any(is.na(S2h))) stop("'S2h' has unknown values")
  if (!all(sapply(S2h, is.numeric))) stop("'S2h' must be numeric values")
  if (is.null(names(S2h))) stop("'S2h' must be colnames")
  
  # H
  H <- data.table(H)
  if (nrow(H) != nrow(S2h)) stop("'H' length must be equal with 'S2h' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  
  # poph 
  poph <- data.frame(poph)
  if (nrow(poph) != nrow(S2h)) stop("'poph' must be equal with 'S2h' row count")
  if (ncol(poph) != 1) stop("'poph' must be vector or 1 column data.frame, matrix, data.table")
  poph <- poph[,1]
  if (!is.numeric(poph)) stop("'poph' must be numerical")
  if (any(is.na(poph))) stop("'poph' has unknown values") 

  # Rh 
  if (is.null(Rh)) Rh <- rep(1, nrow(S2h))
  Rh <- data.frame(Rh)
  if (nrow(Rh) != nrow(S2h)) stop("'Rh' must be equal with 'S2h' row count")
  if (ncol(Rh) != 1) stop("'Rh' must be vector or 1 column data.frame, matrix, data.table")
  Rh <- Rh[, 1]
  if (!is.numeric(Rh)) stop("'Rh' must be numerical")
  if (any(is.na(Rh))) stop("'Rh' has unknown values") 
  
  if (!is.null(deffh)) { 
          deffh <- data.table(deffh, check.names=TRUE)
          if (nrow(deffh) != nrow(S2h)) stop("'deffh' length must be equal with 'S2h' row count")
          if (ncol(deffh) != m) stop("'deffh' and 'Yh' must be equal column count")
          if (any(is.na(deffh))) stop("'deffh' has unknown values")
          if (!all(sapply(deffh, is.numeric))) stop("'deffh' must be numeric values")
          if (is.null(names(deffh))) stop("'deffh' must be colnames")
   }

  Rh <- melt(data.table(H, Rh), id=c(names(H)))
  Rh[, variable:=NULL]
  setnames(Rh, "value", "Rh")
  setkeyv(Rh, names(H))

  poph <- melt(data.table(H, poph), id=c(names(H)))
  poph[, variable:=NULL]
  setnames(poph, "value", "poph")
  setkeyv(poph, names(H))

  resulth <- merge(Rh, poph, all=T)

  S2h <- melt(data.table(H, S2h), id=c(names(H)))
  setnames(S2h, "value", "S2h")
  setkeyv(S2h, c(names(H), "variable"))
  resulth <- merge(S2h, resulth, all=T)
  setkeyv(resulth, c(names(H), "variable"))

  if (!is.null(deffh)) { 
      setnames(deffh, names(deffh), names(S2h))
      deffh <- melt(data.table(H, deffh), id=c(names(H)))
      setnames(deffh, "value", "deffh")
      setkeyv(deffh, c(names(H), "variable"))
      resulth <- merge(deffh, resulth, all=T)
  } 
  if (is.null(deffh)) resulth[, deffh:=1]

  resulth[, pnh:= poph *  S2h  / sqrt(Rh)]
  resulth[, nh:=n * pnh /sum(pnh), keyby="variable"]
  return(resulth)
}

