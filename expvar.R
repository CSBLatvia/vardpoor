
library("data.table")
library("reshape2")

#H <- data.table(H=1:3)
#Yh <- data.table(Yh=10*1:3, Yh=10*4:6)
#S2h <- data.table(S2h=10*runif(3), S2h2=10*runif(3))
#nh <- data.table(nh=4*1:3)
#poph <- data.table(poph=8*1:3)
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
#expvar(Yh, H, S2g, nh, poph, Rh, deffh, Dom=NULL, dataset = NULL, confidence = .95)

expvar <- function(Yh, H, S2h, nh, poph, Rh=NULL, deffh=NULL, Dom=NULL,
                   dataset = NULL, confidence = .95) {
 
  ### Checking
  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {
      dataset <- data.frame(dataset)
      aYh <- Yh
      if (min(Yh %in% names(dataset))!=1) stop("'Yh' does not exist in 'dataset'!")
      if (min(Yh %in% names(dataset))==1) {
                            Yh <- data.frame(dataset[, aYh], check.names=FALSE)
                            names(Yh) <- aYh }
   

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
      if(!is.null(nh)) {
          anh <- nh  
          if (min(nh %in% names(dataset))!=1) stop("'nh' does not exist in 'dataset'!")
          if (min(nh %in% names(dataset))==1) {
                                nh <- as.data.frame(dataset[, anh], stringsAsFactors=FALSE)
                                names(nh) <- anh }}
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

      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom2], stringsAsFactors=FALSE) 
                  names(Dom) <- Dom2 } }
      }

  # Yh
  Yh <- data.table(Yh, check.names=TRUE)
  n <- nrow(Yh)
  m <- ncol(Yh)
  if (any(is.na(Yh))) stop("'Yh' has unknown values")
  if (!all(sapply(Yh, is.numeric))) stop("'Yh' must be all numeric values")
  if (is.null(names(Yh))) stop("'Yh' must be colnames")
  Yh <- data.table(sapply(Yh, as.numeric))
  
  S2h <- data.table(S2h, check.names=TRUE)
  if (nrow(S2h) != n) stop("'S2h' length must be equal with 'Yh' row count")
  if (ncol(S2h) != m) stop("'S2h' and 'Yh' must be equal column count")
  if (any(is.na(S2h))) stop("'S2h' has unknown values")
  if (!all(sapply(S2h, is.numeric))) stop("'S2h' must be numeric values")
  if (is.null(names(S2h))) stop("'S2h' must be colnames")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Yh' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  # nh 
  nh <- data.frame(nh)
  if (nrow(nh) != n) stop("'nh' must be equal with 'Yh' row count")
  if (ncol(nh) != 1) stop("'nh' must be vector or 1 column data.frame, matrix, data.table")
  nh <- nh[,1]
  if (!is.numeric(nh)) stop("'nh' must be numerical")
  if (any(is.na(nh))) stop("'nh' has unknown values") 

  # poph 
  poph <- data.frame(poph)
  if (nrow(poph) != n) stop("'poph' must be equal with 'Yh' row count")
  if (ncol(poph) != 1) stop("'poph' must be vector or 1 column data.frame, matrix, data.table")
  poph <- poph[,1]
  if (!is.numeric(poph)) stop("'poph' must be numerical")
  if (any(is.na(poph))) stop("'poph' has unknown values") 

  # Rh 
  if (is.null(Rh)) Rh <- rep(1, n)
  Rh <- data.frame(Rh)
  if (nrow(Rh) != n) stop("'Rh' must be equal with 'Yh' row count")
  if (ncol(Rh) != 1) stop("'Rh' must be vector or 1 column data.frame, matrix, data.table")
  Rh <- Rh[, 1]
  if (!is.numeric(Rh)) stop("'Rh' must be numerical")
  if (any(is.na(Rh))) stop("'Rh' has unknown values") 
  
  if (!is.null(deffh)) { 
          deffh <- data.table(deffh, check.names=TRUE)
          if (nrow(deffh) != n) stop("'deffh' length must be equal with 'Yh' row count")
          if (ncol(deffh) != m) stop("'deffh' and 'Yh' must be equal column count")
          if (any(is.na(deffh))) stop("'deffh' has unknown values")
          if (!all(sapply(deffh, is.numeric))) stop("'deffh' must be numeric values")
          if (is.null(names(deffh))) stop("'deffh' must be colnames")
   }

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
    Dom <- Dom[, lapply(.SD, as.character), .SDcols = names(Dom)]
  }

  nh <- data.table(melt(data.table(H, nh), id=c(names(H))))
  nh[, variable:=NULL]
  setnames(nh, "value", "nh")
  setkeyv(nh, names(H))

  Rh <- data.table(melt(data.table(H, Rh), id=c(names(H))))
  Rh[, variable:=NULL]
  setnames(Rh, "value", "Rh")
  setkeyv(Rh, names(H))
  resulth <- merge(nh, Rh, all=T)

  poph <- data.table(melt(data.table(H, poph), id=c(names(H))))
  poph[, variable:=NULL]
  setnames(poph, "value", "poph")
  setkeyv(poph, names(H))

  resulth <- merge(resulth, poph, all=T)

  setnames(S2h, names(S2h), names(Yh))
  S2h <- data.table(melt(data.table(H, S2h), id=c(names(H))))
  setnames(S2h, "value", "S2h")
  setkeyv(S2h, c(names(H), "variable"))
  resulth <- merge(S2h, resulth, all=T)
  setkeyv(resulth, c(names(H), "variable"))

  if (!is.null(deffh)) { 
      setnames(deffh, names(deffh), names(Yh))
      deffh <- data.table(melt(data.table(H, deffh), id=c(names(H))))
      setnames(deffh, "value", "deffh")
      setkeyv(deffh, c(names(H), "variable"))
      resulth <- merge(deffh, resulth, all=T)
  } 
  if (is.null(deffh)) resulth[, deffh:=1]
  domH <- H
  if (!is.null(Dom)) domH <- data.table(Dom, domH)
  Yh <- data.table(melt(data.table(domH, Yh), id=c(names(domH))))
  setnames(Yh, "value", "estim")
  setkeyv(Yh, c(names(H), "variable"))
  resulth <- merge(Yh, resulth, all=T)

  tsad <- qnorm(0.5*(1+confidence))
  resulth[, nrh:=round(nh * Rh)]
  resulth[nrh < 1, nrh:=1]
  resulth[, var:=poph^2 * (1-nrh/poph)/ nrh * S2h  * deffh]
  resulth[!is.nan(var), se:=sqrt(var)]
  resulth[is.nan(var) | is.na(var), se:=NA]
  resulth[, cv:=100*se/estim]
  
  domH <- "variable"
  if (!is.null(Dom)) domH <- c(names(Dom), domH)
  result <- resulth[,lapply(.SD, sum,na.rm=TRUE), keyby=domH, .SDcols=c("estim", "var")]
  result[, se:=sqrt(var)]
  result[, cv:=100*se/estim]

  list(resultH = resulth,
       result = result)
}

