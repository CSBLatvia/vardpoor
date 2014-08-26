vardchanges <- function(Y1, H1, PSU1, w_final1, id1,
                     Dom1 = NULL, Z1 = NULL,
                     country1, period1,
                     dataset1 = NULL,
                     Y2, H2, PSU2, w_final2, id2,
                     Dom2 = NULL, Z2 = NULL,
                     country2, period2,
                     dataset2 = NULL,
                     confidence=0.95) {
 
  ### Checking
  linratio = FALSE

  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset1)) {
      dataset1 <- data.frame(dataset1)
      aY <- Y1
      if (min(Y1 %in% names(dataset1))!=1) stop("'Y1' does not exist in 'dataset1'!")
      if (min(Y1 %in% names(dataset1))==1) {
                                Y1 <- data.frame(dataset1[, Y1], check.names=FALSE)
                                names(Y1) <- aY }

      if(!is.null(H1)) {
          aH <- H1  
          if (min(H1 %in% names(dataset1))!=1) stop("'H1' does not exist in 'dataset1'!")
          if (min(H1 %in% names(dataset1))==1) {
                                H1 <- as.data.frame(dataset1[, aH], stringsAsFactors=FALSE)
                                names(H1) <- aH }}
      if(!is.null(id1)) {
          aid <- id1  
          if (min(id1 %in% names(dataset1))!=1) stop("'id1' does not exist in 'dataset1'!")
          if (min(id1 %in% names(dataset1))==1) {
                                id1 <- as.data.frame(dataset1[, aid], stringsAsFactors=FALSE)
                                names(id1) <- aid }}
      if(!is.null(PSU1)) {
          aPSU <- PSU1  
          if (min(PSU1 %in% names(dataset1))!=1) stop("'PSU1' does not exist in 'dataset1'!")
          if (min(PSU1 %in% names(dataset1))==1) {
                                PSU1 <- as.data.frame(dataset1[, aPSU], stringsAsFactors=FALSE)
                                names(PSU1) <- aPSU }}
      if(!is.null(w_final1)) {
          aw_final <- w_final1  
          if (min(w_final1 %in% names(dataset1))!=1) stop("'w_final1' does not exist in 'dataset1'!")
          if (min(w_final1 %in% names(dataset1))==1) {
                                w_final1 <- data.frame(dataset1[, aw_final])
                                names(w_final1) <- aw_final }}
      if(!is.null(Z1)) {
          aZ <- Z1
          if (min(Z1 %in% names(dataset1))!=1) stop("'Z1' does not exist in 'dataset1'!")
          if (min(Z1 %in% names(dataset1))==1) {
                                Z1 <-data.frame(dataset1[, aZ], check.names=FALSE, stringsAsFactors=FALSE)
                                names(Z1) <- aZ }}
      if(!is.null(country1)) {
          acountry <- country1
          if (min(country1 %in% names(dataset1))!=1) stop("'country1' does not exist in 'dataset1'!")
          if (min(country1 %in% names(dataset1))==1) country1 <- data.frame(dataset1[, acountry])
          names(country1) <- acountry  }

      if(!is.null(period1)) {
          aperiod <- period1
          if (min(period1 %in% names(dataset1))!=1) stop("'period1' does not exist in 'dataset1'!")
          if (min(period1 %in% names(dataset1))==1) period1 <- data.frame(dataset1[, aperiod])
          names(period1) <- aperiod  }
     
      if (!is.null(Dom1)) {
          aDom <- Dom1
          if (min(Dom1 %in% names(dataset1))!=1) stop("'Dom1' does not exist in 'dataset1'!")
          if (min(Dom1 %in% names(dataset1))==1) {  
                  Dom1 <- as.data.frame(dataset1[, aDom], stringsAsFactors=FALSE) 
                  names(Dom1) <- aDom }    }
      }

  # Y1
  Y1 <- data.table(Y1, check.names=TRUE)
  n1 <- nrow(Y1)
  m1 <- ncol(Y1)
  if (!all(sapply(Y1, is.numeric))) stop("'Y1' must be numerical")
  if (any(is.na(Y1))) stop("'Y1' has unknown values")
  if (is.null(names(Y1))) stop("'Y1' must be colnames")
  
  # H1
  H1 <- data.table(H1)
  if (nrow(H1) != n1) stop("'H1' length must be equal with 'Y1' row count")
  if (ncol(H1) != 1) stop("'H1' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H1))) stop("'H1' has unknown values")
  if (is.null(names(H1))) stop("'H1' must be colnames")
  
  # id1
  id1 <- data.table(id1)
  if (any(is.na(id1))) stop("'id1' has unknown values")
  if (nrow(id1) != n1) stop("'id1' length must be equal with 'Y1' row count")
  if (ncol(id1) != 1) stop("'id1' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(id1))||(names(id1)=="id")) setnames(id1, names(id1), "ID")

  # PSU1
  PSU1 <- data.table(PSU1)
  if (any(is.na(PSU1))) stop("'PSU1' has unknown values")
  if (nrow(PSU1) != n1) stop("'PSU1' length must be equal with 'Y1' row count")
  if (ncol(PSU1) != 1) stop("'PSU1' has more than 1 column")
  
  # w_final1 
  w_final1 <- data.frame(w_final1)
  if (nrow(w_final1) != n1) stop("'w_final1' must be equal with 'Y1' row count")
  if (ncol(w_final1) != 1) stop("'w_final1' must be vector or 1 column data.frame, matrix, data.table")
  w_final1 <- w_final1[,1]
  if (!is.numeric(w_final1)) stop("'w_final1' must be numerical")
  if (any(is.na(w_final1))) stop("'w_final1' has unknown values") 
  
  # country1
  country1 <- data.table(country1)
  if (any(is.na(country1))) stop("'country1' has unknown values")
  if (nrow(country1) != n1) stop("'country1' length must be equal with 'Y1' row count")
  if (ncol(country1) != 1) stop("'country1' has more than 1 column")
  
  # period1
  period1 <- data.table(period1)
  if (any(is.na(period1))) stop("'period1' has unknown values")
  if (nrow(period1) != n1) stop("'period1' length must be equal with 'Y1' row count")
  if (nrow(period1[,.N, by=names(period1)])!=1) stop("'period1' must be 1 period")

  # Dom1
  if (!is.null(Dom1)) {
    Dom1 <- data.table(Dom1)
    if (any(duplicated(names(Dom1)))) 
           stop("'Dom1' are duplicate column names: ", 
                 paste(names(Dom1)[duplicated(names(Dom1))], collapse = ","))
    if (nrow(Dom1) != n1) stop("'Dom1' and 'Y1' must be equal row count")
    if (any(is.na(Dom1))) stop("'Dom1' has unknown values")
    if (is.null(names(Dom1))) stop("'Dom1' must be colnames")
    if ((is.null(Dom1))!=(is.null(Dom2))) stop("'Dom1' and 'Dom2' must be same column count") 
  }
  
  if (!is.null(Z1)) {
    Z1 <- data.table(Z1, check.names=TRUE)
    if (nrow(Z1) != n1) stop("'Z1' and 'Y1' must be equal row count")
    if (ncol(Z1) != m1) stop("'Z1' and 'Y1' must be equal column count")
    if (any(is.na(Z1))) stop("'Z1' has unknown values")
    if (is.null(names(Z1))) stop("'Z1' must be colnames")
    if ((is.null(Z1))!=(is.null(Z2))) stop("'Z1' and 'Z2' must be same column count") 
  }
 
  if(!is.null(dataset2)) {
      dataset2 <- data.frame(dataset2)
      aY <- Y2
      if (min(Y2 %in% names(dataset2))!=1) stop("'Y2' does not exist in 'dataset2'!")
      if (min(Y2 %in% names(dataset2))==1) {
                                Y2 <- data.frame(dataset2[, Y2], check.names=FALSE)
                                names(Y2) <- aY }

      if(!is.null(H2)) {
          aH <- H2
          if (min(H2 %in% names(dataset2))!=1) stop("'H2' does not exist in 'dataset2'!")
          if (min(H2 %in% names(dataset2))==1) {
                                H2 <- as.data.frame(dataset2[, aH], stringsAsFactors=FALSE)
                                names(H2) <- aH }}
      if(!is.null(id2)) {
          aid <- id2  
          if (min(id2 %in% names(dataset2))!=1) stop("'id2' does not exist in 'dataset2'!")
          if (min(id2 %in% names(dataset2))==1) {
                                id2 <- as.data.frame(dataset2[, aid], stringsAsFactors=FALSE)
                                names(id2) <- aid }}
      if(!is.null(PSU2)) {
          aPSU <- PSU2
          if (min(PSU2 %in% names(dataset2))!=1) stop("'PSU2' does not exist in 'dataset2'!")
          if (min(PSU2 %in% names(dataset2))==1) {
                                PSU2 <- as.data.frame(dataset2[, aPSU], stringsAsFactors=FALSE)
                                names(PSU2) <- aPSU }}
      if(!is.null(w_final2)) {
          aw_final <- w_final2  
          if (min(w_final2 %in% names(dataset2))!=1) stop("'w_final2' does not exist in 'dataset2'!")
          if (min(w_final2 %in% names(dataset2))==1) {
                                w_final2 <- data.frame(dataset2[, aw_final])
                                names(w_final2) <- aw_final }}
      if(!is.null(Z2)) {
          aZ <- Z2
          if (min(Z2 %in% names(dataset1))!=1) stop("'Z2' does not exist in 'dataset1'!")
          if (min(Z2 %in% names(dataset1))==1) {
                                Z2 <-data.frame(dataset1[, aZ], check.names=FALSE, stringsAsFactors=FALSE)
                                names(Z2) <- aZ }}

      if(!is.null(country2)) {
          acountry <- country2
          if (min(country2 %in% names(dataset2))!=1) stop("'country2' does not exist in 'dataset2'!")
          if (min(country2 %in% names(dataset2))==1) country2 <- data.frame(dataset2[, acountry])
          names(country2) <- acountry  }

      if(!is.null(period2)) {
          aperiod <- period2
          if (min(period2 %in% names(dataset2))!=1) stop("'period2' does not exist in 'dataset2'!")
          if (min(period2 %in% names(dataset2))==1) period2 <- data.frame(dataset2[, aperiod])
          names(period2) <- aperiod  }
     
      if (!is.null(Dom2)) {
          aDom <- Dom2
          if (min(Dom2 %in% names(dataset2))!=1) stop("'Dom2' does not exist in 'dataset2'!")
          if (min(Dom2 %in% names(dataset2))==1) {  
                  Dom2 <- as.data.frame(dataset2[, aDom], stringsAsFactors=FALSE) 
                  names(Dom2) <- aDom }    }
      }

  # Y2
  Y2 <- data.table(Y2, check.names=TRUE)
  n2 <- nrow(Y2)
  m2 <- ncol(Y2)
  if (!all(sapply(Y2, is.numeric))) stop("'Y2' must be numerical")   
  if (any(is.na(Y2))) stop("'Y2' has unknown values")
  if (is.null(names(Y2))) stop("'Y2' must be colnames")
  if (m1!=m2) stop("'Y1' and 'Y2' must be equal column count")
  if (!all(names(Y1) %in% names(Y2))) stop("'Y1' and 'Y2' must be same colnames")

  # H2
  H2 <- data.table(H2)
  if (nrow(H2) != n2) stop("'H2' length must be equal with 'Y2' row count")
  if (ncol(H2) != 1) stop("'H2' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H2))) stop("'H2' has unknown values")
  if (is.null(names(H2))) stop("'H2' must be colnames")
  if (!all(names(H1) %in% names(H2))) stop("'H1' and 'H2' must be same colnames")
  
  # id2
  id2 <- data.table(id2)
  if (any(is.na(id2))) stop("'id2' has unknown values")
  if (nrow(id2) != n1) stop("'id2' length must be equal with 'Y2' row count")
  if (ncol(id2) != 1) stop("'id2' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(id2))||(names(id2)=="id")) setnames(id2, names(id2), "ID")

  # PSU2
  PSU2 <- data.table(PSU2)
  if (any(is.na(PSU2))) stop("'PSU2' has unknown values")
  if (nrow(PSU2) != n2) stop("'PSU2' length must be equal with 'Y2' row count")
  if (ncol(PSU2) != 1) stop("'PSU2' has more than 1 column")
  if (!all(names(PSU1) %in% names(PSU2))) stop("'PSU1' and 'PSU2' must be same colnames")
  
  # w_final2 
  w_final2 <- data.frame(w_final2)
  if (nrow(w_final2) != n2) stop("'w_final2' must be equal with 'Y2' row count")
  if (ncol(w_final2) != 1) stop("'w_final2' must be vector or 1 column data.frame, matrix, data.table")
  w_final2 <- w_final2[, 1]
  if (!is.numeric(w_final2)) stop("'w_final2' must be numerical")
  if (any(is.na(w_final2))) stop("'w_final2' has unknown values") 
  
  # country2
  country2 <- data.table(country2)
  if (any(is.na(country2))) stop("'country2' has unknown values")
  if (nrow(country2) != n2) stop("'country2' length must be equal with 'Y2' row count")
  if (ncol(country2) != 1) stop("'country2' has more than 1 column")
  if (!all(names(country1) %in% names(country2))) stop("'country1' and 'country2' must be same colnames")

  # period2
  period2 <- data.table(period2)
  if (any(is.na(period2))) stop("'period2' has unknown values")
  if (nrow(period2) != n2) stop("'period2' length must be equal with 'Y2' row count")
  if (ncol(period1)!=ncol(period2)) stop("'period1' and 'period2' must be same column count")
  if (!all(names(period1) %in% names(period2))) stop("'period1' and 'period2' must be same colnames")
  if (nrow(period2[,.N, by=names(period2)])!=1) stop("'period2' must be 1 period")

  # Dom2
  if (!is.null(Dom2)) {
    Dom2 <- data.table(Dom2)
    if (any(duplicated(names(Dom2)))) 
           stop("'Dom2' are duplicate column names: ", 
                 paste(names(Dom2)[duplicated(names(Dom2))], collapse = ","))
    if (nrow(Dom2) != n2) stop("'Dom2' and 'Y2' must be equal row count")
    if (any(is.na(Dom2))) stop("'Dom2' has unknown values")
    if (is.null(names(Dom2))) stop("'Dom2' must be colnames")
    if ((is.null(Dom1))!=(is.null(Dom2))) stop("'Dom1' and 'Dom2' must be same column count") 
    if (ncol(Dom1)!=ncol(Dom2)) stop("'Dom1' and 'Dom2' must be same column count")
    if (!all(names(Dom1) %in% names(Dom2))) stop("'Dom1' and 'Dom2' must be same colnames")
    Dm1 <- Dom1[,.N,keyby=names(Dom2)][, N:=NULL]
    Dm2 <- Dom2[,.N,keyby=names(Dom2)][, N:=NULL]
    if (!all(Dm1==Dm2)) stop(stop("'Dom1' and 'Dom2' must be equal groups"))    
  }

  # Z2
  if (!is.null(Z2)) {
    Z2 <- data.table(Z2, check.names=TRUE)
    if (nrow(Z2) != n2) stop("'Z2' and 'Y2' must be equal row count")
    if (ncol(Z2) != m2) stop("'Z2' and 'Y2' must be equal column count")
    if (any(is.na(Z2))) stop("'Z2' has unknown values")
    if (is.null(names(Z2))) stop("'Z2' must be colnames")
    if ((is.null(Z1))!=(is.null(Z2))) stop("'Z1' and 'Z2' must be same column count")   
    if (ncol(Z1)!=ncol(Z2)) stop("'Z1' and 'Z2' must be same column count")
    if (!all(names(Z1) %in% names(Z2))) stop("'Z1' and 'Z2' must be same colnames")
  }
  
  data1 <- vardcros(Y=Y1, H=H1, PSU=PSU1, w_final=w_final1,
                    id=id1, Dom=Dom1, Z=Z1, country=country1,
                    period=period1, dataset=NULL,
                    linratio=linratio, household_level_max=TRUE,
                    withperiod=TRUE, netchanges=TRUE, 
                    confidence = confidence)

  namesY <- namesZ <- H1 <- PSU1 <- w_final1 <-  NULL
  id1 <- dataset1 <- Dom1 <- Z1 <- country1 <- NULL
  
  data2 <- vardcros(Y=Y2, H=H2, PSU=PSU2, w_final=w_final2,
                    id=id2, Dom=Dom2, Z=Z2, country=country2,
                    period=period2, dataset=NULL,
                    linratio=linratio, household_level_max=TRUE,
                    withperiod=TRUE, netchanges=TRUE, 
                    confidence = confidence)

  Dom <- names(Dom2)
  H <- names(H2)
  Y <- names(Y2)
  nz <- ifelse(!is.null(Z2), 1, 0)
  np <- ifelse(!is.null(Dom2), ncol(Dom2), 0)
  np <- 2 + np + nz

  namesYs <- namesZs <- PSU2 <- w_final2 <- NULL
  id2 <- dataset2 <- Dom2 <- Z2 <- country2 <- NULL
  N <- grad1 <- grad2 <- country <- rot01 <- rot02 <- stratasf <- NULL
  name1 <- num1num1 <- num1 <- den1den1 <- den1 <- num2num2 <- NULL
  num2 <- den2den2 <- den2 <-  num1den1 <- num1num2 <- num1den2 <- NULL
  den1num2 <- den1den2 <- num2den2 <- C12 <- num1_1 <-  den1_1 <- NULL
  num1den1 <- num1num1 <- den1den1 <- C13 <- num1_2 <- num1num2 <- NULL
  num2num2 <- C14 <- den1_2 <- num1den2 <- den2den2 <- C23 <- NULL
  den1num2 <- C24 <- C34 <- num2den2 <- estim <- estim_1 <- NULL
  estim_2 <- grad1_1 <- grad1_2 <- CI_upper <- grad2_1 <- grad2_2 <- NULL
  se <- rse <- cv <- CI_lower <-  absolute_margin_of_error <- NULL
  relative_margin_of_error <- NULL

  var_grad1 <- data1$var_grad
  var_grad2 <- data2$var_grad
  var_grad1[, (names(period1)):=NULL]
  var_grad2[, (names(period2)):=NULL]
  var_grad1[!is.null(grad1), grad1:=-grad1]
  var_grad1[!is.null(grad1), grad2:=-grad2]

  setnames(var_grad1, names(var_grad1)[-c(1:np)], paste0(names(var_grad1)[-c(1:np)], "_1"))
  setnames(var_grad2, names(var_grad2)[-c(1:np)], paste0(names(var_grad2)[-c(1:np)], "_2"))
  var_grad1[, country:=as.character(country)]
  var_grad2[, country:=as.character(country)]
  setkeyv(var_grad1, names(var_grad1)[1:np])
  setkeyv(var_grad2, names(var_grad2)[1:np])
  var_grad <- merge(var_grad1, var_grad2, all=T)
  var_grad1 <- var_grad2 <- NULL

  data1 <- data1$data_net_changes
  data2 <- data2$data_net_changes
  data1[, (names(period1)):=NULL]
  data2[, (names(period2)):=NULL]
  main <- ifelse(!is.null(var_grad$namesZ), "namesY", c("namesY", Dom))
  nrowv <- nrow(var_grad)
  if ((!linratio)&(4+nrowv<ncol(data1))) namesZ <- names(data1)[(4+nrowv):ncol(data1)] 
  period <- names(data1)[4:ncol(data1)]
  setnames(data1, names(data1)[-c(1:3)], paste0(names(data1)[-c(1:3)], "_1"))
  setnames(data2, names(data2)[-c(1:3)], paste0(names(data2)[-c(1:3)], "_2"))
  period2 <- names(data2)[4:ncol(data2)]
  data1[, rot01:=1]
  data2[, rot02:=1]
  setkeyv(data1, names(data1)[1:3])
  setkeyv(data2, names(data2)[1:3])
  data <- merge(data1, data2, all=T)
  data1 <- data2 <- NULL

  recode.NA <- function(DT, cols = seq_len(ncol(DT))) {
     for (j in cols) if (is.numeric(DT[[j]]))
      set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]), 0L, 0))
   }
  recode.NA(data, c(paste0(period,"_1"), paste0(period,"_2")))

  dataH <- data[[H]]
  dataH <- factor(dataH)
  if (length(levels(dataH))==1) { data[, stratasf:= 1]
                                dataH <- "stratasf"
                      }  else { dataH <- data.table(model.matrix( ~ dataH-1))
                                data <- cbind(data, dataH)
                                dataH <- names(dataH) }

  den1 <- den2 <- NULL
  fit <- lapply(1:nrowv, function(i) {
           fits <- lapply(split(data, data$country), function(d) {
                   y1 <- paste0(period[i], "_1")
                   y2 <- paste0(period[i], "_2")
                   if (!is.null(var_grad$namesZ)) { 
                                    z1 <- paste0(",", toString(period[i + nrowv]), "_1") 
	                              z2 <- paste0(",", toString(period[i + nrowv]), "_2")
                               } else z1 <- z2 <- ""
                   vect <- c("rot01*", "rot02*", "rot01*rot02*")

                   funkc <- as.formula(paste0("cbind(", trim(toString(y1)), z1, ", ", 
                                                        trim(toString(y2)), z2, ")~-1+",
                                                        paste(t(unlist(lapply(dataH, function(x) 
                                                                 paste0("rot01*", toString(x), "+",
                                                                        "rot02*", toString(x), "+",
                                                                        "rot01*rot02*", toString(x))))),
                                                                        collapse= "+"))) 
                   res <- data.table(lm(funkc, data=d)$res)
                   if (!is.null(var_grad$namesZ)) { 
                         setnames(res, names(res), c("num1", "den1", "num2", "den2"))
                       } else setnames(res, names(res), c("num1", "num2"))
                   res[, namesY:=period[i]]
                   res[!is.null(var_grad$namesZ), namesZ:=period[i + nrowv]]
                   res[, num1num1:=num1 * num1]
                   res[!is.null(den1), den1den1:=den1 * den1]
                   res[, num2num2:=num2 * num2]
                   res[!is.null(den2), den2den2:=den2 * den2]
                   res[!is.null(den1), num1den1:=num1 * den1]
                   res[, num1num2:=num1 * num2]
                   res[!is.null(den2), num1den2:=num1 * den2]
                   res[!is.null(den1), den1num2:=den1 * num2]
                   res[!is.null(den2), den1den2:=den1 * den2]
                   res[!is.null(den2), num2den2:=num2 * den2] 

                   res <- data.table(res, d)
                   varsp <- c("num1num1", "den1den1", "num2num2", "den2den2", "num1den1",
                              "num1num2", "num1den2", "den1num2", "den1den2", "num2den2")
                   varsp <- varsp[varsp %in% names(res)]
                   keynames <- c("country", "namesY", "namesZ")
                   keynames <- keynames[keynames %in% names(res)]
                   fits <- res[, lapply(.SD, sum), keyby=keynames, .SDcols=varsp]
                })
            data.table(do.call("rbind", fits))      
        })
   res <- data.table(do.call("rbind", fit))

   res[, country:=as.character(country)]
   namesYZ <- c("namesY", "namesZ")
   namesYZ <- namesYZ[namesYZ %in% names(res)]
   setnames(res, namesYZ,  paste0(namesYZ, "s"))

   var_grad[, namesYs:=Reduce(function(x, y)
                              paste(x, y, sep = "__"), .SD),
                              .SDcols=c("namesY", Dom)]
   var_grad[!is.null(namesZ), namesZs:=Reduce(function(x, y)
                                       paste(x, y, sep = "__"), .SD),
                                      .SDcols=c("namesZ", Dom)]

   setkeyv(res, c("country", paste0(namesYZ, "s")))
   setkeyv(var_grad, c("country", paste0(namesYZ, "s")))
   data <- merge(res, var_grad, all=T)
   res <- fit <- var_gr <- NULL
   data[, namesYs:=NULL]
   data[!is.null(namesZ), namesZs:=NULL]


   data[!is.null(namesZ), C12:=sqrt(num1_1)*sqrt(den1_1)*num1den1/(sqrt(num1num1)*sqrt(den1den1))]
   data[, C13:=sqrt(num1_1)*sqrt(num1_2)*num1num2/(sqrt(num1num1)*sqrt(num2num2))]
   data[!is.null(namesZ), C14:=sqrt(num1_1)*sqrt(den1_2)*num1den2/(sqrt(num1num1)*sqrt(den2den2))]
   data[!is.null(namesZ), C23:=sqrt(den1_1)*sqrt(num1_2)*den1num2/(sqrt(den1den1)*sqrt(num2num2))]
   data[!is.null(namesZ), C24:=sqrt(den1_1)*sqrt(den1_2)*den1num2/(sqrt(den1den1)*sqrt(den2den2))]
   data[!is.null(namesZ), C34:=sqrt(num1_2)*sqrt(den1_2)*num2den2/(sqrt(num2num2)*sqrt(den2den2))]

   data[, estim:=estim_1 - estim_2]
   data[!is.null(namesZ), var:= (grad1_1 * grad1_1 * num1_1) +  
                (grad1_2 * grad1_2 * den1_1) + 
                (grad2_1 * grad2_1 * num1_2) + 
                (grad2_2 * grad2_2 * den1_2) + 
             2*((grad1_1 * grad1_2 * C12) +
                (grad1_1 * grad2_1 * C13) +
                (grad1_1 * grad2_2 * C14) +
                (grad1_2 * grad2_1 * C23) +
                (grad1_2 * grad2_2 * C24) +
                (grad2_1 * grad2_2 * C34))]

   data[is.null(namesZ), var:= num1_1 + num1_2]

   data[, se:=sqrt(var)]
   data[, rse:=se/estim]
   data[, cv:=rse*100]
   tsad <- qnorm(0.5*(1+confidence))
   data[, absolute_margin_of_error:= tsad * se]
   data[, relative_margin_of_error:= tsad * cv]
   data[, CI_lower:=estim - tsad*se]
   data[, CI_upper:=estim + tsad*se]
   data[, c("country", Dom, namesYZ, "estim",
            "var", "se", "rse", "cv",
            "absolute_margin_of_error",
            "relative_margin_of_error",
            "CI_lower", "CI_upper"), with=F]
}   
