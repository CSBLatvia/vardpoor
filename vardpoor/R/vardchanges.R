vardchanges <- function(Y, H, PSU, w_final, id,
                     Dom = NULL, Z=NULL, 
                     country, periods,
                     dataset = NULL,
                     period1, period2,
                     linratio = FALSE,
                     use.estVar = FALSE,
                     confidence=0.95) {
 
  ### Checking

  if (!is.logical(linratio)) stop("'linratio' must be the logical value")
  if (!is.logical(use.estVar)) stop("'use.estVar' must be the logical value")
  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset))!=1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset))==1)  Y <- dataset[, Y, with=FALSE]

      if(!is.null(H)) {
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) H <- dataset[, H, with=FALSE] }

      if(!is.null(id)) {
          if (min(id %in% names(dataset))!=1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset))==1) id <- dataset[, id, with=FALSE]}

      if(!is.null(PSU)) {
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) PSU <- dataset[, PSU, with=FALSE] }

      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset))!=1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset))==1) w_final <- dataset[, w_final, with=FALSE] }

      if(!is.null(Z)) {
          if (min(Z %in% names(dataset))!=1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset))==1) Z <- dataset[, Z, with=FALSE]}

      if(!is.null(country)) {
          if (min(country %in% names(dataset))!=1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset))==1) country <- dataset[, country, with=FALSE] }

      if(!is.null(periods)) {
          if (min(periods %in% names(dataset))!=1) stop("periods' does not exist in 'dataset'!")
          if (min(periods %in% names(dataset))==1) periods <-dataset[, periods, with=FALSE] }
     
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset))!=1) stop("'Dom1' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset))==1) Dom <- dataset[, Dom, with=FALSE]    }
   }

  # Y
  Y <- data.table(Y, check.names=TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numerical")
  if (any(is.na(Y))) stop("'Y' has unknown values")
  if (is.null(names(Y))) stop("'Y' must be colnames")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")
  
  # id
  id <- data.table(id)
  if (any(is.na(id))) stop("'id' has unknown values")
  if (nrow(id) != n) stop("'id' length must be equal with 'Y' row count")
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(id))||(names(id)=="id")) setnames(id, names(id), "ID")

  # PSU
  PSU <- data.table(PSU)
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  
  # w_final
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[,1]
  if (!is.numeric(w_final)) stop("'w_final' must be numerical")
  if (any(is.na(w_final))) stop("'w_final' has unknown values") 
  
  # country
  country <- data.table(country)
  if (any(is.na(country))) stop("'country' has unknown values")
  if (nrow(country) != n) stop("'country' length must be equal with 'Y' row count")
  if (ncol(country) != 1) stop("'country' has more than 1 column")
  if (ncol(country) != 1) stop("'country' has more than 1 column")
  if (!is.character(country[[names(country)]])) stop("'country' must be character")

  # periods
  periods <- data.table(periods, check.names=TRUE)
  if (any(is.na(periods))) stop("'periods' has unknown values")
  if (nrow(periods) != n) stop("'periods' length must be equal with 'Y' row count")
  if (nrow(periods[,.N, by=names(periods)])!=2) stop("'periods' must be two periods")

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
    Dom[, (names(Dom)):=lapply(.SD, as.character)]
  }
  
  namesZ <- NULL
  if (!is.null(Z)) {
    Z <- data.table(Z, check.names=TRUE)
    if (nrow(Z) != n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' must be equal column count")
    if (any(is.na(Z))) stop("'Z' has unknown values")
    if (is.null(names(Z))) stop("'Z' must be colnames")
    namesZ <- names(Z)
  }
 
   # period1
   period1 <- data.table(t(period1), check.names=TRUE)
   if (nrow(period1) != 1) stop("'period1' must be 1 row")
   if (ncol(period1) != ncol(periods)) stop("'period1' column and 'periods' row count must be equal")
   setnames(period1, names(period1), names(periods))
   if (any(is.na(period1))) stop("'period1' has unknown values")
   setkeyv(period1, names(periods))
   periodss <- copy(periods)
   periodss[, periodss:=1]
   setkeyv(periodss, names(periods))
   if (any(is.na(merge(period1, periodss, all.x=TRUE)))) stop("'period1' row must be exist in 'periods'")

   # period2
   period2 <- data.table(t(period2), check.names=TRUE)
   if (nrow(period2) != 1) stop("'period2' must be 1 row")
   if (ncol(period2) != ncol(periods)) stop("'period2' column and 'periods' row count must be equal")
   setnames(period2, names(period2), names(periods))
   if (any(is.na(period2))) stop("'period2' has unknown values")
   setkeyv(period2, names(periods))
   if (any(is.na(merge(period2, periodss, all.x=TRUE)))) stop("'period2' row must be exist in 'periods'")

   data <- vardcros(Y=Y, H=H, PSU=PSU, w_final=w_final,
                    id=id, Dom=Dom, Z=Z, country=country,
                    periods=periods, dataset=NULL,
                    linratio=linratio, use.estVar=use.estVar,
                    household_level_max=TRUE,
                    withperiod=TRUE, netchanges=TRUE, 
                    confidence=confidence)

  crossectional_results <- data$results
  Dom <- names(Dom)
  H <- names(H)
  Y <- names(Y)
  country <- names(country)
  np <- ifelse(!is.null(Dom), length(Dom), 0)
  np <- 2 + np + as.integer(!is.null(namesZ))
  N <- namesY <- PSU <- w_final <- id <- NULL
  
  dataset <- namesYs <- namesZs <- grad1 <- grad2 <- NULL
  Z <- rot01 <- rot02 <- stratasf <- name1 <-  NULL
  num1 <- num1num1 <- den1den1 <- den1 <- num2num2 <- NULL
  den2den2 <- den2 <- num1den1 <- num1num2 <- NULL
  num2 <- num1den2 <- den1num2 <- den1den2 <- num2den2 <- NULL
  C22 <- C12 <- num1_1 <-  den1_1 <- num1den1 <- NULL
  C11 <- den1den1 <- C13 <- num1_2 <- C14 <- den1_2 <- NULL
  C23 <- C24 <- C33 <- C34 <- C44 <- estim <- estim_1 <- NULL
  estim_2 <- grad1_1 <- grad1_2 <- CI_upper <- grad2_1 <- NULL
  grad2_2 <- se <- rse <- cv <- CI_lower <-  absolute_margin_of_error <- NULL
  relative_margin_of_error <- NULL

  var_grad <- data$var_grad
  setkeyv(var_grad, names(periods))
  var_grad1 <- merge(period1, var_grad, all.x=TRUE)
  var_grad2 <- merge(period2, var_grad, all.x=TRUE)
  var_grad1[, (names(periods)):=NULL]
  var_grad2[, (names(periods)):=NULL]
  if (!is.null(var_grad1$grad1)){var_grad1[, grad1:=-grad1]
                                 var_grad1[, grad2:=-grad2] }

  setnames(var_grad1, names(var_grad1)[-c(1:np)], paste0(names(var_grad1)[-c(1:np)], "_1"))
  setnames(var_grad2, names(var_grad2)[-c(1:np)], paste0(names(var_grad2)[-c(1:np)], "_2"))
  setkeyv(var_grad1, names(var_grad1)[1:np])
  setkeyv(var_grad2, names(var_grad2)[1:np])
  var_grad <- merge(var_grad1, var_grad2, all=TRUE)
  var_grad1 <- var_grad2 <- NULL
 
  data <- data$data_net_changes
  setkeyv(data, names(periods))
  data1 <- merge(period1, data, all.x=TRUE)
  data2 <- merge(period2, data, all.x=TRUE)
  data1[, (names(periods)):=NULL]
  data2[, (names(periods)):=NULL]
  nrowv <- nrow(var_grad)

  period <- names(data1)[4:ncol(data1)]
  print(period)
  setnames(data1, names(data1)[-c(1:3)], paste0(names(data1)[-c(1:3)], "_1"))
  setnames(data2, names(data2)[-c(1:3)], paste0(names(data2)[-c(1:3)], "_2"))
  period2 <- names(data2)[4:ncol(data2)]
  data1[, rot01:=1]
  data2[, rot02:=1]
  setkeyv(data1, names(data1)[1:3])
  setkeyv(data2, names(data2)[1:3])
  data <- merge(data1, data2, all=TRUE)
  data1 <- data2 <- NULL

  recode.NA <- function(DT, cols = seq_len(ncol(DT))) {
     for (j in cols) if (is.numeric(DT[[j]]))
      set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]), 0L, 0))
   }
  recode.NA(data, c(paste0(period,"_1"), paste0(period,"_2"),
                    "rot01", "rot02"))

  dataH <- data[[H]]
  dataH <- factor(dataH)
  if (length(levels(dataH))==1) { data[, stratasf:= 1]
                                  dataH <- "stratasf"
                         } else { dataH <- data.table(model.matrix( ~ dataH-1))
                                  data <- cbind(data, dataH)
                                  dataH <- names(dataH) }
  den1 <- den2 <- NULL

  fit <- lapply(1:nrowv, function(i) {
            fits <- lapply(split(data, data[[country]]), function(DT3c) {
                      y1 <- paste0(period[i], "_1")
                      y2 <- paste0(period[i], "_2")
                      if (!is.null(namesZ)) { 
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
                      res <- lm(funkc, data=DT3c)
                      
                      if (use.estVar) { res <- data.table(estVar(res))
                                  } else res <- data.table(lm(funkc, data=DT3c)$res)
                      if (!is.null(namesZ)) { 
                                   setnames(res, names(res), c("num1", "den1", "num2", "den2"))
                                   res[, namesZ:=period[i + nrowv]]
                            } else setnames(res, names(res), c("num1", "num2"))

                      Zvn <- as.integer(!is.null(namesZ))
                      res [, namesY:=period[i]]

                      if (use.estVar) { 
                           res[, num1num1:=res[["num1"]][1]]
                           res[, num2num2:=res[["num2"]][2+Zvn]]
                           res[, num1num2:=res[["num1"]][2+Zvn]]
                           if (!is.null(namesZ)) {
                                 res[, den1den1:=res[["den1"]][1+Zvn]]
                                 res[, den2den2:=res[["den2"]][3+Zvn]]
                                 res[, num1den1:=res[["num1"]][1+Zvn]]
                                 res[, num1den2:=res[["num1"]][3+Zvn]]

                                 res[, den1num2:=res[["den1"]][2+Zvn]]
                                 res[, den1den2:=res[["den1"]][3+Zvn]]
                                 res[, num2den2:=res[["num2"]][3+Zvn]] }
                           res <- data.table(res[1], DT3c[1])
                        } else {
                            res[, num1num1:=num1 * num1]
                            res[, num2num2:=num2 * num2]
                            res[, num1num2:=num1 * num2]
                            if (!is.null(namesZ)) {
                                  res[, den1den1:=den1 * den1]
                                  res[, den2den2:=den2 * den2]
                                  res[, num1den1:=num1 * den1]
                                  res[, num1den2:=num1 * den2]
                                  res[, den1num2:=den1 * num2]
                                  res[, den1den2:=den1 * den2]
                                  res[, num2den2:=num2 * den2] }
                            res <- data.table(res, DT3c)}

                      varsp <- c("num1num1", "den1den1",
                                 "num2num2", "den2den2",
                                 "num1den1", "num1num2",
                                 "num1den2", "den1num2",
                                 "den1den2", "num2den2")
                      varsp <- varsp[varsp %in% names(res)]
                      keynames <- c(country, "namesY", "namesZ")
                      keynames <- keynames[keynames %in% names(res)]
                      fits <- res[, lapply(.SD, sum), keyby=keynames, .SDcols=varsp]
                      return(fits)
                })
            rbindlist(fits)      
        })
   res <- rbindlist(fit)

   set(res, j=country, value=as.character(res[[country]]))
   namesYZ <- c("namesY", "namesZ")
   namesYZ <- namesYZ[namesYZ %in% names(res)]
   setnames(res, namesYZ,  paste0(namesYZ, "s"))

   if (!is.null(Dom)) {
          var_grad[, paste0(Dom, "_ss"):=lapply(Dom, function(x) paste0(x,".", get(x)))]
          var_grad[, namesYs:=Reduce(function(x, y)
                                      paste(x, y, sep = "__"), .SD),
                                     .SDcols=c("namesY", paste0(Dom, "_ss"))]
          if (!is.null(namesZ)) { var_grad[, namesZs:=Reduce(function(x, y)
                                                               paste(x, y, sep = "__"), .SD),
                                                             .SDcols=c("namesZ", paste0(Dom, "_ss"))]
                                }
       } else { var_grad[, namesYs:=namesY]
                if (!is.null(namesZ)) var_grad[, namesZs:=namesZ]}

   setkeyv(res, c(country, paste0(namesYZ, "s")))
   setkeyv(var_grad, c(country, paste0(namesYZ, "s")))
   data <- merge(res, var_grad, all=TRUE)
   res <- fit <- var_gr <- NULL
   data[, namesYs:=NULL]

   data[, C11:=num1_1]
   data[, C33:=num1_2]
   data[, C13:=sqrt(num1_1*num1_2/(num1num1*num2num2))*num1num2]

   if (!is.null(namesZ)) {
          data[, namesZs:=NULL]   
          data[, C22:=den1_1]
          data[, C44:=den1_2]
          data[, C12:=sqrt(num1_1*den1_1/(num1num1*den1den1))*num1den1]
          data[, C14:=sqrt(num1_1*den1_2/(num1num1*den2den2))*num1den2]
          data[, C23:=sqrt(den1_1*num1_2/(den1den1*num2num2))*den1num2]
          data[, C24:=sqrt(den1_1*den1_2/(den1den1*den2den2))*den1den2]
          data[, C34:=sqrt(num1_2*den1_2/(num2num2*den2den2))*num2den2]
        }

   data[, estim:=estim_1 - estim_2]
   if (!is.null(namesZ)) {
         data[, var:= (grad1_1 * grad1_1 * C11) +  
                      (grad1_2 * grad1_2 * C22) + 
                      (grad2_1 * grad2_1 * C33) + 
                      (grad2_2 * grad2_2 * C44) + 
                   2*((grad1_1 * grad1_2 * C12) +
                      (grad1_1 * grad2_1 * C13) +
                      (grad1_1 * grad2_2 * C14) +
                      (grad1_2 * grad2_1 * C23) +
                      (grad1_2 * grad2_2 * C24) +
                      (grad2_1 * grad2_2 * C34))]
       } else data[, var:=num1_1 + num1_2 - 2 * C13]

   data[, se:=sqrt(var)]
   data[, rse:=se/estim]
   data[, cv:=rse*100]
   tsad <- qnorm(0.5*(1+confidence))
   data[, absolute_margin_of_error:= tsad * se]
   data[, relative_margin_of_error:= tsad * cv]
   data[, CI_lower:=estim - tsad*se]
   data[, CI_upper:=estim + tsad*se]
   changes_results <- data[, c(country, Dom, namesYZ, "estim",
                               "var", "se", "rse", "cv",
                               "absolute_margin_of_error",
                               "relative_margin_of_error",
                               "CI_lower", "CI_upper"), with=FALSE]

 list(crossectional_results=crossectional_results, changes_results=changes_results)
}   
