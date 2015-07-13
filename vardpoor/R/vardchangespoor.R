

vardchangespoor <- function(Y,
                     age=NULL,
                      pl085=NULL,
                     month_at_work=NULL,
                     Y_den=NULL,
                     Y_thres = NULL,
                     wght_thres = NULL,
                     H, PSU, w_final, id,
                     Dom = NULL,
                     country, periods,
                     sort=NULL,
                     gender = NULL,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     dataset = NULL,
                     period1, period2,
                     linratio = FALSE,
                     use.estVar = FALSE,
                     confidence=0.95,
                     several.ok=FALSE,
                     type="linrmpg") {
 
  ### Checking

  all_choices <- c("linarpr","linarpt","lingpg",
                            "linpoormed",  "linrmpg","lingini",
                            "lingini2","linqsr", "linrmi", "linarr")
  choices <- c("all_choices", all_choices)
  type <- tolower(type)

  type <- match.arg(type, choices, several.ok)
  if (any(type == "all_choices")) {type <- all_choices
                                   several.ok <- TRUE }  

  # check 'p'
  p <- percentage
  if(!is.numeric(p) || length(p) != 1 || p[1] < 0 || p[1] > 100) {
         stop("'percentage' must be a numeric value in [0,100]")
     } else p <- percentage[1]

  # check 'order_quant'

  oq <- order_quant
  if(!is.numeric(oq) || length(oq) != 1 || oq[1] < 0 || oq[1] > 100) {
         stop("'order_quant' must be a numeric value in [0,100]")
     } else order_quant <- order_quant[1]

  if(!is.numeric(alpha) || length(alpha) != 1 || alpha[1] < 0 || alpha[1] > 100) {
         stop("'alpha' must be a numeric value in [0,100]")  }

  if (!is.logical(use.estVar)) stop("'use.estVar' must be the logical value")
  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {
      dataset <- data.frame(dataset)
      aY <- Y
      if (min(Y %in% names(dataset))!=1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset))==1) {
                                Y <- data.frame(dataset[, Y], check.names=FALSE)
                                names(Y) <- aY }
      if(!is.null(age)) {
          if (min(age %in% names(dataset))!=1) stop("'age' does not exist in 'dataset'!")
          if (min(age %in% names(dataset))==1) age <- dataset[, age] }
      if(!is.null(pl085)) {
          if (min(pl085 %in% names(dataset))!=1) stop("'pl085' does not exist in 'dataset'!")
          if (min(pl085 %in% names(dataset))==1) pl085 <- dataset[, pl085] }
      if(!is.null(month_at_work)) {
          if (min(month_at_work %in% names(dataset))!=1) stop("'month_at_work' does not exist in 'dataset'!")
          if (min(month_at_work %in% names(dataset))==1) month_at_work <- dataset[, month_at_work] }
      if(!is.null(Y_den)) {
          if (min(Y_den %in% names(dataset))!=1) stop("'Y_den' does not exist in 'dataset'!")
          if (min(Y_den %in% names(dataset))==1) Y_den <- dataset[, Y_den] }
      if(!is.null(Y_thres)) {
          if (min(Y_thres %in% names(dataset))!=1) stop("'Y_thres' does not exist in 'dataset'!")
          if (min(Y_thres %in% names(dataset))==1) Y_thres <- dataset[, Y_thres] }    
      if(!is.null(wght_thres)) {
          if (min(wght_thres %in% names(dataset))!=1) stop("'wght_thres' does not exist in 'dataset'!")
          if (min(wght_thres %in% names(dataset))==1) wght_thres <- dataset[, wght_thres] }
      if(!is.null(H)) {
          aH <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- as.data.frame(dataset[, aH], stringsAsFactors=FALSE)
                                names(H) <- aH }}
      if(!is.null(id)) {
          aid <- id  
          if (min(id %in% names(dataset))!=1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset))==1) {
                                id <- as.data.frame(dataset[, aid], stringsAsFactors=FALSE)
                                names(id) <- aid }}
      if(!is.null(PSU)) {
          aPSU <- PSU  
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) {
                                PSU <- as.data.frame(dataset[, aPSU], stringsAsFactors=FALSE)
                                names(PSU) <- aPSU }}
      if(!is.null(w_final)) {
          aw_final <- w_final  
          if (min(w_final %in% names(dataset))!=1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset))==1) {
                                w_final <- data.frame(dataset[, aw_final])
                                names(w_final) <- aw_final }}

      if(!is.null(country)) {
          acountry <- country
          if (min(country %in% names(dataset))!=1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset))==1) country <- data.frame(dataset[, acountry], stringsAsFactors=FALSE)
          names(country) <- acountry  }

      if(!is.null(periods)) {
          aperiods <- periods
          if (min(periods %in% names(dataset))!=1) stop("periods' does not exist in 'dataset'!")
          if (min(periods %in% names(dataset))==1) periods <- data.frame(dataset[, aperiods])
          names(periods) <- aperiods  }

      if(!is.null(gender)) {
          if (min(gender %in% names(dataset))!=1) stop("'gender' does not exist in 'dataset'!")
          if (min(gender %in% names(dataset))==1) gender <- dataset[, gender] }

      if(!is.null(sort)) {
          if (min(sort %in% names(dataset))!=1) stop("'sort' does not exist in 'dataset'!")
          if (min(sort %in% names(dataset))==1) sort <- dataset[, sort] }
     
      if (!is.null(Dom)) {
          aDom <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom1' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, aDom], stringsAsFactors=FALSE) 
                  names(Dom) <- aDom }    }
      }

  # Y
  Y <- data.frame(Y)
  n <- nrow(Y)
  if (ncol(Y) != 1) stop("'Y' must have vector or 1 column data.frame, matrix, data.table")
  Y <- Y[,1]
  if (!is.numeric(Y)) stop("'Y' must be numerical")
  if (any(is.na(Y))) stop("'Y' has unknown values")
  
 if (!is.null(Y_den)) {
          Y_den <- data.frame(Y_den)
          if (ncol(Y_den) != 1) stop("'Y_den' must be vector or 1 column data.frame, matrix, data.table")
          if (nrow(Y_den) != n) stop("'Y_den' must be the same length as 'Y'")
          Y_den <- Y_den[,1]
          if(!is.numeric(Y_den)) stop("'Y_den' must be numerical")
          if (any(is.na(Y_den))) stop("'Y_den' has unknown values")
  }

  # age
  if (!is.null(age)) {
       age <- data.frame(age)
       if (nrow(age) != n) stop("'age' must be the same length as 'Y'")
       if (ncol(age) != 1) stop("'age' must be vector or 1 column data.frame, matrix, data.table")
      age <- age[, 1]
      if (!is.numeric(age)) stop("'age' must be numerical")
      if (any(is.na(age))) stop("'age' has unknown values")
   }

   # pl085
   if (!is.null(pl085)) {
       pl085 <- data.frame(pl085)
       if (nrow(pl085) != n) stop("'pl085' must be the same length as 'Y'")
       if (ncol(pl085) != 1) stop("'pl085' must be vector or 1 column data.frame, matrix, data.table")
       pl085 <- pl085[, 1]
       if (!is.numeric(pl085)) stop("'pl085' must be numerical")
       if (any(is.na(pl085))) stop("'pl085' has unknown values")
   }

   # month_at_work
   if (!is.null(month_at_work)) {
        month_at_work <- data.frame(month_at_work)
        if (nrow(month_at_work) != n) stop("'month_at_work' must be the same length as 'Y'")
        if (ncol(month_at_work) != 1) stop("'month_at_work' must be vector or 1 column data.frame, matrix, data.table")
        month_at_work <- month_at_work[, 1]
        if (!is.numeric(pl085)) stop("'month_at_work' must be numerical")
        if (any(is.na(pl085))) stop("'month_at_work' has unknown values")
  }

  # Y_thres
  if (!is.null(Y_thres)) {
       Y_thres <- data.frame(Y_thres)
       if (nrow(Y_thres) != n) stop("'Y_thres' must have the same length as 'Y'")
       if (ncol(Y_thres) != 1) stop("'Y_thres' must have vector or 1 column data.frame, matrix, data.table")
       Y_thres <- Y_thres[,1]
       if (!is.numeric(Y_thres)) stop("'Y_thres' must be numerical")
       if (any(is.na(Y_thres))) stop("'Y_thres' has unknown values") 
     } else Y_thres <- Y

  # wght_thres
  if (is.null(wght_thres)) wght_thres <- w_final
  wght_thres <- data.frame(wght_thres)
  if (nrow(wght_thres) != n) stop("'wght_thres' must have the same length as 'Y'")
  if (ncol(wght_thres) != 1) stop("'wght_thres' must have vector or 1 column data.frame, matrix, data.table")
  wght_thres <- wght_thres[,1]
  if (!is.numeric(wght_thres)) stop("'wght_thres' must be a numeric vector")
 
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")
  
  # id
  if (is.null(id)) id <- 1:n
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
  
  # gender
  if (!is.null(gender)) {
      if (!is.numeric(gender)) stop("'gender' must be numerical")
      if (length(gender) != n) stop("'gender' must be the same length as 'Y'")
      if (length(unique(gender)) != 2) stop("'gender' must be exactly two values")
      if (!all.equal(unique(gender),c(1, 2))) stop("'gender' must be value 1 for male, 2 for females")
   }

  # sort
  if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
        stop("'sort' must be a vector or ordered factor") }
  if (!is.null(sort) && length(sort) != n) stop("'sort' must have the same length as 'Y'")  

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
  }
  

   # period1
   period1 <- data.table(t(period1), check.names=TRUE)
   if (nrow(period1) != 1) stop("'period1' must be 1 row")
   if (ncol(period1) != ncol(periods)) stop("'period1' column and 'periods' row count must be equal")
   setnames(period1, names(period1), names(periods))
   if (any(is.na(period1))) stop("'period1' has unknown values")
   setkeyv(period1, names(periods))
   periodss <- copy(periods)
   periodss[,periodss:=1]
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

   data <- vardcrospoor(Y=Y, age=age, pl085=pl085,
                        month_at_work=month_at_work,
                        Y_den=Y_den, Y_thres=Y_thres,
                        H=H, PSU=PSU, w_final=w_final,
                        id=id, Dom=Dom, country=country,
                        periods=periods, sort=sort, 
                        gender = NULL,
                        percentage=percentage,
                        order_quant=order_quant,
                        alpha = alpha,
                        dataset = NULL,
                        use.estVar = FALSE,
                        withperiod = TRUE,
                        netchanges = TRUE,
                        confidence=confidence,
                        several.ok=several.ok,
                        type=type)

  crossectional_results <- data$results
  Dom <- names(Dom)
  H <- names(H)
  Y <- names(Y)
  country <- names(country)
  np <- ifelse(!is.null(Dom), length(Dom), 0)
  np <- 2 + np
  N <- namesY <- PSU <- w_final <- id <- NULL
  
  dataset <- rot01 <- rot02 <- stratasf <- name1 <- num1 <- NULL
  num1num1 <- num2num2 <- num1num2 <- num2 <- num1_1 <- NULL
  C13 <- grad1 <- grad2 <- num1_2 <- estim <- estim_1 <- NULL
  var_1 <- var_2 <- typs <- estim_2 <- se <- rse <- cv <- NULL

  var_grad <- copy(crossectional_results)
  var_grad[, (c("se", "rse", "cv")):=NULL]
  var_grad[, (c("count_respondents", "pop_size")):=NULL]

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

  fit <- lapply(1:nrowv, function(i) {
            fits <- lapply(split(data, data[[country]]), function(DT3c) {
                      y1 <- paste0(period[i], "_1")
                      y2 <- paste0(period[i], "_2")
                      vect <- c("rot01*", "rot02*", "rot01*rot02*")

                      funkc <- as.formula(paste0("cbind(", trim(toString(y1)), ", ", 
                                                           trim(toString(y2)), ")~-1+",
                                                           paste(t(unlist(lapply(dataH, function(x) 
                                                                     paste0("rot01*", toString(x), "+",
                                                                            "rot02*", toString(x), "+",
                                                                            "rot01*rot02*", toString(x))))),
                                                                             collapse= "+"))) 
                      res <- lm(funkc, data=DT3c)
                      
                      if (use.estVar) { res <- data.table(estVar(res))
                                  } else res <- data.table(lm(funkc, data=DT3c)$res)
                      setnames(res, names(res), c("num1", "num2"))

                      res[, namesY:=period[i]]

                      if (use.estVar) { 
                           res[, num1num1:=res[["num1"]][1]]
                           res[, num2num2:=res[["num2"]][2]]
                           res[, num1num2:=res[["num1"]][2]]
                           res <- data.table(res[1], DT3c[1])
                        } else {
                            res[, num1num1:=num1 * num1]
                            res[, num2num2:=num2 * num2]
                            res[, num1num2:=num1 * num2]
                            res <- data.table(res, DT3c)}

                      keynames <- c(country, "namesY")
                      fits <- res[, lapply(.SD, sum), keyby=keynames,
                                      .SDcols=c("num1num1", "num2num2", "num1num2")]
                      return(fits)
                })
            rbindlist(fits)      
        })
   res <- rbindlist(fit)

   set(res, j=country, value=as.character(res[[country]]))

   if (!is.null(Dom)) {
          var_grad[, paste0(Dom, "_ss"):=lapply(Dom, function(x) make.names(paste0(x,".", get(x))))]
          var_grad[, paste0(Dom[1], "_ss"):=paste0("Dom.", get(paste0(Dom[1], "_ss")))]
          var_grad[, typs:=paste0("lin_", tolower(type))]
          var_grad[, namesY:=Reduce(function(x, y)
                                      paste(x, y, sep = "__"), .SD),
                                     .SDcols=c("typs", paste0(Dom, "_ss"))]
       }

   setkeyv(res, c(country, "namesY"))
   setkeyv(var_grad, c(country, "namesY"))
   data <- merge(res, var_grad, all=TRUE)
   res <- fit <- var_gr <- NULL
   data[, (c("namesY", "typs", paste0(Dom, "_ss"))):=NULL]

   data[, C13:=sqrt(var_1*var_2/(num1num1*num2num2))*num1num2]

   data[, estim:=estim_1 - estim_2]
   data[, var:=var_1 + var_2 - 2 * C13]

   data[, se:=sqrt(var)]
   data[, rse:=se/estim]
   data[, cv:=rse*100]

   changes_results <- data[, c(country, Dom, "type", "estim",
                               "var", "se", "rse", "cv"), with=FALSE]

 list(crossectional_results=crossectional_results, changes_results=changes_results)
}   
