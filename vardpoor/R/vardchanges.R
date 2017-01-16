vardchanges <- function(Y, H, PSU, w_final,
                        ID_level1, ID_level2,
                        Dom = NULL, Z = NULL, 
                        country = NULL, period,
                        dataset = NULL,
                        period1, period2,
                        X = NULL, countryX = NULL,
                        periodX = NULL, X_ID_level1 = NULL,
                        ind_gr = NULL, g = NULL,
                        q = NULL, datasetX = NULL,
                        annual = FALSE,
                        linratio = FALSE,
                        percentratio = 1,
                        use.estVar = FALSE,
                        outp_res = FALSE,
                        confidence = 0.95,
                        change_type = "absolute") {
 
  ### Checking

  if (!change_type %in% c("absolute", "relative")) stop("'change_type' must be 'absolute' or 'relative'")
  if (length(linratio) != 1 | !any(is.logical(linratio))) stop("'linratio' must be logical")
  if (length(annual) != 1 | !any(is.logical(annual))) stop("'annual' must be logical")
  if (length(percentratio) != 1 | !any(is.numeric(percentratio) | percentratio > 0)) stop("'percentratio' must be a numeric integer value")
  if (length(use.estVar) != 1 | !any(is.logical(use.estVar))) stop("'use.estVar' must be logical")
  if (length(outp_res) != 1 | !any(is.logical(outp_res))) stop("'outp_res' must be logical")
  if(length(confidence) != 1 | any(!is.numeric(confidence) |  confidence < 0 | confidence > 1)) {
          stop("'confidence' must be a numeric value in [0, 1]")  }

  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset)) == 1)  Y <- dataset[, Y, with = FALSE]

      if(!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE] }

      if(!is.null(PSU)) {
          if (min(PSU %in% names(dataset)) != 1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset)) == 1) PSU <- dataset[, PSU, with = FALSE] }

      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset)) != 1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset)) == 1) w_final <- dataset[, w_final, with = FALSE] }

      if(!is.null(ID_level1)) {
          if (min(ID_level1 %in% names(dataset)) != 1) stop("'ID_level1' does not exist in 'dataset'!")
          if (min(ID_level1 %in% names(dataset)) == 1) ID_level1 <- dataset[, ID_level1, with = FALSE]  }

      if(!is.null(ID_level2)) {
          if (min(ID_level2 %in% names(dataset)) != 1) stop("'ID_level2' does not exist in 'dataset'!")
          if (min(ID_level2 %in% names(dataset)) == 1) ID_level2 <- dataset[, ID_level2, with = FALSE]  }   

      if(!is.null(Z)) {
          if (min(Z %in% names(dataset)) != 1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset)) == 1) Z <- dataset[, Z, with = FALSE]}

      if(!is.null(country)) {
          if (min(country %in% names(dataset)) != 1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset)) == 1) country <- dataset[, country, with = FALSE] }

      if(!is.null(period)) {
          if (min(period %in% names(dataset)) != 1) stop("period' does not exist in 'dataset'!")
          if (min(period %in% names(dataset)) == 1) period <- dataset[, period, with = FALSE] }
     
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'Dom1' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE]    }
   }

   if (is.null(datasetX)) datasetX <- copy(dataset)
   if(!is.null(datasetX)) {
          datasetX <- data.table(datasetX)
          if (!is.null(countryX)) {
               if (min(countryX %in% names(datasetX)) != 1) stop("'countryX' does not exist in 'datasetX'!")
               if (min(countryX %in% names(datasetX)) == 1) countryX <- datasetX[, countryX,  with = FALSE] }

          if (!is.null(periodX)) {
               if (min(periodX %in% names(datasetX)) != 1) stop("'periodX' does not exist in 'datasetX'!")
               if (min(periodX %in% names(datasetX)) == 1) periodX <- datasetX[, periodX,  with = FALSE] }

          if (!is.null(X_ID_level1)) {
               if (min(X_ID_level1 %in% names(datasetX)) != 1) stop("'ID_level1' does not exist in 'datasetX'!")
               if (min(X_ID_level1 %in% names(datasetX)) == 1) X_ID_level1 <- datasetX[, X_ID_level1,  with = FALSE]  }

          if(!is.null(X)) {
              if (min(X %in% names(datasetX)) != 1) stop("'X' does not exist in 'datasetX'!")
              if (min(X %in% names(datasetX)) == 1) X <- datasetX[, X,  with = FALSE] }

          if(!is.null(ind_gr)) {
              if (min(ind_gr %in% names(datasetX)) != 1) stop("'ind_gr' does not exist in 'datasetX'!")
              if (min(ind_gr %in% names(datasetX)) == 1) ind_gr <- datasetX[, ind_gr,  with = FALSE] }     
              
          if(!is.null(g)) {
              if (min(g %in% names(datasetX)) != 1) stop("'g' does not exist in 'datasetX'!")
              if (min(g %in% names(datasetX)) == 1) g <- datasetX[, g,  with = FALSE] }

          if(!is.null(q)) {
              if (min(q %in% names(datasetX)) != 1) stop("'q' does not exist in 'datasetX'!") 
              if (min(q %in% names(datasetX)) == 1) q <- datasetX[, q,  with = FALSE] } 
     }

  equal_dataset <- identical(dataset, datasetX) & !is.null(dataset) & !is.null(X)
  dataset <- datasetX <- NULL
  if (equal_dataset) X_ID_level1 <- ID_level1
  if (equal_dataset) countryX <- country

  # Y
  Y <- data.table(Y, check.names = TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (anyNA(Y)) stop("'Y' has missing values")
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric")
  if (any(grepl("__", names(Y)))) stop("'Y' is not allowed column names with '__'")

  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  H[, (names(H)) := lapply(.SD, as.character)]
  if (anyNA(H)) stop("'H' has missing values")

  # PSU
  PSU <- data.table(PSU)
  if (nrow(PSU) !=  n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) !=  1) stop("'PSU' has more than 1 column")
  PSU[, (names(PSU)) := lapply(.SD, as.character)]
  if (anyNA(PSU)) stop("'PSU' has missing values")
  
  # w_final
  w_final <- data.frame(w_final)
  if (anyNA(w_final)) stop("'w_final' has missing values") 
  if (nrow(w_final) !=  n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) !=  1) stop("'w_final' must be a vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[, 1]
  if (!is.numeric(w_final)) stop("'w_final' must be numeric")
  
  # ID_level1
  if (is.null(ID_level1)) stop("'ID_level1' must be defined")
  ID_level1 <- data.table(ID_level1)
  ID_level1[, (names(ID_level1)) := lapply(.SD, as.character)]
  if (anyNA(ID_level1)) stop("'ID_level1' has missing values")
  if (ncol(ID_level1) != 1) stop("'ID_level1' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_level1) != n) stop("'ID_level1' must be the same length as 'Y'")
  if (names(ID_level1) == names(PSU)) setnames(PSU, names(PSU), paste0(names(PSU), "_PSU")) 

  # ID_level2
  ID_level2 <- data.table(ID_level2)
  ID_level2[, (names(ID_level2)) := lapply(.SD, as.character)]
  if (anyNA(ID_level2)) stop("'ID_level2' has missing values")
  if (nrow(ID_level2) != n) stop("'ID_level2' length must be equal with 'Y' row count")
  if (ncol(ID_level2) != 1) stop("'ID_level2' must be 1 column data.frame, matrix, data.table")
  if (names(ID_level2) == names(ID_level1)) setnames(ID_level2, names(ID_level2), paste0(names(ID_level2), "_id"))

  # country
  if (!is.null(country)){
        country <- data.table(country)
        country[, (names(country)) := lapply(.SD, as.character)]
        if (anyNA(country)) stop("'country' has missing values")
        if (names(country) == "percoun") stop("'country' must be different name")
        if (nrow(country) != n) stop("'country' length must be equal with 'Y' row count")
        if (ncol(country) != 1) stop("'country' has more than 1 column")
    } 

  # period
  period <- data.table(period, check.names = TRUE)
  period[, (names(period)) := lapply(.SD, as.character)]
  if (anyNA(period)) stop("'period' has missing values")
  if (names(period) == "percoun") stop("'period' must be different name")
  if (nrow(period) != n) stop("'period' length must be equal with 'Y' row count")

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    Dom[, (names(Dom)) := lapply(.SD, as.character)]
    if (anyNA(Dom)) stop("'Dom' has missing values")
    if (any(grepl("__", names(Dom)))) stop("'Dom' is not allowed column names with '__'")
  }
  
  namesZ <- NULL
  if (!is.null(Z)) {
    Z <- data.table(Z, check.names=TRUE)
    if (anyNA(Z)) stop("'Z' has missing values")
    if (nrow(Z) !=  n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) !=  m) stop("'Z' and 'Y' must be equal column count")
    if (any(grepl("__", names(Z)))) stop("'Z' is not allowed column names with '__'")
    namesZ <- names(Z)
  }
 
   # period1
   period1 <- data.table(period1, check.names = TRUE)
   if (ncol(period1) != 1) stop("'period1' must be 1 column")
   setnames(period1, names(period1), names(period))
   period1[, (names(period1)) := lapply(.SD, as.character)]
   if (anyNA(period1)) stop("'period1' has missing values")
   periods <- copy(period)
   periods[, periods := 1]
   if (anyNA(merge(period1, periods, all.x = TRUE,
                        by = names(period), allow.cartesian = TRUE)))
              stop("'period1' row must be exist in 'period'")


   # period2
   period2 <- data.table(period2, check.names=TRUE)
   if (ncol(period2) != 1) stop("'period2' must be 1 column")
   setnames(period2, names(period2), names(period))
   period2[, (names(period2)) := lapply(.SD, as.character)]
   if (anyNA(period2)) stop("'period2' has missing values")
   if (anyNA(merge(period2, periods, all.x = TRUE,
                         by = names(period), allow.cartesian = TRUE)))
             stop("'period2' row must be exist in 'period'")

  if (!is.null(X)) {
    X <- data.table(X, check.names = TRUE)
    if (!all(sapply(X, is.numeric))) stop("'X' must be numeric values")
  }

  # countryX
  if (!is.null(X)) {
    if(!is.null(countryX)) {
        countryX <- data.table(countryX)
        if (nrow(countryX) != nrow(X)) stop("'countryX' length must be equal with 'X' row count")
        if (ncol(countryX) != 1) stop("'countryX' has more than 1 column")
        countryX[, (names(countryX)) := lapply(.SD, as.character)]
        if (anyNA(countryX)) stop("'countryX' has missing values")
        if (names(countryX) != names(country)) stop("'countryX' must be equal with 'country' names")
        countrX <- countryX[, .N, keyby = names(countryX)][, N := NULL]
        countr <- country[, .N, keyby = names(country)][, N := NULL]
        if (any(countr != countrX)) stop("'unique(country)' and 'unique(countryX)' records have different")
     } else if (!is.null(country)) stop("'countryX' must be defined")
  }

  # periodX
  if (!is.null(X)) {
     if (is.null(periodX)) stop("'periodX' must be defined")
     periodX <- data.table(periodX)
     periodX[, (names(periodX)) := lapply(.SD, as.character)]
     if (anyNA(periodX)) stop("'periodX' has missing values")
     if (any(duplicated(names(periodX)))) 
                 stop("'periodX' are duplicate column names: ", 
                        paste(names(periodX)[duplicated(names(periodX))], collapse = ","))
     if (nrow(periodX) != nrow(X)) stop("'periodX' length must be equal with 'X' row count")
     if (ncol(periodX) != ncol(period)) stop("'periodX' length must be equal with 'period' column count")
     if (names(periodX) != names(period)) stop("'periodX' must be equal with 'periods' names")
     peri <- copy(period)
     periX <- copy(periodX)
     if (!is.null(country)) peri <- data.table(country, peri)
     if (!is.null(countryX)) periX <- data.table(countryX, periX)
     periX <- periX[, .N, keyby = names(periX)][, N := NULL]
     peri <- peri[, .N, keyby = names(peri)][, N := NULL]
     if (any(peri != periX) & is.null(country)) stop("'unique(period)' and 'unique(periodX)' records have different")
     if (any(peri != periX) & !is.null(country)) stop("'unique(country, period)' and 'unique(countryX, periodX)' records have different")
   } 


  # X_ID_level1
  if (!is.null(X)) {
    X_ID_level1 <- data.table(X_ID_level1)
    X_ID_level1[, (names(X_ID_level1)) := lapply(.SD, as.character)]
    if (anyNA(X_ID_level1)) stop("'X_ID_level1' has missing values")
    if (nrow(X) != nrow(X_ID_level1)) stop("'X' and 'X_ID_level1' have different row count")
    if (ncol(X_ID_level1) != 1) stop("'X_ID_level1' must be 1 column data.frame, matrix, data.table")
 
    ID_level1h <- copy(ID_level1)
    if (!is.null(countryX)) { X_ID_level1 <- data.table(countryX, X_ID_level1)
                              ID_level1h <- data.table(country, ID_level1h)}
    X_ID_level1 <- data.table(periodX, X_ID_level1)
    ID_level1h <- data.table(period, ID_level1h)
    ID_level1h <- ID_level1h[, .N, by = names(ID_level1h)][, N := NULL]
    if (nrow(X_ID_level1[, .N, by = names(X_ID_level1)][N > 1]) > 0) stop("'X_ID_level1' have duplicates")
    setkeyv(X_ID_level1, names(X_ID_level1))
    setkeyv(ID_level1h, names(ID_level1h))
    nperIDh <- names(ID_level1h)
    if (any(nperIDh != names(X_ID_level1))) stop("'X_ID_level1' and 'ID_level1' must be equal names")
    if (ID_level1h[, class(get(nperIDh))] != X_ID_level1[, class(get(nperIDh))])  stop("Class for 'X_ID_level1' and class for 'ID_level1' must be equal ")

    if (!is.null(country)) {
          if (nrow(ID_level1h) != nrow(X_ID_level1)) stop("'unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' have different row count")
          if (any(ID_level1h != X_ID_level1)) stop("''unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' records have different")
       } else {
          if (nrow(ID_level1h) != nrow(X_ID_level1)) stop("'unique(periodX, X_ID_level1)' and 'unique(period, ID_level1)' have different row count")
          if (any(ID_level1h != X_ID_level1)) stop("''unique(periodX, X_ID_level1)' and 'unique(period, ID_level1)' records have different")  }
  }

  # ind_gr
  if (!is.null(X)) {
     if(is.null(ind_gr)) ind_gr <- rep("1", nrow(X)) 
     ind_gr <- data.table(ind_gr)
     if (nrow(ind_gr) != nrow(X)) stop("'ind_gr' length must be equal with 'X' row count")
     if (ncol(ind_gr) != 1) stop("'ind_gr' must be 1 column data.frame, matrix, data.table")
     ind_gr[, (names(ind_gr)) := lapply(.SD, as.character)]
     if (anyNA(ind_gr)) stop("'ind_gr' has missing values")
   }

  # X
  if (!is.null(X)) {
       X1 <- data.table(X, check.names = TRUE)
       nX1 <- names(X1)
       ind_gr1 <- data.table(periodX, ind_gr, check.names = TRUE)
       X2 <- data.table(ind_gr1, X1)
       X1 <- X2[, .N, keyby = names(ind_gr1)][["N"]]
       X2 <- X2[, lapply(.SD, function(x) sum(!is.na(x))), keyby = names(ind_gr1), .SDcols = nX1]
       X2 <- X2[, nX1, with = FALSE]

       if (!all(X2 == 0 | X1 == X2)) stop("X has missing values")
       ind_gr1 <- nX1 <- X1 <- X2 <- NULL
    }

  # g
  if (!is.null(X)) {
    if (is.null(class(g)) | all(class(g) == "function")) stop("'g' must be numeric")
    g <- data.frame(g)
    if (anyNA(g)) stop("'g' has missing values")
    if (nrow(g) != nrow(X)) stop("'g' length must be equal with 'X' row count")
    if (ncol(g) != 1) stop("'g' must be 1 column data.frame, matrix, data.table")
    g <- g[, 1]
    if (!is.numeric(g)) stop("'g' must be numeric")
    if (any(g == 0)) stop("'g' value can not be 0")
   }
    
  # q
  if (!is.null(X)) {
    if (is.null(q))  q <- rep(1, nrow(X))
    if (is.null(class(q)) | all(class(q) == "function")) stop("'q' must be numeric")
    q <- data.frame(q)
    if (anyNA(q)) stop("'q' has missing values")
    if (nrow(q) != nrow(X)) stop("'q' length must be equal with 'X' row count")
    if (ncol(q) != 1) stop("'q' must be 1 column data.frame, matrix, data.table")
    q <- q[, 1]
    if (!is.numeric(q)) stop("'q' must be numeric")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }

   datas <- vardcros(Y = Y, H = H, PSU = PSU, w_final = w_final,
                     ID_level1 = ID_level1, ID_level2 = ID_level2,
                     Dom = Dom, Z = Z, country = country,
                     period = period, dataset = NULL,
                     countryX = countryX, periodX = periodX,
                     X_ID_level1 = X_ID_level1, ind_gr = ind_gr,
                     g = g, q = q, datasetX = NULL,
                     linratio = linratio,
                     percentratio = percentratio,
                     use.estVar = use.estVar,
                     ID_level1_max = TRUE,
                     outp_res = outp_res,
                     withperiod = TRUE,
                     netchanges = TRUE, 
                     confidence = confidence)
  res_out <- datas$res_out
  crossectional_results <- datas$results

  countryX <- periodX <- X_ID_level1 <- NULL
  X_ID_level1 <- ind_gr <- g  <- q  <- NULL

  if (!is.null(Dom)) { 
        Y1 <- names(domain(Y, Dom))
        if (!is.null(Z)) Z1 <- names(domain(Z, Dom))
   } else { Y1 <- names(Y)
            Z1 <- names(Z) }
  
  names_country <- names(country)
  country <- ifelse(!is.null(names_country), names_country, "percoun")
  per <- names(period)
  Dom <- names(Dom)
  PSU <- names(PSU)
  H <- names(H)
  Y <- names(Y)
  Z <- names(Z)
  sarp <- c(country, H, PSU)
 
  namesY <- w_final <- ind <- dataset <- nameYs <- NULL  
  nameZs <- grad1 <- grad2 <- rot_1 <- rot_2 <- NULL
  rot_1_rot_2 <- stratasf <- name1 <- num1 <- NULL
  num1num1 <- den1den1 <- den1 <- num2num2 <- NULL
  den2den2 <- den2 <- num1den1 <- num1num2 <- NULL
  num2 <- num1den2 <- den1num2 <- den1den2 <- num2den2 <- NULL
  num1_1 <- den1_1 <- num1den1 <- den1den1 <- num1_2 <- NULL
  den1_2 <- estim <- estim_1 <- estim_2 <- NULL
  grad1_1 <- grad1_2 <- CI_upper <- grad2_1 <- NULL
  ids_nr <- rot <- grad2_2 <- se <-  CI_lower <-  NULL
  valueY1_1 <- valueZ1_1 <- valueY1_2 <- valueZ1_2 <- NULL
  nh <- period_country_1 <- period_country_2 <- NULL
  nhcor <- significant <- id_nams <- nams <- NULL
  ids_nr <- N <- percoun <- NULL

  var_grad <- datas$var_grad
  cros_var_grad <- copy(var_grad)
  per1 <- paste0(per, "_1")
  per2 <- paste0(per, "_2")
  period1[, ind := .I] 
  period2[, ind := .I]
  setnames(period1, per, per1)
  setnames(period2, per, per2)
  period1 <- merge(period1, period2, by = "ind")
  period2 <- NULL
  var_grad1 <- merge(period1, var_grad, all.x = TRUE,
                              by.x = per1, by.y = per,
                              allow.cartesian = TRUE)
  var_grad2 <- merge(period1, var_grad, all.x = TRUE,
                              by.x = per2, by.y = per,
                              allow.cartesian = TRUE)
  
  sarc <- c("ind", per1, per2, country, Dom, "namesY", "namesZ")
  sarc <- sarc[sarc %in% names(var_grad1)]
  sar <- names(var_grad1)[!(names(var_grad1) %in% sarc)]
  setnames(var_grad1, sar, paste0(sar, "_1"))
  setnames(var_grad2, sar, paste0(sar, "_2"))
  var_grad <- merge(var_grad1, var_grad2, all = TRUE, by = sarc)
  var_grad[, ids_nr := 1 : .N]

  if (change_type == "relative"){
        if (!linratio & !is.null(Z)){
             var_grad[, grad1_1 := - valueY1_2 * valueZ1_1 / (valueZ1_2 * (valueY1_1)^2)]
             var_grad[, grad1_2 := valueY1_2 / (valueZ1_2 * valueY1_1)]
             var_grad[, grad2_1 := valueZ1_1 / (valueZ1_2 * valueY1_1)]
             var_grad[, grad1_1 := - valueY1_2 * valueZ1_1 / ((valueZ1_2)^2 * valueY1_1)]
          } else {
             var_grad[, grad1_1 := - valueY1_2 / (valueY1_1)^2]
             var_grad[, grad1_2 := 1 / valueY1_1] }
     } else {
        if (!is.null(var_grad$grad1_1)){
                var_grad[, grad1_1 := - grad1_1]
                var_grad[, grad2_1 := - grad2_1] 
           } else {var_grad[, grad1_1 := - 1]
                   var_grad[, grad1_2 := 1] }}

  var_grad11 <- copy(var_grad)
  var_grad12 <- copy(var_grad)
  var_grad11[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad1_1, num1_1, 1, "num1")]
  var_grad12[, (c("grad", "cros_var", "nams")) := list(grad1_2, num1_2, "num2")]
  var_grad12[, id_nams := 2 + as.numeric(!is.null(var_grad$grad2_1))]

  var_grad21 <- var_grad22 <- NULL
  if (!is.null(var_grad$grad2_1)) {
        var_grad21 <- copy(var_grad)
        var_grad22 <- copy(var_grad)
        var_grad21[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad2_1, den1_1, 2, "den1")]
        var_grad22[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad2_2, den1_2, 4, "den2")]
   }
  var_gradn <- rbindlist(list(var_grad11, var_grad12,
                              var_grad21, var_grad22), fill = TRUE)

  var_gradn <- var_gradn[, c(sarc, "ids_nr", "id_nams", 
                             "nams", "grad", "cros_var"), with = FALSE]

  var_grad11 <- var_grad12 <- NULL
  var_grad21 <- var_grad22 <- NULL



  data <- datas$data_net_changes
  data[, rot := 1]
  data1 <- merge(period1, data, all.x = TRUE,
                    by.x = per1, by.y = per,
                    allow.cartesian = TRUE)
  data2 <- merge(period1, data, all.x = TRUE, 
                    by.x = per2, by.y = per,
                    allow.cartesian = TRUE)
  sard <- names(data)[!(names(data) %in% c(sarp, per))]
  setnames(data1, sard, paste0(sard, "_1"))
  setnames(data2, sard, paste0(sard, "_2"))
  data <- merge(data1, data2, all = TRUE, by = c("ind", per1, per2, sarp))

  data[is.na(period_country_2), period_country_1 := paste0(get(per1), "_",
                                                    ifelse(names_country == "country", country, get(country)))]
  data[is.na(period_country_2), period_country_2 := paste0(get(per2), "_", 
                                                    ifelse(names_country == "country", country, get(country)))]
  data1 <- data2 <- NULL

  recode.NA <- function(DT, cols = seq_len(ncol(DT))) {
     for (j in cols) if (is.numeric(DT[[j]]))
      set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]), 0L, 0))
   }

  data[, nh := .N, by = c("ind", "period_country_1", "period_country_2", H)]

  dataH <- data[[H]]
  dataH <- factor(dataH)
  if (length(levels(dataH)) == 1) { data[, stratasf := 1]
                                  dataH <- "stratasf"
                         } else { dataH <- data.table(model.matrix( ~ dataH - 1))
                                  data <- cbind(data, dataH)
                                  dataH <- names(dataH) }
  den1 <- den2 <- NULL
  sard <- sard[!(sard %in% "period_country")]
  recode.NA(data, c(paste0(sard, "_1"), paste0(sard, "_2")))

  fit <- lapply(1 : length(Y1), function(i) {
       fitd <- lapply(split(data, data[["ind"]]), function(data1) {
            fits <- lapply(split(data1, data1[[country]]), function(DT3c) {

                      y1 <- paste0(Y1[i], "_1")
                      y2 <- paste0(Y1[i], "_2")
                      if (!is.null(namesZ) & !linratio) { 
                                    z1 <- paste0(",", Z1[i], "_1") 
	                              z2 <- paste0(",", Z1[i], "_2") 
                               } else z1 <- z2 <- ""

                      funkc <- as.formula(paste0("cbind(", y1, z1, ", ", 
                                                           y2, z2, ") ~ 0 + ",
                                                           paste(t(unlist(lapply(dataH, function(x) 
                                                                     paste0("rot_1 : ", toString(x), "+",
                                                                            "rot_2 : ", toString(x), "+",
                                                                            "rot_1 : rot_2 : ", toString(x))))),
                                                                            collapse= "+")))
                      res <- lm(funkc, data = DT3c)
                      if (use.estVar) { res <- data.table(estVar(res))
                                  } else res <- data.table(res$res)

                      if (!is.null(namesZ) & !linratio) { 
                                   setnames(res, names(res), c("num1", "den1", "num2", "den2"))
                                   res[, nameZs := Z1[i]]
                            } else setnames(res, names(res), c("num1", "num2"))

                      nosv <- c("num1", "den1", "num2", "den2")
                      nosv <- names(res)[names(res) %in% nosv]
                      Zvn <- as.integer(!is.null(namesZ) & !linratio)
                      res[, nameYs := Y1[i]]

                      keynames <- c(country, "ind", paste0(per, "_1"),
                                    paste0(per, "_2"), "nameYs", "nameZs")
                      keynames <- keynames[keynames %in% c(names(DT3c), names(res))]

                      if (use.estVar) { 
                            res <- data.table(id_nams = 1 : nrow(res), nams = nosv, res, DT3c[1])
                        } else {
                            res <- data.table(res, DT3c)
                            if (annual) { res[, nhcor:=ifelse(nh > 1, nh / (nh - 1), 1)]
                                        } else res[, nhcor := 1]

                            res[, num1num1 := num1 * num1 * nhcor]
                            res[, num2num2 := num2 * num2 * nhcor]
                            res[, num1num2 := num1 * num2 * nhcor]
                            res[, id_nams := 0]
                            res[, nams := ""]
                            if (!is.null(namesZ) & !linratio) {
                                  res[, den1den1 := den1 * den1 * nhcor]
                                  res[, den2den2 := den2 * den2 * nhcor]
                                  res[, num1den1 := num1 * den1 * nhcor]
                                  res[, num1den2 := num1 * den2 * nhcor]
                                  res[, den1num2 := den1 * num2 * nhcor]
                                  res[, den1den2 := den1 * den2 * nhcor]
                                  res[, num2den2 := num2 * den2 * nhcor] }
      
                            varsp <- c("num1num1", "den1den1",
                                       "num2num2", "den2den2",
                                       "num1den1", "num1num2",
                                       "num1den2", "den1num2",
                                       "den1den2", "num2den2")
                            varsp <- varsp[varsp %in% names(res)]
                            fits <- res[, lapply(.SD, sum), keyby = c(keynames,
                                                              "id_nams", "nams"),
                                                           .SDcols = varsp]
                            fits1 <- copy(fits)
                            fits1[, (c("id_nams", "nams")) := list(1, "num1")]
                            setnames(fits1, (c("num1num1", "num1num2")), c("num1", "num2"))

                            fits2 <- copy(fits)
                            fits2[, id_nams := 2 + as.numeric(!is.null(fits$den2den2))]
                            fits2[, nams := "num2"]
                            setnames(fits2, c("num1num2", "num2num2"), c("num1", "num2"))

                            fits3 <- fits4 <- NULL
                             if (!is.null(fits$den2den2)){
                                 setnames(fits1, c("num1den1", "num1den2"), c("den1", "den2"))
                                 setnames(fits2, c("den1num2", "num2den2"), c("den1", "den2"))

                                 fits3 <- copy(fits)
                                 fits3[, (c("id_nams", "nams")) := list(2, "den1")]
                                 setnames(fits3, c("num1den1", "den1num2",
                                                   "den1den1", "den1den2"),
                                                 c("num1", "num2", "den1", "den2"))
  
                                 fits4 <- copy(fits)
                                 fits4[, (c("id_nams", "nams")) := list(4, "den2")]
                                 setnames(fits4, c("num1den2", "num2den2",
                                                   "den1den2", "den2den2"),
                                                 c("num1", "num2", "den1", "den2"))
                               }
                            res <- rbindlist(list(fits1, fits2, fits3, fits4), fill = TRUE)
                            fits <- fits1 <- fits2 <- fits3 <- fits4 <- NULL
                        }
                      fits <- res[, lapply(.SD, sum),
                                     keyby = c(keynames, "id_nams", "nams"),
                                    .SDcols = nosv]
                     return(fits)
                })
            rbindlist(fits)
         })
       rbindlist(fitd)
     })
   res <- rbindlist(fit)

   set(res, j = country, value = as.character(res[[country]]))

   if (!is.null(Dom)) {
          var_gradn[, paste0(Dom, "_ss"):=lapply(Dom, function(x) paste0(x,".", get(x)))]
          var_gradn[, nameYs:=Reduce(function(x, y)
                                      paste(x, y, sep = "__"), .SD),
                                     .SDcols=c("namesY", paste0(Dom, "_ss"))]
          if (!is.null(namesZ)) { var_gradn[, nameZs := Reduce(function(x, y)
                                                                 paste(x, y, sep = "__"), .SD),
                                                               .SDcols = c("namesZ", paste0(Dom, "_ss"))]
                                }
       } else { var_gradn[, nameYs := namesY]
                if (!is.null(namesZ)) var_gradn[, nameZs := namesZ]}

   nameYZ <- c("nameYs", "nameZs")
   nameYZ <- nameYZ[nameYZ %in% names(res)]

   sars <- c(country, "ind", paste0(per, "_1"), 
             paste0(per, "_2"), nameYZ)

   data <- merge(res, var_gradn, all = TRUE, by = c(sars, "id_nams", "nams"))
   res <- fit <- var_gradn <- NULL

   rmax <- max(data[, .N, by = "ids_nr"][["ids_nr"]])

   nosv <- c("num1", "den1", "num2", "den2")
   nosv <- names(data)[names(data) %in% nosv]

   dat <- lapply(1:rmax, function(i) {
             res <- data[ids_nr == i]
             res1 <- as.matrix(res[, nosv, with = FALSE])
             rhod <- diag(sqrt(1 / diag(res1)), length(nosv), length(nosv))
             rhod <- data.table((t(rhod) %*% res1) %*% rhod)

             setnames(rhod, names(rhod), paste0("rho_", nosv))
             dmatr <- diag(sqrt(res[["cros_var"]] / diag(res1)),
                                length(nosv), length(nosv))
             var_tau <- data.table((t(dmatr) %*% res1) %*% dmatr) 
             dmatr <- data.table(dmatr)
             setnames(dmatr, names(dmatr), paste0("d_", nosv))
             setnames(var_tau, names(var_tau), paste0("var_tau_", nosv))
             res <- data.table(res, rhod, dmatr, var_tau)

             var_t <- (t(res[["grad"]]) %*% as.matrix(var_tau)) %*% res[["grad"]]
             var_t <- var_t[, 1]
             var_grads <- var_grad[ids_nr == i]

             if (change_type == "absolute") {
                          var_grads[, estim := estim_2 - estim_1]
                     } else var_grads[, estim := estim_2 / estim_1 * percentratio]

             var_grads[, var := var_t]
             list(matricas = res, data = var_grads) })

   matricas <- rbindlist(lapply(dat, function(x) x[[1]]))              
   datas <- rbindlist(lapply(dat, function(x) x[[2]]))

   if (change_type == "relative" | (!is.null(datas$namesZ) & !linratio)) { 
                  datas[, var:=var * (percentratio)^2] }

   datas[var >= 0, se := sqrt(var)]
   tsad <- qnorm(0.5 * (1 + confidence))
   datas[, CI_lower := estim - tsad * se]
   datas[, CI_upper := estim + tsad * se]

   sarc <- c(sarc, "nams")
   sarc <- sarc[!(sarc %in% "ind")]

   rho_matrix <- matricas[, c(sarc, paste0("rho_", nosv)), with = FALSE]
   var_tau <- matricas[, c(sarc, paste0("var_tau_", nosv)), with = FALSE]
   grad_var <- matricas[, c(sarc, "grad", "cros_var"), with = FALSE]

   namesYZ <- c("namesY", "namesZ")
   namesYZ <- names(datas)[(names(datas) %in% namesYZ)]
   

   changes_results <- datas[, c(paste0(per,"_", c(1, 2)), country, Dom,
                                namesYZ, "estim_1",  "estim_2", "estim", 
                                "var", "se", "CI_lower", "CI_upper"), with = FALSE]

   changes_results[, significant := TRUE]
   boundss <- as.numeric(change_type == "relative")
   changes_results[CI_lower <= boundss & CI_upper >= boundss, significant := FALSE]
   
   if (!annual & is.null(names_country)) {
           crossectional_results[, percoun := NULL]
           cros_var_grad[, percoun := NULL]
           grad_var[, percoun := NULL]
           rho_matrix[, percoun := NULL]
           var_tau[, percoun := NULL]
           changes_results[, percoun := NULL]  }
  
   list(res_out = res_out,
        crossectional_results = crossectional_results,
        crossectional_var_grad = cros_var_grad,
        grad_var = grad_var,
        rho = rho_matrix,
        var_tau = var_tau,
        changes_results = changes_results)
 }