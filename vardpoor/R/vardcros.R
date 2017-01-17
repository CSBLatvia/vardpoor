vardcros <- function(Y, H, PSU, w_final,
                     ID_level1,
                     ID_level2,
                     Dom = NULL,
                     Z = NULL, 
                     country = NULL,
                     period,
                     dataset = NULL,
                     X = NULL,
                     countryX = NULL,
                     periodX = NULL,
                     X_ID_level1 = NULL,
                     ind_gr = NULL,
                     g = NULL,
                     q = NULL,
                     datasetX = NULL,
                     linratio = FALSE,
                     percentratio=1,
                     use.estVar = FALSE,
                     ID_level1_max = TRUE,
                     outp_res = FALSE,
                     withperiod = TRUE,
                     netchanges = TRUE,
                     confidence = .95) {
 
  ### Checking
  if (length(linratio) != 1 | !any(is.logical(linratio))) stop("'linratio' must be logical")
  if (length(percentratio) != 1 | !any(is.numeric(percentratio) | percentratio > 0)) stop("'percentratio' must be a positive numeric value")
  if (length(netchanges) != 1 | !any(is.logical(netchanges))) stop("'netchanges' must be logical")
  if (length(withperiod) != 1 | !any(is.logical(withperiod))) stop("'withperiod' must be logical")
  if (length(use.estVar) != 1 | !any(is.logical(use.estVar))) stop("'use.estVar' must be logical")
  if (length(ID_level1_max) != 1 | !any(is.logical(ID_level1_max))) stop("'ID_level1_max' must be logical")
  if (length(outp_res) != 1 | !any(is.logical(outp_res))) stop("'outp_res' must be logical")
  if (all(ID_level1_max, !is.null(X))) stop("'ID_level1_max' must be ", !ID_level1_max, "!")

  if(length(confidence) != 1 | any(!is.numeric(confidence) |  confidence < 0 | confidence > 1)) {
          stop("'confidence' must be a numeric value in [0, 1]")  }
  if (all(!is.null(Z), !is.null(X), !linratio)) stop("'linratio' must be TRUE")
  if (all(is.null(Z), linratio)) stop("'linratio' must be FALSE")
  
  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset)) == 1) Y <- dataset[, Y, with = FALSE]

      if(!is.null(H)) {
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE] }

     if(!is.null(PSU)) {
          if (min(PSU %in% names(dataset)) != 1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset)) == 1) PSU <- dataset[, PSU, with = FALSE] }

      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset)) != 1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset)) == 1) w_final <- dataset[, w_final, with = FALSE] }

      if(!is.null(ID_level1)) {
          if (min(ID_level1 %in% names(dataset)) != 1) stop("'ID_level1' does not exist in 'dataset'!")
          if (min(ID_level1 %in% names(dataset)) == 1) ID_level1 <- dataset[, ID_level1, with = FALSE] }

      if(!is.null(ID_level2)) {
          if (min(ID_level2 %in% names(dataset)) != 1) stop("'ID_level2' does not exist in 'dataset'!")
          if (min(ID_level2 %in% names(dataset)) == 1) ID_level2 <- dataset[, ID_level2, with = FALSE] }

      if(!is.null(Z)) {
          if (min(Z %in% names(dataset)) != 1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset)) == 1) Z <- dataset[, Z, with = FALSE] }

      if(!is.null(country)) {
          if (min(country %in% names(dataset)) != 1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset)) == 1) country <- dataset[, country, with = FALSE] }

      if(!is.null(period)) {
          if (min(period %in% names(dataset)) != 1) stop("'period' does not exist in 'dataset'!")
          if (min(period %in% names(dataset)) == 1) period <- dataset[, period, with = FALSE] }
     
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE] }
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

  equal_dataset <- identical(dataset, datasetX) & !is.null(datasetX) & !is.null(X)
  if (equal_dataset) X_ID_level1 <- ID_level1
  if (equal_dataset) countryX <- country

  N <- dataset <- datasetX <- NULL
  
  # Y
  Y <- data.table(Y, check.names = TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric values")
  if (anyNA(Y)) stop("'Y' has missing values")
  if (is.null(names(Y))) stop("'Y' must have column names")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (names(H) == "dataH_stratas") stop("'H' must have different column name")
  H[, (names(H)) := lapply(.SD, as.character)]
  if (anyNA(H)) stop("'H' has missing values")

  # PSU
  PSU <- data.table(PSU)
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  PSU[, (names(PSU)) := lapply(.SD, as.character)]
  if (anyNA(PSU)) stop("'PSU' has missing values")
  
  # w_final 
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be a vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[, 1]
  if (!is.numeric(w_final)) stop("'w_final' must be numeric values")
  if (anyNA(w_final)) stop("'w_final' has missing values") 
  
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
  if (withperiod) {
        period <- data.table(period)
        period[, (names(period)) := lapply(.SD, as.character)]
        if (anyNA(period)) stop("'period' has missing values")
        if (names(period) == "percoun") stop("'period' must be different name")
        if (nrow(period) != n) stop("'period' length must be equal with 'Y' row count")
    } else if (!is.null(period)) stop("'period' must be NULL for those data")

  # Dom
  namesDom <- NULL
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    namesDom <- names(Dom)
    Dom[, (namesDom) := lapply(.SD, as.character)]
    if (anyNA(Dom)) stop("'Dom' has missing values")
    if (any(grepl("__", namesDom))) stop("'Dom' is not allowed column names with '__'")
    Dom_agg <- Dom[,.N, keyby = namesDom][, N := NULL]
    Dom_agg1 <- Dom_agg[, lapply(namesDom, function(x) make.names(paste0(x, ".", get(x))))]
    Dom_agg1[, Dom  :=  Reduce(function(x, y) paste(x, y, sep = "__"), .SD)]
    Dom_agg <- data.table(Dom_agg, Dom_agg1[, "Dom",  with = FALSE])
  }
 
  # Z
  if (!is.null(Z)) {
    Z <- data.table(Z, check.names = TRUE)
    if (anyNA(Z)) stop("'Z' has missing values")
    if (!all(sapply(Z, is.numeric))) stop("'Z' must be numeric values")
    if (nrow(Z) != n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' must be equal column count")
    if (any(grepl("__", names(Z)))) stop("'Z' is not allowed column names with '__'")
  }
      
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
     if(!is.null(periodX)) {
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
      } else if (!is.null(period)) stop("'periodX' must be defined")
   } 


  # X_ID_level1
    if (!is.null(X)) {
      if (is.null(X_ID_level1)) stop("'X_ID_level1' must be defined")
      X_ID_level1 <- data.table(X_ID_level1)
      X_ID_level1[, (names(X_ID_level1)) := lapply(.SD, as.character)]
      if (anyNA(X_ID_level1)) stop("'X_ID_level1' has missing values")
      if (nrow(X) != nrow(X_ID_level1)) stop("'X' and 'X_ID_level1' have different row count")
      if (ncol(X_ID_level1) != 1) stop("'X_ID_level1' must be 1 column data.frame, matrix, data.table")
      if (any(names(X_ID_level1) != names(ID_level1))) stop("'X_ID_level1' and 'ID_level1' must be equal names")
      
      ID_level1h <- copy(ID_level1)
      X_ID_level1h <- copy(X_ID_level1)
      if (!is.null(countryX)) {X_ID_level1h <- data.table(countryX, X_ID_level1h)
                               ID_level1h <- data.table(country, ID_level1h)}
      if (!is.null(periodX)) {X_ID_level1h <- data.table(periodX, X_ID_level1h)
                              ID_level1h <- data.table(period, ID_level1h)}

      ID_level1h <- ID_level1h[, .N, by = names(ID_level1h)][, N := NULL]
      if (nrow(X_ID_level1h[,.N, by = names(X_ID_level1h)][N > 1]) > 0) stop("'X_ID_level1' have duplicates")  

      setkeyv(X_ID_level1h, names(X_ID_level1h))
      setkeyv(ID_level1h, names(ID_level1h))
      if (!is.null(period)) {
          if (!is.null(country)) {
                 if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' have different row count")
                 if (any(ID_level1h != X_ID_level1h)) stop("''unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' records have different")
              } else {
                 if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(periodX, X_ID_level1)' and 'unique(period, ID_level1)' have different row count")
                 if (any(ID_level1h != X_ID_level1h)) stop("''unique(periodX, X_ID_level1)' and 'unique(period, ID_level1)' records have different")  }
        } else {
          if (!is.null(country)) {
               if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(countryX, X_ID_level1)' and 'unique(country, ID_level1)' have different row count")
               if (any(ID_level1h != X_ID_level1h)) stop("''unique(countryX, X_ID_level1)' and 'unique(country, ID_level1)' records have different")
            } else {
               if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(X_ID_level1)' and 'unique(ID_level1)' have different row count")
               if (any(ID_level1h != X_ID_level1h)) stop("''unique(X_ID_level1)' and 'unique(ID_level1)' records have different") }
      }
      ID_level1h <- X_ID_level1h <- NULL
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
       ind_gr1 <- copy(ind_gr) 
       if (!is.null(periodX)) ind_gr1 <- data.table(periodX, ind_gr1, check.names = TRUE)
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
  
  # Calculation
      
  sar_nr <- N <- nameY <- nameZ <- variable <- NULL
  sample_size <- totalY <- totalZ <- Z1 <- percoun <- NULL

  # Design weights
  if (!is.null(X)) {
             idh <- data.table(ID_level1)
             idhx <- data.table(X_ID_level1)
             if (!is.null(countryX)) {idh <- data.table(country, idh)
                                      idhx <- data.table(countryX, idhx)}
             if (!is.null(periodX)) {idh <- data.table(period, idh)
                                     idhx <- data.table(periodX, idhx)}
             idhx <- data.table(idhx, g)
             setnames(idhx, names(idhx)[c(1 : (ncol(idhx)-1))], names(idh))
             idg <- merge(idh, idhx,  by = names(idh), sort = FALSE)
             w_design <- w_final / idg[[ncol(idg)]]
             idg <- data.table(idg, w_design = w_design)
             idh <- idg[, .N, keyby = c(names(idh), "w_design")]
             if (nrow(X) != nrow(idh))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- idh <- NULL
      } else w_design <- w_final


  # Domains
  size <- data.table(size = rep(1, nrow(Y)))
  if (!is.null(Dom)) size1 <- domain(size, Dom) else size1 <- copy(size)

  if (!is.null(Dom)) Y1 <- domain(Y, Dom) else Y1 <- Y

  namesDom <- names(Dom)
  if (!is.null(country)) { DTp <- data.table(country)
                        } else DTp <- data.table(percoun = rep("1", nrow(size)))
  if (withperiod) DTp <- data.table(period, DTp)
  namesperc <- names(DTp)
  namesperc2 <- c("period_country", namesperc)
  period_country <- do.call("paste", c(as.list(DTp), sep = "_")) 

  if (!is.null(Z)) {
       if (!is.null(Dom)) Z1 <- domain(Z, Dom) else Z1 <- Z               
       if (linratio){ 
                   sorts <- unlist(split(Y1[, .I], period_country))
                   lin1 <- lapply(split(Y1[, .I], period_country), 
                                  function(i) data.table(sar_nr = i,
                                                         lin.ratio(Y1[i], Z1[i], w_final[i],
                                                                   Dom = NULL, percentratio = percentratio)))
                   Y2 <- rbindlist(lin1)
                   setkeyv(Y2, "sar_nr")
                   Y2[, sar_nr := NULL]
                   if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
                  } else Y2 <- data.table(copy(Y1), copy(Z1))
    } else Y2 <- copy(Y1)

   namesY2 <- names(Y2)
   namesY2w <- paste0(namesY2, "w")
   namesY <- names(Y)
   namesZ <- names(Z)
   names_H <- names(H)
   namesY1 <- names(Y1)
   namesZ1 <- names(Z1)
   names_id1 <- names(ID_level1)
   names_id2 <- names(ID_level2)
   names_PSU <- names(PSU)
   names_size1 <- names(size1)
   namesYZ <- c(namesY, namesZ)
   namesY1Z1 <- c(namesY1, namesZ1)
   names_country <- names(country)
   names_size1w <- paste0(names_size1, "w")
 
   size1w <- size1 * w_final
   setnames(size1w, names_size1, names_size1w)
 
   DT <- data.table(period_country, DTp, H, PSU, ID_level1, ID_level2,
                    w_final, w_design, size1, size1w, Y2) 

   DTc <- DT[, lapply(.SD, sum, na.rm = TRUE), 
                            by = c(namesperc2, names_H, names_PSU,
                                   names_id1, "w_final", "w_design"),
                          .SDcols = c(names_size1, names_size1w, namesY2)]

   H <- PSU <- id <- DTp <- country <- NULL
   DTagg <- data.table(DT[, namesperc,  with = FALSE], w_final)
   if (!is.null(Dom)) DTagg <- data.table(DTagg, Dom)
   DTagg <- data.table(DTagg, sample_size = 1, 
                       pop_size = w_final, w_final * Y)
   if (!is.null(Z)) DTagg <- data.table(DTagg, w_final * Z)

   DTagg <- DTagg[, lapply(.SD, sum, na.rm = TRUE),
                            keyby = c(namesperc, namesDom),
                          .SDcols = c("sample_size", "pop_size", namesYZ)]
   DTaggs <- DTagg[, c(namesperc, namesDom,
                       "sample_size", "pop_size"), with = FALSE] 

   vars <- data.table(variable = namesY, namesY = namesY)
   if (!is.null(namesZ)) vars <- data.table(variable = as.character(1 : length(namesY)), 
                                            namesY = namesY, namesZ = namesZ) 

   varsYZ <- list(namesY)
   if (!is.null(namesZ)) varsYZ <- list(namesY, namesZ)
   DTagg <- melt(DTagg, id = c(namesperc, namesDom),
                        measure = varsYZ,
                        variable.factor = FALSE)  

   setnames(DTagg, ifelse(!is.null(DTagg$value1), "value1", "value"), "totalY")   
   if (!is.null(Z)) setnames(DTagg, "value2", "totalZ")

   DTagg <- merge(DTagg, vars,  by = "variable")[, variable := NULL]

   DTagg <- merge(DTagg, DTaggs, all.x = TRUE,
                                 by = c(namesperc, namesDom))

   if (!is.null(namesDom)) DTagg[,(paste0(namesDom, "_new")) := lapply(namesDom,
                      function(x) make.names(paste0(x,".", get(x))))]

    varsYZ <- vars <- nameY1 <- nameZ1 <- valueY1 <- valueZ1 <- Dom <- NULL
    Z1 <- Y1 <- period_country <- Y2 <- total <- pop_size <- NULL
    stderr_nw <- nhcor <- num1 <- num <- den1 <- den <- num_den1 <- NULL
    grad1 <- grad2 <- estim <- sd_nw <- stderr_w <- sd_w <- se <- rse <- NULL
    cv <- CI_lower <- absolute_margin_of_error <- CI_upper <- totalZ <- NULL
    relative_margin_of_error <- NULL
  

    # Calibration
    res_outp <- NULL
    if (!is.null(X)) {
         X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
         if (!is.null(countryX)) X0 <- data.table(countryX, X0)
         if (!is.null(periodX)) X0 <- data.table(periodX, X0)
         nos <- c(names(periodX), names(countryX), names(ID_level1))
         DT1 <- merge(DTc, X0, by = nos)

         ind_gr <- DT1[, c(namesperc, names(ind_gr)), with = FALSE]
         ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))
     
         res <- lapply(split(DT1[, .I], ind_period), function(i)                  
                        data.table(DT1[i, names(ID_level1h), with = FALSE],
                                   res <- residual_est(Y = DT1[i, namesY2, with = FALSE],
                                                       X = DT1[i, names(X), with = FALSE],
                                                       weight = DT1[i, "w_design", with = FALSE],
                                                       q = DT1[i, "q", with = FALSE])))
 
         res <- rbindlist(res)
         setnames(res, namesY2, namesY2w)
         DTc <- merge(DTc, res, by = names(ID_level1h)) 
         if (outp_res) res_outp <- DTc[, c(names(ID_level1h), names_PSU, "w_final", namesY2w), with = FALSE]
     } else DTc[, (namesY2w) := .SD[, namesY2, with = FALSE]]

   DTc[, (namesY2w):=.SD[, namesY2w, with = FALSE] * get("w_final")]

   #--------------------------------------------------------*
   # AGGREGATION AT PSU LEVEL ("ULTIMATE CLUSTER" APPROACH) |
   #--------------------------------------------------------*

   DT1 <- DTc[, lapply(.SD, sum, na.rm = TRUE), keyby = c(namesperc2,
                           names_H, names_PSU), .SDcols = namesY2w]
   setnames(DT1, namesY2w, namesY2)

   DTnet <- copy(DT1)
   if (!netchanges)  DTnet <- NULL

   DT2 <- DT1[, lapply(.SD, sum, na.rm = TRUE),
                        keyby = namesperc, .SDcols = namesY2]
   varsYZ <- list(namesY1)
   if (!is.null(namesZ1) & !linratio) varsYZ <- list(namesY1, namesZ1)

   DT2 <- melt(DT2, id = namesperc,
                    measure = varsYZ,
                    variable.factor = FALSE)
   if (!is.null(namesZ1) & !linratio) {setnames(DT2, c("value1", "value2"),
                                                     c("valueY1", "valueZ1"))
                     } else setnames(DT2, ifelse(!is.null(DT2$value1), "value1", "value"), "valueY1")
  
   if (!is.null(namesZ1) & !linratio) {
                    vars <- data.table(variable = 1 : length(namesY1))
                        } else vars <- data.table(variable = namesY1)
 
   if (!is.null(namesDom)) { vars <- data.table(vars, nameY1 = namesY1,
                                               t(data.frame(strsplit(namesY1, "__"))))
                            setnames(vars, names(vars)[3 : length(vars)], 
                                        c("namesY", paste0(namesDom, "_new")))
                     } else {vars <- data.table(vars, nameY1 = namesY1, namesY = namesY1) }
                           
   if (!is.null(namesZ1)) { vars <- data.table(vars, nameZ1 = namesZ1)
                            if (!is.null(namesDom)) {
                                      varsZ <- data.table(nameZ1 = namesZ1, 
                                                          t(data.frame(strsplit(namesZ1, "__"))))
                                      setnames(varsZ, names(varsZ)[2 : length(varsZ)],
                                                    c("namesZ", paste0(namesDom, "_new"))) 
                                      varsZ[, (paste0(namesDom, "_new")) := NULL] 
                                      vars <- merge(vars, varsZ,  by = "nameZ1") 
                               } else vars[, namesZ := nameZ1]  }

  vars <- vars[, lapply(vars, as.character)]

  DT2 <- merge(DT2, vars, by = "variable")
  DT2[, variable := NULL]
  vars <- varsZ <- NULL 

  vars <- c(namesperc, paste0(namesDom, "_new"), "namesY", "namesZ")
  vars <- names(DT2)[names(DT2) %in% vars]

  DTagg <- merge(DTagg, DT2,  by = vars)
  DT2 <- vars <- NULL


  # VECTOR OF THE PARTIAL DERIVATIVES (GRADIENT FUNCTION)

  if (!is.null(namesZ1) & !linratio) {
      DTagg[, grad1 := 1 / valueZ1]
      DTagg[, grad2 := - valueY1 / valueZ1^2]
  }

  # NUMBER OF PSUs PER STRATUM
  setkeyv(DT1, c(namesperc2, names_H))

  stratasf <- nh <- nhcor <- NULL
  DT1[, nh := .N, by = c(namesperc2, names_H)]



 #--------------------------------------------------------------------------*
 # MULTIVARIATE REGRESSION APPROACH USING STRATUM DUMMIES AS REGRESSORS AND |
 # STANDARD ERROR ESTIMATION 						      |
 #--------------------------------------------------------------------------*


  DT1H <- DT1[[names_H]]
  DT1H <- factor(DT1H)
  if (length(levels(DT1H)) == 1) { DT1[, stratasf := 1]
                                 DT1H <- "stratasf"
                       }  else { DT1H <- data.table(model.matrix( ~ DT1H - 1, DT1H,  contrasts = "contr.SAS"))
                                 DT1 <- cbind(DT1, DT1H)
                                 DT1H <- names(DT1H) }

  fits <-lapply(1 : length(namesY1), function(i) {
           fitss <- lapply(split(DT1, DT1$period_country), function(DT1c) {

                        y <- namesY1[i]
                        if ((!is.null(namesZ1))&(!linratio)) z <- paste0(",", toString(namesZ1[i])) else z <- ""

                        funkc <- as.formula(paste("cbind(", trim(toString(y)), z, ")~ 0 + ",
                                       paste(c(0, DT1H), collapse= "+")))

                        res1 <- lm(funkc, data = DT1c)
                        if (use.estVar == TRUE) {res1 <- data.table(crossprod(res1$res))
                                } else res1 <- data.table(res1$res)
                        setnames(res1, names(res1)[1], "num") 
                        res1[, nameY1 := y]
                        if (!is.null(namesZ1) & !linratio) {
                              setnames(res1, names(res1)[2], "den")
                              res1[, nameZ1 := namesZ1[i]]
                            }

                        if (use.estVar == TRUE) {    
                              setnames(res1, "num", "num1") 
                              if (!is.null(namesZ1) & !linratio) {
                                       res1[, num_den1 := res1[["den"]][1]]
                                       res1[, den1 := res1[["den"]][2]] }
                              res1 <- data.table(res1[1], DT1c[1])
                          } else {
                              res1 <- data.table(res1, DT1c)
                              res1[, nhcor := ifelse(nh > 1, nh / (nh - 1), 1)]
                              res1[, num1 := nhcor * num * num]
                              if (!is.null(namesZ1) & !linratio) {
                                   res1[, num_den1 := nhcor * num * den]
                                   res1[, den1 := nhcor * den * den]
                               }}
                         namep <- c("nameY1", "nameZ1")
                         namep <- namep[namep %in% names(res1)]
                         varsp <- c("num1", "den1", "num_den1")
                         varsp <- varsp[varsp %in% names(res1)]

                        fits <- res1[, lapply(.SD, sum), 
                                       keyby = c("period_country",
                                               namesperc, namep),
                                       .SDcols = varsp]
                        return(fits)
                    })
            return(rbindlist(fitss))
        })
  res <- rbindlist(fits)
  DT1 <- fits <- DT1H <- NULL

  vars <- c(namesperc, namesDom, "nameY1", "nameZ1")
  vars <- names(res)[names(res) %in% vars]

  res <- merge(DTagg, res, by = vars)

  DTagg <- total <- NULL
	
  res[, estim := totalY]
  res[, var := num1]
  if (!is.null(res$totalZ)) res[, estim := totalY / totalZ * percentratio]

  if (!is.null(res$totalZ) & !linratio) { 
                    res[, var :=  (grad1 * grad1 * num1) +
                                  (grad2 * grad2 * den1) +
                              2 * (grad1 * grad2 * num_den1)] 
                    res[, var := var * (percentratio)^2] }
    
  main <- c(namesperc, namesDom, "namesY", "nameY1")
  if (!is.null(namesDom)) main <- c(main, paste0(namesDom, "_new"))
  if (!is.null(res$namesZ)) main <- c(main, "namesZ", "nameZ1") 
  main2 <- c(main, "estim", "totalY", "valueY1")
  if (!is.null(res$namesZ)) main <- c(main, "totalZ")
  if (!is.null(res$namesZ)) main2 <- c(main2, "totalZ")
  if (!is.null(namesZ1) & !linratio) main2 <- c(main2, "valueZ1")
  main2 <- c(main2, "num1")
  if (!is.null(namesZ1) & !linratio) main2 <- c(main2, "den1", "grad1", "grad2")

  if (netchanges) { res1 <- res[, main2[!(main2 %in% c("nameY1",
                                     paste0(namesDom, "_new"), "nameZ1"))], with = FALSE]                  
                  } else res1 <- NULL

  main <- c(main, "totalY", "sample_size", "pop_size", "estim", "var")
  res <- res[, main, with = FALSE]

  #-------------------------------------------------------------------------*
  # DESIGN EFFECT (DEFF) ESTIMATION - VARIANCE UNDER SIMPLE RANDOM SAMPLING |
  #-------------------------------------------------------------------------*

  # We aggregate the target variables at household level

  DTs <- DT[, lapply(.SD, sum, na.rm = TRUE), 
                          keyby = c(namesperc2, names_id1, "w_final"),
                         .SDcols = c(names_size1, names_size1w, namesY2)]
  if (ID_level1_max) {    
           DTm <- DT[, lapply(.SD, max, na.rm = TRUE), keyby = c(namesperc2, names_id1), .SDcols = names_size1]
       } else {
          DTm <- DT[, lapply(.SD, sum, na.rm = TRUE), keyby = c(namesperc2, names_id1), .SDcols = names_size1]
     }

  setnames(DTm, names_size1, paste0(names_size1, "m"))
  DTs <- merge(DTs, DTm, by = c(namesperc2, names_id1))

  # Linearised variables

  if (!is.null(namesZ1) & !linratio) {
                   lin1 <- lapply(split(DTs[, .I], DTs$period_country), function(i) 
                                lin.ratio(Y=DTs[i, namesY1,  with = FALSE],
                                          Z=DTs[i, namesZ1,  with = FALSE],
                                          weight=DTs[["w_final"]][i], Dom=NULL,
                                          percentratio=percentratio))
                   Y2a <- rbindlist(lin1)
                   setnames(Y2a, names(Y2a), paste0("lin___", namesY1))
                   DTs <- data.table(DTs, Y2a)
                   Y2a <- paste0("lin___", namesY1)
               } else Y2a <- namesY1

  w_final <- DTs[["w_final"]]
  DTsd <- DTs[, lapply(.SD[, Y2a, with = FALSE], function(x) 
                       sum(w_final*((x-sum(w_final*x)/sum(w_final))^2))/(sum(w_final)-1)),
                       keyby = "period_country"]

  setnames(DTsd, Y2a, paste0("sd_w__", namesY1))
  DTs <- merge(DTs, DTsd, by = "period_country")

  DTm <- DTs[, lapply(.SD[, paste0(names_size1, "m"), with = FALSE], function(x) sum(w_final * x, na.rm = TRUE)),
                       keyby = "period_country"]
  setnames(DTm, paste0(names_size1, "m"), paste0("pop_", names_size1))
  DTs <- merge(DTs, DTm, by = "period_country")

  DTsd <- DTs[, lapply(.SD, sd, na.rm = TRUE), keyby = "period_country", .SDcols = Y2a]
  setnames(DTsd, Y2a, paste0("sd_nw__", namesY1))
  DTs <- merge(DTs, DTsd, by = "period_country")

  DTm <- DTs[, lapply(.SD, sum, na.rm = TRUE), keyby = "period_country", .SDcols = names_size1]
  setnames(DTm, names_size1, paste0("samp_", names_size1))
  DTs <- merge(DTs, DTm, by = "period_country")
    
  DTx <- DTs[, .N, keyby = c(namesperc, paste0("sd_w__", namesY1),
                               paste0("sd_nw__", namesY1),
                               paste0("pop_", names_size1),
                               paste0("samp_", names_size1))]
  DTx[, N := NULL]

  main <- melt(DTx[, c(namesperc, paste0("sd_w__", namesY1)), with = FALSE], id = namesperc)
  main[, nameY1 := substr(variable, 7, nchar(trim(as.character(variable))))] 
  main[, variable := NULL]
  setnames(main, "value", "sd_w")
  res <- merge(res, main, all.x = TRUE, by = c(namesperc, "nameY1"))

  main <- melt(DTx[, c(namesperc, paste0("sd_nw__", namesY1)), with = FALSE], id = namesperc)
  main[, nameY1 := substr(variable, 8, nchar(trim(as.character(variable))))] 
  main[, variable := NULL]
  setnames(main, "value", "sd_nw")
  res <- merge(res, main, all = TRUE, by = c(namesperc, "nameY1"))

  main <- melt(DTx[, c(namesperc, paste0("pop_", names_size1)), with = FALSE], id = namesperc)
  if (!is.null(namesDom)){ 
                   main[, Dom := substr(variable, 11, nchar(trim(as.character(variable))))] 
                   vars <- unique(main[["Dom"]])
                   vars <- data.table(Dom=vars, t(data.frame(strsplit(vars, "__"))))
                   setnames(vars, names(vars)[2 : length(vars)], paste0(namesDom, "_new"))
                   main <- merge(main, vars, all.x = TRUE,  by = "Dom")  }
  main[, variable := NULL]
  setnames(main, "value", "pop")
  nds <- namesperc
  if (!is.null(namesDom)) nds <- c(namesperc, paste0(namesDom, "_new"))
  res <- merge(res, main, all.x = TRUE, by = nds)

  main <- melt(DTx[, c(namesperc, paste0("samp_", names_size1)), with = FALSE], id = namesperc)
  if (!is.null(namesDom)) main[, Dom := substr(variable, 12, nchar(trim(as.character(variable))))] 
  main[, variable := NULL]
  setnames(main, "value", "sampl_siz")
  if (is.null(namesDom)) nds <- namesperc else nds <- c(namesperc, "Dom")
  res <- merge(res, main, all = TRUE, by = nds)

  res[sample_size < pop_size, stderr_nw := 100 * sqrt((1 - (sample_size / pop_size)) / pop_size * sd_nw * sd_nw / sample_size)]
  res[sample_size < pop_size, stderr_w := 100 * sqrt((1 - (sample_size / pop_size)) / pop_size * sd_w * sd_w / sample_size)]

  DT <- DTw <- DTx <- DTs <- DTsd <- sd1 <- nds <- NULL

  res[, se := sqrt(var)]
  res[, rse := se / estim]
  res[, cv := rse * 100]
  tsad <- qnorm(0.5 * (1 + confidence))
  res[, absolute_margin_of_error := tsad * se]
  res[, relative_margin_of_error := tsad * cv]
  res[, CI_lower := estim - tsad * se]
  res[, CI_upper := estim + tsad * se]

  main <- namesperc 
  if (!is.null(namesDom))  main <- c(main, namesDom)
  main <- c(main, "namesY")
  if (!is.null(res$namesZ)) main <- c(main, "namesZ")

  main <- c(main, "sample_size", "pop_size", "estim", "se", 
            "var", "rse", "cv", "absolute_margin_of_error",
            "relative_margin_of_error", "CI_lower", "CI_upper", 
            "sd_w", "sd_nw", "pop", "sampl_siz", "stderr_nw",
            "stderr_w")

  main <- main[main %in% names(res)]
  res <- res[, main,  with = FALSE]
  if (!netchanges & is.null(names_country)) {
      if (!is.null(DTnet)) DTnet[, percoun := NULL]
      res1[, percoun := NULL]
      res[, percoun := NULL]  }
  list(data_net_changes = DTnet, res_out = res_outp, var_grad = res1, results = res)

}   



