vardomh <- function(Y, H, PSU, w_final,
                    ID_level1,
                    ID_level2 = NULL,   
                    Dom = NULL,
                    period = NULL,
                    N_h = NULL,
                    PSU_sort = NULL,
                    fh_zero = FALSE,
                    PSU_level = TRUE,
                    Z = NULL,
                    dataset = NULL,
                    X = NULL,
                    periodX = NULL,
                    X_ID_level1 = NULL,
                    ind_gr = NULL,
                    g = NULL,
                    q = NULL,
                    datasetX = NULL,
                    confidence = .95,
                    percentratio = 1, 
                    outp_lin = FALSE,
                    outp_res = FALSE) {
 
  ### Checking
  if (length(fh_zero) != 1 | !any(is.logical(fh_zero))) stop("'fh_zero' must be logical")
  if (length(PSU_level) != 1 | !any(is.logical(PSU_level))) stop("'PSU_level' must be logical")
  if (length(percentratio) != 1 | !any(is.numeric(percentratio) | percentratio > 0)) stop("'percentratio' must be a positive numeric value")
  if (length(outp_lin) != 1 | !any(is.logical(outp_lin))) stop("'outp_lin' must be logical")
  if (length(outp_res) != 1 | !any(is.logical(outp_res))) stop("'outp_res' must be logical")

  if(length(confidence) != 1 | any(!is.numeric(confidence) | confidence < 0 | confidence > 1)) {
         stop("'confidence' must be a numeric value in [0, 1]")  }

  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset)) == 1) Y <- dataset[, Y, with = FALSE]

      if(!is.null(ID_level1)) {
          if (min(ID_level1 %in% names(dataset)) != 1) stop("'ID_level1' does not exist in 'dataset'!")
          if (min(ID_level1 %in% names(dataset)) == 1) ID_level1 <- dataset[, ID_level1, with = FALSE]  }
      if(!is.null(ID_level2)) {
          if (min(ID_level2 %in% names(dataset)) != 1) stop("'ID_level2' does not exist in 'dataset'!")
          if (min(ID_level2 %in% names(dataset)) == 1) ID_level2 <- dataset[, ID_level2, with = FALSE]  }   
      if(!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE] }
      if(!is.null(PSU)) {
          if (min(PSU %in% names(dataset)) != 1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset)) == 1) PSU <- dataset[, PSU, with = FALSE] }
      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset)) != 1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset)) == 1) w_final <- dataset[, w_final, with = FALSE]}
      if(!is.null(Z)) {
          if (min(Z %in% names(dataset)) != 1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset)) == 1) Z <-dataset[, Z, with = FALSE] }
      if (!is.null(period)) {
           if (min(period %in% names(dataset)) != 1) stop("'period' does not exist in 'dataset'!")
           if (min(period %in% names(dataset)) == 1) period <- dataset[, period, with = FALSE] }
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'Dom' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE]  }
       if (!is.null(PSU_sort)) {
            if (min(PSU_sort %in% names(dataset)) != 1) stop("'PSU_sort' does not exist in 'dataset'!")
            if (min(PSU_sort %in% names(dataset)) == 1) PSU_sort <- dataset[, PSU_sort, with = FALSE] }  
    }

  if (is.null(datasetX)) datasetX <- copy(dataset)
  if (!is.null(datasetX)) {
       datasetX <- data.table(datasetX)
       if (!is.null(periodX)) {
            if (min(periodX %in% names(datasetX)) != 1) stop("'periodX' does not exist in 'datasetX'!")
            if (min(periodX %in% names(datasetX)) == 1) periodX <- datasetX[, periodX, with = FALSE] }

      if (!is.null(X_ID_level1)) {
          if (min(X_ID_level1 %in% names(datasetX)) != 1) stop("'X_ID_level1' does not exist in 'datasetX'!")
          if (min(X_ID_level1 %in% names(datasetX)) == 1) X_ID_level1 <- datasetX[, X_ID_level1, with = FALSE]  }

      if (!is.null(X)) {
          if (min(X %in% names(datasetX)) != 1) stop("'X' does not exist in 'datasetX'!")
          if (min(X %in% names(datasetX)) == 1) X <- datasetX[, X, with = FALSE] }

      if (!is.null(ind_gr)) {
          if (min(ind_gr %in% names(datasetX)) != 1) stop("'ind_gr' does not exist in 'datasetX'!")
          if (min(ind_gr %in% names(datasetX)) == 1) ind_gr <- datasetX[, ind_gr, with = FALSE] }     
              
      if(!is.null(g)) {
          if (min(g %in% names(datasetX)) != 1) stop("'g' does not exist in 'datasetX'!")
          if (min(g %in% names(datasetX)) == 1) g <- datasetX[, g, with = FALSE] }

      if(!is.null(q)) {
          if (min(q %in% names(datasetX)) != 1) stop("'q' does not exist in 'datasetX'!")
          if (min(q %in% names(datasetX)) == 1) q <- datasetX[, q, with = FALSE] } 
     }

  equal_dataset <- identical(dataset, datasetX) & !is.null(X)
  if (equal_dataset) X_ID_level1 <- ID_level1
  N <- dataset <- datasetX <- NULL

  # Y
  Y <- data.table(Y, check.names = TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric values")
  if (any(is.na(Y))) stop("'Y' has missing values")
  if (is.null(names(Y))) stop("'Y' must have column names")
  if (any(grepl("__", names(Y)))) stop("'Y' is not allowed column names with '__'")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(H))) stop("'H' must have column names")
  H[, (names(H)) := lapply(.SD, as.character)]
  if (any(is.na(H))) stop("'H' has missing values")
  
  # PSU
  PSU <- data.table(PSU)
  aPSU <- names(PSU)
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  PSU[, (names(PSU)) := lapply(.SD, as.character)]
  if (any(is.na(PSU))) stop("'PSU' has missing values")
  
  # w_final 
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be a vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[, 1]
  if (!is.numeric(w_final)) stop("'w_final' must be numeric")
  if (any(is.na(w_final))) stop("'w_final' has missing values") 

  # period     
  if (!is.null(period)) {
      period <- data.table(period)
      if (any(duplicated(names(period)))) 
                stop("'period' are duplicate column names: ", 
                     paste(names(period)[duplicated(names(period))], collapse = ","))
      if (nrow(period) != n) stop("'period' must be the same length as 'Y'")
      period[, (names(period)) := lapply(.SD, as.character)]
      if(any(is.na(period))) stop("'period' has missing values")
  } 
  np <- sum(ncol(period))

  # ID_level2
  if (is.null(ID_level2)) ID_level2 <- 1 : n
  ID_level2 <- data.table(ID_level2)
  ID_level2[, (names(ID_level2)) := lapply(.SD, as.character)]
  if (any(is.na(ID_level2))) stop("'ID_level2' has missing values")
  if (ncol(ID_level2) != 1) stop("'ID_level2' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_level2) != n) stop("'ID_level2' length must be equal with 'Y' row count")
  if (is.null(period)){ if (any(duplicated(ID_level2))) stop("'ID_level2' are duplicate values") 
                       } else {dd <- data.table(period, ID_level2)
                               if (any(duplicated(dd, by = names(dd)))) stop("'ID_level2' by period are duplicate values")
                               dd <- NULL}

  # ID_level1
  if (is.null(ID_level1)) stop("'ID_level1' must be defined")
  ID_level1 <- data.table(ID_level1)
  if (names(ID_level1) == names(PSU)) setnames(PSU, names(PSU), paste0(names(PSU), "_PSU"))
  ID_level1[, (names(ID_level1)) := lapply(.SD, as.character)]
  if (any(is.na(ID_level1))) stop("'ID_level1' has missing values")
  if (ncol(ID_level1) != 1) stop("'ID_level1' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_level1) != n) stop("'ID_level1' must be the same length as 'Y'")
  if (names(ID_level2) == names(ID_level1)) setnames(ID_level2, names(ID_level2), paste0(names(ID_level2), "_id"))


  # PSU_sort
  if (!is.null(PSU_sort)) {
          PSU_sort <- data.frame(PSU_sort)
          if (nrow(PSU_sort) != n) stop("'PSU_sort' must be equal with 'Y' row count")
          if (ncol(PSU_sort) != 1) stop("'PSU_sort' must be a vector or 1 column data.frame, matrix, data.table")
          PSU_sort <- PSU_sort[, 1]
          if (!is.numeric(PSU_sort)) stop("'PSU_sort' must be numeric")
          if (any(is.na(PSU_sort))) stop("'PSU_sort' has missing values")

          psuag <- data.table(PSU, PSU_sort)
          if (!is.null(period)) psuag <- data.table(period, psuag)
          psuag <- psuag[,.N, by = names(psuag)][,N := NULL]
          psuag <- psuag[,.N, by = c(names(period), names(PSU))]
          if (nrow(psuag[N > 1]) > 0) stop("'PSU_sort' must be equal for each 'PSU'")
  }
  psusn <- as.integer(!is.null(PSU_sort))

  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2, " columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numeric")
      if (any(is.na(N_h))) stop("'N_h' has missing values") 
      if (is.null(names(N_h))) stop("'N_h' must have column names")
      nams <- c(names(period), names(H))
      if (all(nams %in% names(N_h))) {N_h[, (nams) := lapply(.SD, as.character), .SDcols = nams]
             } else stop(paste0("All strata titles of 'H'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h'"))
      if (is.null(period)) {
             if (any(is.na(merge(unique(H), N_h, by = names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
             if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                pH <- NULL
              }

      setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  }

  # Dom
  namesDom <- NULL  
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
          stop("'Dom' are duplicate column names: ", 
               paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (is.null(names(Dom))) stop("'Dom' must have column names")
    Dom[, (names(Dom)) := lapply(.SD, as.character)]
    if (any(is.na(Dom))) stop("'Dom' has missing values")
    if (any(grepl("__", names(Dom)))) stop("'Dom' is not allowed column names with '__'")
    namesDom <- names(Dom)
  }
  
  # Z
  if (!is.null(Z)) {
    Z <- data.table(Z, check.names = TRUE)
    if (nrow(Z) != n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' must be equal column count")
    if (!all(sapply(Z, is.numeric))) stop("'Z' must be numeric values")
    if (any(is.na(Z))) stop("'Z' has missing values")
    if (is.null(names(Z))) stop("'Z' must have column names")
    if (any(grepl("__", names(Z)))) stop("'Z' is not allowed column names with '__'")
  }
 
  if (!is.null(X)) {
    X <- data.table(X, check.names = TRUE)
    if (!all(sapply(X, is.numeric))) stop("'X' must be numeric values")
  }

  # periodX
  if (!is.null(X)) {
     if(!is.null(periodX)) {
        periodX <- data.table(periodX)
        periodX[, (names(periodX)) := lapply(.SD, as.character)]
        if (any(is.na(periodX))) stop("'periodX' has missing values")
        if (any(duplicated(names(periodX)))) 
                    stop("'periodX' are duplicate column names: ", 
                         paste(names(periodX)[duplicated(names(periodX))], collapse = ","))
        if (nrow(periodX) != nrow(X)) stop("'periodX' length must be equal with 'X' row count")
        if (ncol(periodX) != ncol(period)) stop("'periodX' length must be equal with 'period' column count")
        if (names(periodX) != names(period)) stop("'periodX' must be equal with 'period' names")
        periX <- periodX[, .N, keyby = names(periodX)][, N := NULL]
        peri <- period[, .N, keyby = names(period)][, N := NULL]
        if (any(peri != periX)) stop("'unique(period)' and 'unique(periodX)' records have different")
      } else if (!is.null(period)) stop("'periodX' must be defined")
   } 

 # X_ID_level1
  if (!is.null(X)) {
    if (is.null(X_ID_level1)) stop("'X_ID_level1' must be defined")
    X_ID_level1 <- data.table(X_ID_level1)
    X_ID_level1[, (names(X_ID_level1)) := lapply(.SD, as.character)]
    if (any(is.na(X_ID_level1))) stop("'X_ID_level1' has missing values")
    if (nrow(X) != nrow(X_ID_level1)) stop("'X' and 'X_ID_level1' have different row count")
    if (ncol(X_ID_level1) != 1) stop("'X_ID_level1' must be 1 column data.frame, matrix, data.table")

    ID_level1h <- data.table(unique(ID_level1))
    X_ID_level1h <- copy(X_ID_level1)
    if (!is.null(periodX)) {X_ID_level1h <- data.table(periodX, X_ID_level1h)
                           ID_level1h <- data.table(period, ID_level1)
                           ID_level1h <- ID_level1h[, .N, by = names(ID_level1h)][, N := NULL]}
 
    if (nrow(X_ID_level1h[,.N, by = names(X_ID_level1h)][N > 1]) > 0) stop("'X_ID_level1' have duplicates")
    nperIDh <- names(ID_level1h)
    if (any(nperIDh != names(X_ID_level1h))) stop("'X_ID_level1' and 'ID_level1' must be equal names")
    if (ID_level1h[, class(get(nperIDh))] != X_ID_level1h[, class(get(nperIDh))])  stop("Class for 'X_ID_level1' and class for 'ID_level1' must be equal ")

    setkeyv(X_ID_level1h, names(X_ID_level1h))
    setkeyv(ID_level1h, names(ID_level1h))
    if (!is.null(period)) {
        if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'periodX' with 'X_ID_level1' and 'unique(period, ID_level1)' have different row count")
        if (any(ID_level1h != X_ID_level1h)) stop("'periodX' with 'X_ID_level1' and 'unique(period, ID_level1)' records have different")
      } else {
        if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'X_ID_level1' and 'unique(ID_level1)' have different row count")
        if (any(ID_level1h != X_ID_level1h)) stop("'X_ID_level1' and 'unique(ID_level1)' records have different")
     }
    X_ID_level1h <- NULL
  }

  # ind_gr
  if (!is.null(X)) {
     if(is.null(ind_gr)) ind_gr <- rep("1", nrow(X)) 
     ind_gr <- data.table(ind_gr)
     if (nrow(ind_gr) != nrow(X)) stop("'ind_gr' length must be equal with 'X' row count")
     if (ncol(ind_gr) != 1) stop("'ind_gr' must be 1 column data.frame, matrix, data.table")
     ind_gr[, (names(ind_gr)) := lapply(.SD, as.character)]
     if (any(is.na(ind_gr))) stop("'ind_gr' has missing values")
   }

  # X
  if (!is.null(X)) {
       X1 <- data.table(X, check.names = TRUE)
       nX1 <- names(X1)
       ind_gr1 <- copy(ind_gr) 
       if (!is.null(periodX)) ind_gr1 <- data.table(periodX, ind_gr1, check.names = TRUE)
       X2 <- data.table(ind_gr1, X1)
       X1 <- X2[, .N, keyby = names(ind_gr1)][[ncol(ind_gr1) + 1]]
       X2 <- X2[, lapply(.SD, function(x) sum(!is.na(x))), keyby = names(ind_gr1), .SDcols = nX1]
       X2 <- X2[, !(names(X2) %in% names(ind_gr1)), with = FALSE]

       if (!all(X2 == 0 | X1 == X2)) stop("X has missing values")
       ind_gr1 <- nX1 <- nX2 <- X1 <- X2 <- NULL
    }

  # g
  if (!is.null(X)) {
    if (is.null(class(g)) | all(class(g) == "function")) stop("'g' must be numeric")
    g <- data.frame(g)
    if (nrow(g) != nrow(X)) stop("'g' length must be equal with 'X' row count")
    if (ncol(g) != 1) stop("'g' must be 1 column data.frame, matrix, data.table")
    g <- g[, 1]
    if (!is.numeric(g)) stop("'g' must be numeric")
    if (any(is.na(g))) stop("'g' has missing values")
    if (any(g == 0)) stop("'g' value can not be 0")
   }
    
  # q
  if (!is.null(X)) {
    if (is.null(q))  q <- rep(1, nrow(X))
    if (is.null(class(q)) | all(class(q) == "function")) stop("'q' must be numeric")
    q <- data.frame(q)
    if (nrow(q) != nrow(X)) stop("'q' length must be equal with 'X' row count")
    if (ncol(q) != 1) stop("'q' must be 1 column data.frame, matrix, data.table")
    q <- q[, 1]
    if (!is.numeric(q)) stop("'q' must be numeric")
    if (any(is.na(q))) stop("'q' has missing values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }
  
  ### Calculation 
  # Domains

  if (!is.null(Dom)) Y1 <- domain(Y, Dom) else Y1 <- Y
  Y <- NULL
  n_nonzero <- copy(Y1)
  if (!is.null(period)){ n_nonzero <- data.table(period, n_nonzero) 
                         n_nonzero <- n_nonzero[, lapply(.SD, function(x) 
                                                         sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         keyby = names(period),
                                                         .SDcols = names(Y1)]
                  } else n_nonzero <- n_nonzero[, lapply(.SD, function(x) 
                                                         sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         .SDcols = names(Y1)]

  respondent_count <- sample_size <- pop_size <- NULL
  nhs <- data.table(respondent_count = 1, pop_size = w_final)
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                       keyby = eval(names(nhs)[0 : 1 - ncol(nhs)]),
                                                      .SDcols = c("respondent_count", "pop_size")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                     .SDcols = c("respondent_count", "pop_size")]

  # Design weights
  if (!is.null(X)) {
             ID_level1h <- data.table(ID_level1)
             if (!is.null(period)) { ID_level1h <- data.table(period, ID_level1h)
                                     X_ID_level1 <- data.table(period, X_ID_level1)
                                    }
             idhx <- data.table(X_ID_level1, g)
             setnames(idhx, names(idhx)[c(1 : (ncol(idhx) - 1))], names(ID_level1h))
             idg <- merge(ID_level1h, idhx, by = names(ID_level1h), sort = FALSE)
             w_design <- w_final / idg[[ncol(idg)]]
             idg <- data.table(idg, w_design = w_design)
             idh <- idg[, .N, keyby = c(names(ID_level1h), "w_design")]
             if (nrow(X) != nrow(idh))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- ID_level1h <- NULL
      } else w_design <- w_final
      
  # Ratio of two totals
  sar_nr <- Z1 <- persort <- linratio_outp <- estim <- NULL 
  var_est2 <- se <- rse <- cv <- absolute_margin_of_error <- NULL
  relative_margin_of_error <- CI_lower <- S2_y_HT <- NULL
  S2_y_ca <- S2_res <- CI_upper <- variable <- variableZ <- NULL
  .SD <- deff_sam <- deff_est <- deff <- n_eff <- NULL
  
  aH <- names(H)
  idper <- ID_level2
  period0 <- copy(period)
  if (!is.null(period)) idper <- data.table(idper, period)

  if (!is.null(Z)) {
     if (!is.null(Dom)) Z1 <- domain(Z, Dom) else Z1 <- Z
     if (is.null(period)) {
          Y2 <- lin.ratio(Y1, Z1, w_final, Dom = NULL, percentratio = percentratio)
        } else {
          periodap <- do.call("paste", c(as.list(period), sep="_"))
          lin1 <- lapply(split(Y1[, .I], periodap), function(i)
                         data.table(sar_nr = i, 
                              lin.ratio(Y1[i], Z1[i], w_final[i],
                                        Dom = NULL, percentratio = percentratio)))
          Y2 <- rbindlist(lin1)
          setkeyv(Y2, "sar_nr")
          Y2[, sar_nr := NULL]
        }
     if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
     if (outp_lin) linratio_outp <- data.table(idper, PSU, Y2) 
    } else {
            Y2 <- Y1
          }


  # Total estimation
  lin1 <- Y_est <- Z_est <- .SD <- variableDZ <- NULL

  hY <- data.table(Y1 * w_final)
  if (is.null(period)) { Y_est <- hY[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Y1)]
                } else { hY <- data.table(period0, hY)
                         Y_est <- hY[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Y1)]
                       }
  Y_est <- transpos(Y_est, is.null(period), "Y_est", names(period))
  all_result <- Y_est
  
  if (!is.null(Z1)) {
         YZnames <- data.table(variable = names(Y1), variableDZ = names(Z1))
         setkeyv(YZnames, "variable")
         setkeyv(all_result, "variable")
         all_result <- merge(all_result, YZnames)
         
         hZ <- data.table(Z1 * w_final)
         if (is.null(period)) { Z_est <- hZ[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Z1)]
                       } else { hZ <- data.table(period, hZ)
                                Z_est <- hZ[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Z1)]
                              }
         Z_est <- transpos(Z_est, is.null(period), "Z_est", names(period), "variableDZ")
         all_result <- merge(all_result, Z_est, all = TRUE, by = c(names(period), "variableDZ"))                                            
      }

  vars <- data.table(variable = names(Y1), nr_names = 1 : ncol(Y1))
  all_result <- merge(vars, all_result, by = "variable")

  n_nonzero <- transpos(n_nonzero, is.null(period), "n_nonzero", names(period))
  all_result <- merge(all_result, n_nonzero, all = TRUE, by = c(names(period), "variable"))
  n_nonzero <- vars <- Y1 <- Z1 <- Y_est <- Z_est <- hY <- hZ <- YZnames <- NULL


  # Calibration

  YY <- data.table(idper, ID_level1, H, PSU, check.names = TRUE)
  if (!is.null(PSU_sort)) YY <- data.table(YY, PSU_sort, check.names = TRUE)
  YY <- data.table(YY, w_design, w_final, Y2, check.names = TRUE)

  YY2 <- YY[, lapply(.SD, sum, na.rm = TRUE), by = c(names(YY)[c(2 : (6 + np + psusn))]),
                                             .SDcols = names(YY)[-(1 : (6 + np + psusn))]]
  Y3 <- YY2[, c(-(1 : (5 + np + psusn))), with = FALSE]

  idper <- period <- NULL
  if (np > 0) period <- YY2[, c(1 : np), with = FALSE]

  ID_level1h <- YY2[, np + 1, with = FALSE]
  H <- YY2[, np + 2, with = FALSE]
  setnames(H, names(H), aH)

  PSU <- YY2[, np + 3, with = FALSE]
  setnames(PSU, names(PSU), aPSU)

  if (!is.null(PSU_sort)) PSU_sort <- YY2[[np + 4]]

  w_design2 <- YY2[[np + 4 + psusn]]    
  w_final2 <- YY2[[np + 5 + psusn]]
  YY <- YY2 <- NULL

  # Calibration
  res_outp <- NULL
  if (!is.null(X)) {
       if (np > 0) ID_level1h <- data.table(period, ID_level1h)
       setnames(ID_level1h, names(ID_level1h), names(X_ID_level1))
       X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
       D1 <- merge(ID_level1h, X0, by = names(ID_level1h), sort = FALSE)

       ind_gr <- D1[, np + 2, with = FALSE]
       if (!is.null(period)) ind_gr <- data.table(D1[, names(periodX), with = FALSE], ind_gr)
       ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))
    
       lin1 <- lapply(split(Y3[, .I], ind_period), function(i) 
                 data.table(sar_nr = i, 
                            residual_est(Y = Y3[i],
                                         X = D1[i, (np + 5) : ncol(D1), with = FALSE],
                                         weight = w_design2[i],
                                         q = D1[i, np + 3, with = FALSE])))
       Y4 <- rbindlist(lin1)
       setkeyv(Y4, "sar_nr")
       Y4[, sar_nr := NULL]
       if (outp_res) res_outp <- data.table(ID_level1h, PSU, w_final2, Y4)
   } else Y4 <- Y3
  X0 <- D1 <- X_ID_level1 <- ID_level1h <- ind_gr <- lin1 <- X <- g <- q <- NULL                             

  var_est <- variance_est(Y = Y4, H = H, PSU = PSU,
                          w_final = w_final2,
                          N_h = N_h, fh_zero = fh_zero, 
                          PSU_level = PSU_level,
                          PSU_sort = PSU_sort,
                          period = period, dataset = NULL,
                          msg = "Current variance estimation")
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- merge(all_result, var_est, all = TRUE, by = c(names(period), "variable"))
 
  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y3, H = H, PSU = PSU,
                             w_final = w_design2,
                             N_h = N_h, fh_zero = fh_zero, 
                             PSU_level = PSU_level,
                             PSU_sort = PSU_sort,
                             period = period, dataset = NULL,
                             msg = "Variance of HT estimator under current design")
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT, all = TRUE, by = c(names(period), "variable"))
  n_nonzero <- var_est <- var_cur_HT <- NULL
  H <- PSU <- PSU_sort <- N_h <- NULL

  # Variance of HT estimator under SRS
  if (is.null(period)) {
           varsrs <- var_srs(Y = Y3, w = w_design2)
           S2_y_HT <- varsrs$S2p
           S2_y_ca <- var_srs(Y = Y3, w = w_final2)$S2p
           var_srs_HT <- varsrs$varsrs
       } else {
           period_agg <- unique(period)
           lin1 <- lapply(1 : nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y3)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          varsrs <- var_srs(Y = Y3[ind], w = w_design2[ind])
                          varsca <- var_srs(Y = Y3[ind], w = w_final2[ind])
                          list(S2p = data.table(period_agg[i,], varsrs$S2p),
                               varsrs = data.table(period_agg[i,], varsrs$varsrs),
                               S2ca = data.table(period_agg[i,], varsca$S2p))
                        })
           S2_y_HT <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_HT <- rbindlist(lapply(lin1, function(x) x[[2]]))
           S2_y_ca <- rbindlist(lapply(lin1, function(x) x[[3]]))
      }
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_HT <- transpos(S2_y_HT, is.null(period), "S2_y_HT", names(period))
  all_result <- merge(all_result, S2_y_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_ca <- transpos(S2_y_ca, is.null(period), "S2_y_ca", names(period))
  all_result <- merge(all_result, S2_y_ca, all = TRUE, by = c(names(period), "variable"))
  Y3 <- w_design2 <- var_srs_HT <- S2_y_HT <- S2_y_ca <- NULL

  # Variance of calibrated estimator under SRS
  if (is.null(period)) {
           varsres <- var_srs(Y4, w = w_final2)
           S2_res <- varsres$S2p
           var_srs_ca <- varsres$varsrs
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1 : nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y4)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          varsres <- var_srs(Y = Y4[ind], w = w_final2[ind])
                          list(S2p = data.table(period_agg[i,], varsres$S2p),
                               varsrs = data.table(period_agg[i,], varsres$varsrs))
                        })
           S2_res <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_ca <- rbindlist(lapply(lin1, function(x) x[[2]]))
        }
  var_srs_ca <- transpos(var_srs_ca, is.null(period), "var_srs_ca", names(period), "variable")
  all_result <- merge(all_result, var_srs_ca, all = TRUE, by = c(names(period), "variable"))
  S2_res <- transpos(S2_res, is.null(period), "S2_res", names(period), "variable")
  all_result <- merge(all_result, S2_res, all = TRUE, by = c(names(period), "variable"))
  Y4 <- w_final2 <- var_srs_ca <- S2_res <- NULL


  all_result[, estim := Y_est]   
  if (!is.null(all_result$Z_est)) all_result[, estim := Y_est / Z_est * percentratio]

  if (nrow(all_result[var_est < 0]) > 0) stop("Estimation of variance are negative!")
 
  # Design effect of sample design
  all_result[, deff_sam := var_cur_HT / var_srs_HT]
  
  # Design effect of estimator
  all_result[, deff_est := var_est / var_cur_HT]
  
  # Overall effect of sample design and estimator
  all_result[, deff := deff_sam * deff_est]

  all_result[, var_est2 := var_est]
  all_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  all_result[, se := sqrt(var_est2)]
  all_result[(estim != 0) & !is.nan(estim), rse := se / estim]
  all_result[estim == 0 | is.nan(estim), rse := NA]
  all_result[, cv := rse * 100]


  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, absolute_margin_of_error := tsad * se]
  all_result[, relative_margin_of_error := tsad * cv]
  all_result[, CI_lower := estim - tsad * se]
  all_result[, CI_upper := estim + tsad * se]

  variableD <- NULL
  setnames(all_result, c("variable", "var_est"), c("variableD", "var"))
  if (!is.null(all_result$Z_est)) {
                       nosrZ <- data.table(all_result[, "variableDZ"], all_result[, tstrsplit(variableDZ, "__")][, 1])
                       nosrZ <- nosrZ[!duplicated(nosrZ)]
                       setnames(nosrZ, "V1", "variableZ")
                       all_result <- merge(all_result, nosrZ, by = "variableDZ")
                       nosrZ <- NULL
                    }

  nosr <- data.table(all_result[, "variableD"], all_result[, tstrsplit(variableD, "__")])
  nosr <- nosr[!duplicated(nosr)]
  nosr <- nosr[, lapply(nosr, as.character)]
  setnames(nosr, names(nosr)[2], "variable")

  namesDom1 <- namesDom
  if (!is.null(Dom)) {
       setnames(nosr, names(nosr)[3 : ncol(nosr)], paste0(namesDom, "_new"))
       nhs[, (paste0(namesDom, "_new")) := lapply(namesDom, function(x) make.names(paste0(x,".", get(x))))]
       namesDom1 <- paste0(namesDom, "_new")
    }

  all_result <- merge(nosr, all_result, by="variableD")
  namesDom <- nosr <- NULL
  
  if (!is.null(all_result$Z_est)) { 
       all_result[, variable := paste("R", get("variable"), get("variableZ"), sep="__")] }

  if (!is.null(c(Dom, period))) { all_result <- merge(all_result, nhs,
                                                      all = TRUE, by = c(namesDom1, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]} 

  variab <- c("respondent_count", "n_nonzero", "pop_size")
  if (!is.null(all_result$Z_est)) variab <- c(variab, "Y_est", "Z_est")
  variab <- c(variab, "estim", "var", "se", "rse", "cv", 
              "absolute_margin_of_error", "relative_margin_of_error",
              "CI_lower", "CI_upper")
  if (is.null(Dom))  variab <- c(variab, "S2_y_HT", "S2_y_ca", "S2_res") 
  variab <- c(variab, "var_srs_HT",  "var_cur_HT", "var_srs_ca",
              "deff_sam", "deff_est", "deff")

  setkeyv(all_result, c("nr_names", names(Dom), names(period)))
  all_result <- all_result[, c("variable", names(Dom), names(period), variab), with = FALSE]
  
  list(lin_out = linratio_outp,
       res_out = res_outp,
       all_result = all_result)
}

