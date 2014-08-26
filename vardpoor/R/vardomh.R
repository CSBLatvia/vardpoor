
vardomh <- function(Y, H, PSU, w_final,
                   ID_household,
                   id = NULL,   
                   Dom = NULL,
                   period = NULL,
                   N_h = NULL,
                   fh_zero=FALSE,
                   PSU_level=TRUE,
                   Z = NULL,
                   dataset = NULL,
                   X = NULL,
                   periodX = NULL,
                   X_ID_household = NULL,
                   ind_gr = NULL,
                   g = NULL,
                   datasetX = NULL,
                   q = rep(1, if (is.null(datasetX)) 
                           nrow(data.frame(X)) else nrow(datasetX)),
                   confidence = .95, 
                   outp_lin = FALSE,
                   outp_res = FALSE) {
 
  ### Checking
  if (!is.logical(fh_zero)) stop("'fh_zero' must be the logical value")
  if (!is.logical(PSU_level)) stop("'PSU_level' must be the logical value")

  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {
      dataset <- data.frame(dataset)
      aY <- Y
      if (min(Y %in% names(dataset))!=1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset))==1) {
                                Y <- data.frame(dataset[, Y], check.names=FALSE)
                                names(Y) <- aY }
      if(!is.null(id)) {
          id2 <- id
          if (min(id %in% names(dataset))!=1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset))==1) id <- data.frame(dataset[, id])
          names(id) <- id2  }
      if(!is.null(ID_household)) {
          ID_household2 <- ID_household
          if (min(ID_household %in% names(dataset))!=1) stop("'ID_household' does not exist in 'dataset'!")
          if (min(ID_household %in% names(dataset))==1) ID_household <- data.frame(dataset[, ID_household])
          names(ID_household) <- ID_household2  }
      if(!is.null(H)) {
          aH <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- as.data.frame(dataset[, aH], stringsAsFactors=FALSE)
                                 names(H) <- aH }}
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
      if(!is.null(Z)) {
          aZ <- Z
          if (min(Z %in% names(dataset))!=1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset))==1) {
                                Z <-data.frame(dataset[, aZ], check.names=FALSE)
                                names(Z) <- aZ }}
      if (!is.null(period)) {
           aperiod <- period  
           if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
           if (min(period %in% names(dataset))==1) {
                               period <- as.data.frame(dataset[, aperiod], stringsAsFactors=FALSE)
                               names(period) <- aperiod }}
      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom2], stringsAsFactors=FALSE) 
                  names(Dom) <- Dom2 }    }
    }

  if(!is.null(datasetX)) {
       if (!is.null(periodX)) {
            aperiodX <- periodX  
            if (min(periodX %in% names(datasetX))!=1) stop("'periodX' does not exist in 'datasetX'!")
            if (min(periodX %in% names(datasetX))==1) {
                                periodX <- as.data.frame(dataset[, periodX], stringsAsFactors=FALSE)
                                names(periodX) <- aperiodX }}

      if(!is.null(X_ID_household)) {
          X_ID_household2 <- X_ID_household
          if (min(X_ID_household %in% names(datasetX))!=1) stop("'X_ID_household' does not exist in 'datasetX'!")
          if (min(X_ID_household %in% names(datasetX))==1) X_ID_household <- data.frame(datasetX[, X_ID_household])
          names(X_ID_household) <- X_ID_household2  }

      if(!is.null(X)) {
          if (min(X %in% names(datasetX))!=1) stop("'X' does not exist in 'datasetX'!")
          if (min(X %in% names(datasetX))==1) X <- datasetX[, X] }

      if(!is.null(ind_gr)) {
          if (min(ind_gr %in% names(datasetX))!=1) stop("'ind_gr' does not exist in 'datasetX'!")
          if (min(ind_gr %in% names(datasetX))==1) ind_gr <- datasetX[, ind_gr] }     
              
      if(!is.null(g)) {
          if (min(g %in% names(datasetX))!=1) stop("'g' does not exist in 'datasetX'!")
          if (min(g %in% names(datasetX))==1) g <- datasetX[, g] }

      if(!is.null(q)) {
          if (min(q %in% names(datasetX))!=1) {
              if (length(q)!=nrow(datasetX))  stop("'q' does not exist in 'datasetX'!") }
          if (min(q %in% names(datasetX))==1) q <- datasetX[, q] } 
     }

  # Y
  Y <- data.table(Y, check.names=TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric values")
  if (any(is.na(Y))) stop("'Y' has unknown values")
  if (is.null(names(Y))) stop("'Y' must be colnames")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")
  
  # PSU
  PSU <- data.table(PSU)
  aPSU <- names(PSU)
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

  # ID_household
  if (is.null(ID_household)) stop("'ID_household' must be defined")
  ID_household <- data.table(ID_household)
  if (ncol(ID_household) != 1) stop("'ID_household' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_household) != n) stop("'ID_household' must be the same length as 'Y'")
  if (is.null(names(ID_household))) setnames(ID_household,names(ID_household),"ID_household")

  # id
  if (is.null(id)) id <- 1:n
  id <- data.table(id)
  if (any(is.na(id))) stop("'id' has unknown values")
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (nrow(id) != n) stop("'id' length must be equal with 'Y' row count")
  if (is.null(names(id))||(names(id)=="id")) setnames(id,names(id),"ID")
  if (names(id)==names(ID_household)) setnames(id,names(id),paste(names(id),"_id",sep=""))
  if (is.null(period)){ if (any(duplicated(id))) stop("'id' are duplicate values") 
                       } else {
                          id1 <- data.table(period, id)
                          if (any(duplicated(id1))) stop("'id' by period are duplicate values")
                         }

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
      if (is.null(period)) {
             if (names(H) != names(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
             if (any(is.na(merge(unique(H), N_h, by=names(H), all.x = T)))) stop("'N_h' is not defined for all stratas")
       } else { pH <- data.frame(period, H)
                if (any(names(pH) != names(N_h)[c(1:(1+np))])) stop("Strata titles for 'period' with 'H' and 'N_h' is not equal")
                if (any(is.na(merge(unique(pH), N_h, by=names(pH), all.x = T)))) stop("'N_h' is not defined for all stratas and periods")
                if (any(duplicated(N_h[, head(names(N_h),-1), with=F]))) stop("Strata values for 'N_h' must be unique in all periods")
                pH <- NULL 
     }
    setkeyv(N_h, names(N_h)[c(1:(1+np))])
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
  }
  
  # Z
  if (!is.null(Z)) {
    Z <- data.table(Z, check.names = T)
    if (nrow(Z) != n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' must be equal column count")
    if (!all(sapply(Z, is.numeric))) stop("'Z' must be numeric values")
    if (any(is.na(Z))) stop("'Z' has unknown values")
    if (is.null(names(Z))) stop("'Z' must be colnames")
  }
 
  # periodX
  if (!is.null(X)) {
     if(!is.null(periodX)) {
        periodX <- data.table(periodX)
        periX <- data.table(unique(periodX))
        setkeyv(periX, names(periX))
        peri <- data.table(unique(period))
        setkeyv(peri, names(peri))
        if (any(duplicated(names(periodX)))) 
                    stop("'periodX' are duplicate column names: ", 
                         paste(names(periodX)[duplicated(names(periodX))], collapse = ","))
        if (nrow(periodX) != nrow(X)) stop("'periodX' length must be equal with 'X' row count")
        if (ncol(periodX) != ncol(period)) stop("'periodX' length must be equal with 'period' column count")
        if (names(periodX) != names(period)) stop("'periodX' must be equal with 'period' names")
        if (any(is.na(periodX))) stop("'periodX' has unknown values")
        if (any(peri != periX)) stop("'unique(period)' and 'unique(periodX)' records have different")
      }
   }

 # X_ID_household
  if (!is.null(X)) {
    X_ID_household <- data.table(X_ID_household)
    if (ncol(X_ID_household) != 1) stop("'X_ID_household' must be 1 column data.frame, matrix, data.table")
    if (any(is.na(X_ID_household))) stop("'X_ID_household' has unknown values")

    IDh <- data.table(unique(ID_household))
    if (!is.null(periodX)) {X_ID_household <- data.table(periodX, X_ID_household)
                           IDh <- data.table(unique(data.table(period, ID_household)))}

    if (any(duplicated(X_ID_household))) stop("'X_ID_household' have duplicates")
    setkeyv(X_ID_household, names(X_ID_household))
    setkeyv(IDh, names(IDh))
    if (!is.null(period)) {
        if (nrow(IDh) != nrow(X_ID_household)) stop("'period' with 'X_ID_household' and 'unique(period, ID_household)' have different row count")
        if (any(IDh != X_ID_household)) stop("'period' with 'X_ID_household' and 'unique(period, ID_household)' records have different")
      } else {
        if (nrow(IDh) != nrow(X_ID_household)) stop("'X_ID_household' and 'unique(ID_household)' have different row count")
        if (any(IDh != X_ID_household)) stop("'X_ID_household' and 'unique(ID_household)' records have different")
    }}


  # X
  if (!is.null(X)) {
    X <- data.table(X, check.names=T)
    if (nrow(X) != nrow(X_ID_household)) stop("'X' and 'X_ID_household' have different row count")
  }



  # ind_gr
  if (!is.null(X)) {
     if(is.null(ind_gr)) ind_gr <- rep.int(1, nrow(X)) 
     ind_gr <- data.table(ind_gr)
     if (nrow(ind_gr) != nrow(X)) stop("'ind_gr' length must be equal with 'X' row count")
     if (ncol(ind_gr) != 1) stop("'ind_gr' must be 1 column data.frame, matrix, data.table")
     if (any(is.na(ind_gr))) stop("'ind_gr' has unknown values")
   }

  # X
  if (!is.null(X)) {
       X1 <- data.table(X, check.names=T)
       nX1 <- names(X1)
       ind_gr1 <- copy(ind_gr) 
       if (!is.null(periodX)) ind_gr1 <- data.table(periodX, ind_gr1, check.names=TRUE)
       X2 <- data.table(ind_gr1, X1)
       X1 <- X2[, .N, keyby=names(ind_gr1)][[ncol(ind_gr1)+1]]
       X2 <- X2[,lapply(.SD, function(x) sum(!is.na(x))), keyby=names(ind_gr1), .SDcols=nX1]
       X2 <- X2[, !(names(X2) %in% names(ind_gr1)), with=F]
       if (!all(X2==0 | X1==X2)) stop("X has unknown values")
       ind_gr1 <- nX1 <- X1 <- X2 <- NULL
    }

  # g
  if (!is.null(X)) {
    if (is.null(class(g))| class(g)=="function") stop("'g' must be numerical")
    g <- data.frame(g)
    if (nrow(g) != nrow(X)) stop("'g' length must be equal with 'X' row count")
    if (ncol(g) != 1) stop("'g' must be 1 column data.frame, matrix, data.table")
    g <- g[,1]
    if (!is.numeric(g)) stop("'g' must be numerical")
    if (any(is.na(g))) stop("'g' has unknown values")
    if (any(g == 0)) stop("'g' value can not be 0")
   }
    
  # q
  if (!is.null(X)) {
    if (is.null(class(q))| class(q)=="function") stop("'q' must be numerical")
    q <- data.frame(q)
    if (nrow(q) != nrow(X)) stop("'q' length must be equal with 'X' row count")
    if (ncol(q) != 1) stop("'q' must be 1 column data.frame, matrix, data.table")
    q <- q[,1]
    if (!is.numeric(q)) stop("'q' must be numerical")
    if (any(is.na(q))) stop("'q' has unknown values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }


  ### Calculation
      
  # Domains

  if (!is.null(Dom)) Y1 <- domain(Y, Dom) else Y1 <- Y

  # Design weights
  if (!is.null(X)) {
             idh <- ID_household
             if (!is.null(period)) idh <- data.table(period, idh)
             idhx <- data.table(X_ID_household, g)
             setnames(idhx, names(idhx)[c(1:(ncol(idhx)-1))], names(idh))
             idg <- merge(idh, idhx, by=names(idh))
             w_design <- w_final / idg[[ncol(idg)]]
             idhx <- idh <- NULL
      } else w_design <- w_final

      
  # Ratio of two totals
  
  Z1 <- persort <- linratio_outp <- NULL 
  estim <- var_est2 <- se <- rse <- NULL
  cv <- absolute_margin_of_error <- NULL
  relative_margin_of_error <- CI_lower <- NULL
  CI_upper <- variable <- variableZ <- .SD <- NULL
  deff_sam <- deff_est <- deff <- NULL
  
  aH <- names(H)
  idper <- id
  if (!is.null(period)) idper <- data.table(idper, period)

  if (!is.null(Z)) {
     if (!is.null(Dom)) Z1 <- domain(Z, Dom) else Z1 <- Z
            
     if (is.null(period)) {
          Y2 <- lin.ratio(Y1, Z1, w_final, Dom=NULL)
          Y2a <- lin.ratio(Y1, Z1, w_design, Dom=NULL)
        } else {
          periodap <- do.call("paste", c(as.list(period), sep="_"))
          sorts <- unlist(split(Y1[, .I], periodap))

          lin1 <- lapply(split(Y1[, .I], periodap), function(i) lin.ratio(Y1[i], Z1[i], w_final[i], Dom=NULL))
          Y2 <- rbindlist(lin1)[sorts]
           
          lin2 <- lapply(split(Y1[, .I], periodap), function(i) lin.ratio(Y1[i], Z1[i], w_design[i], Dom=NULL))
          Y2a <- rbindlist(lin2)[sorts]
        }
     if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
     if (outp_lin) linratio_outp <- data.table(idper, PSU, Y2) 
    } else {
            Y2 <- Y1
            Y2a <- Y1
          }
  lin1 <- lin2 <- NULL

  # Calibration

  YY <- data.table(idper, ID_household, H, PSU, w_final, Y2)
  YYa <- data.table(idper, ID_household, H, PSU, w_design, Y2a)
  
  YY2 <- YY[, lapply(.SD, sum, na.rm = T), by = c(names(YY)[c(2:(5+np))]), .SDcols = names(YY)[-(1:(5+np))]]
  YY2a <- YYa[, lapply(.SD, sum, na.rm = T), by = c(names(YYa)[c(2:(5+np))]), .SDcols = names(YYa)[-(1:(5+np))]]
  Y3 <- YY2[, c(-(1:(4+np))), with=F]
  Y3a <- YY2a[, c(-(1:(4+np))), with=F]

  period <- NULL
  if (np>0) period <- YY2[, c(1:np), with=F]

  IDh <- YY2[, np+1, with=F]
  H <- YY2[, np+2, with=F]
  setnames(H, names(H), aH)

  PSU <- YY2[, np+3, with=F]
  setnames(PSU, names(PSU), aPSU)

  w_final2 <- YY2[[np+4]]
  w_design2 <- YY2a[[np+4]]    
  YY <- YYa <- YY2 <- YY2a <- NULL

  # Calibration
  res_outp <- NULL
  if (!is.null(X)) {
       if (np>0) IDh <- data.table(period, IDh)
       setnames(IDh, names(IDh), names(X_ID_household))
       X0 <- data.table(X_ID_household, ind_gr, q, g, X)
       D1 <- data.table(merge(IDh, X0, by=names(IDh)))

       ind_gr <- D1[, np+2, with=F]
       if (!is.null(period)) ind_gr <- data.table(D1[, names(periodX), with=F], ind_gr)
       ind_period <- do.call("paste", c(as.list(ind_gr), sep="_"))
       sorts <- unlist(split(Y3[, .I], ind_period))
    
       lin1 <- lapply(split(Y3[, .I], ind_period), function(i) 
                   residual_est(Y=Y3[i],
                                X=D1[i,(np+5):ncol(D1),with=F],
                                weight=w_design2[i],
                                q=D1[i, np+3, with=F]))
       Y4 <- rbindlist(lin1)[sorts]
       if (outp_res) res_outp <- data.table(IDh, PSU, w_final2, Y4)
   } else Y4 <- Y3
  Y3 <- NULL
                                
  var_est <- variance_est(Y=Y4, H=H, PSU=PSU,
                          w_final=w_final2, N_h=N_h, 
                          fh_zero=fh_zero, PSU_level=PSU_level,
                          period=period, dataset=NULL)
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- var_est

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y=Y3a, H=H, PSU=PSU, w_final=w_design2,
                             N_h=N_h, fh_zero=fh_zero, PSU_level=PSU_level,
                             period=period, dataset=NULL)
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT)
  var_est <-  var_cur_HT <- NULL
  H <- PSU <- N_h <- NULL

  # Variance of HT estimator under SRS
  if (is.null(period)) {
           var_srs_HT <- var_srs(Y3a, w = w_design2)
       } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y3a)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          data.table(period_agg[i,], 
                                     var_srs(Y3a[ind], w = w_design2[ind]))
                        })
           var_srs_HT <- rbindlist(lin1)
      }
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT)

  # Variance of calibrated estimator under SRS
  if (is.null(period)) {
           var_srs_ca <- var_srs(Y4, w = w_final2)
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y3a)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          data.table(period_agg[i,], 
                                     var_srs(Y4[ind], w = w_final2[ind]))
                        })
           var_srs_ca <- rbindlist(lin1)
        }
  var_srs_ca <- transpos(var_srs_ca, is.null(period), "var_srs_ca", names(period))
  all_result <- merge(all_result, var_srs_ca)
  var_srs_HT <-  var_srs_ca <- NULL
  Y3a <- Y4 <- NULL

  # Total estimation
  Y_nov <- Z_nov <- .SD <- NULL

  hY <- data.table(Y1*w_final)
  if (is.null(period)) { Y_nov <- hY[, lapply(.SD, sum, na.rm = T), .SDcols = names(Y1)]
                } else { hY <- data.table(period, hY)
                         Y_nov <- hY[, lapply(.SD, sum, na.rm = T), keyby=names(period), .SDcols = names(Y1)]
                       }
  Y_nov <- transpos(Y_nov, is.null(period), "Y_nov", names(period))
  all_result <- merge(all_result, Y_nov)
  
  if (!is.null(Z1)) {
         YZnames <- data.table(variable=names(Y1), variableDZ=names(Z1))
         setkeyv(YZnames, "variable")
         setkeyv(all_result, "variable")
         all_result <- merge(all_result, YZnames)
         
         hZ <- data.table(Z1*w_final)
         if (is.null(period)) { Z_nov <- hZ[, lapply(.SD, sum, na.rm = T), .SDcols = names(Z1)]
                       } else { hZ <- data.table(period, hZ)
                                Z_nov <- hZ[, lapply(.SD, sum, na.rm = T), keyby=names(period), .SDcols = names(Z1)]
                              }
         Z_nov <- transpos(Z_nov, is.null(period), "Z_nov", names(period), "variableDZ")
         setkeyv(all_result, "variableDZ")
         all_result <- merge(all_result, Z_nov)                                            
      }

  vars <- data.table(variable=names(Y1), nr_names=1:ncol(Y1))
  setkey(vars, "variable")
  setkey(all_result, "variable")
  all_result <- merge(vars, all_result)
                        
  vars <- idper <- Y1 <- Z1 <- Y_nov <- NULL
  Z_nov <- hY <- hZ <- YZnames <- dati <- NULL
 
  all_result[, estim:=Y_nov]   
  all_result[!is.null(Z_nov), estim:=Y_nov/Z_nov]

  if (nrow(all_result[var_est < 0])>0) stop("Estimation of variance are negative!")
 
  # Effect of sample design
  all_result[, deff_sam:=var_cur_HT / var_srs_HT]
  
  # Effect of estimator
  all_result[, deff_est:= var_est / var_cur_HT]
  
  # Overall effect of sample design and estimator
  all_result[, deff:= deff_sam * deff_est]

  all_result[, var_est2:=var_est]
  all_result[xor(is.na(var_est2), var_est2 < 0), var_est2:=NA]
  all_result[, se:=sqrt(var_est2)]
  all_result[(estim!=0) & !is.nan(estim), rse:= se/estim]
  all_result[estim==0 | is.nan(estim), rse:=NA]
  all_result[, cv:= rse*100]


  tsad <- qnorm(0.5*(1+confidence))
  all_result[, absolute_margin_of_error:= tsad*se]
  all_result[, relative_margin_of_error:= tsad*cv]
  all_result[, CI_lower:= estim - tsad*se]
  all_result[, CI_upper:= estim + tsad*se]

  setnames(all_result, c("variable", "var_est"), c("variableD", "var"))
  if (!is.null(all_result$Z_nov)) {
                         nosrZ <- all_result$variableDZ
                         nosrZ <- nosrZ[!duplicated(nosrZ)]
                         nosrZ1 <- data.table(variableZ=t(data.frame(strsplit(nosrZ, "__")))[,c(1)])
                         nosrZ <- data.table(variableDZ=nosrZ, nosrZ1)
                         setkeyv(nosrZ, "variableDZ")
                         setkeyv(all_result, "variableDZ")
                         all_result <- merge(all_result, nosrZ)
                         nosrZ <- nosrZ1 <- NULL
                      }

  nosr <- data.table(variableD=all_result$variableD, t(data.frame(strsplit(all_result$variableD, "__"))))
  nosr <- nosr[!duplicated(nosr)]
  nosr <- nosr[, lapply(nosr, as.character)]
  setnames(nosr, names(nosr)[2], "variable")

  if (!is.null(Dom)) {
       setnames(nosr, names(nosr)[3:ncol(nosr)], names(Dom))
       nosr1 <- nosr[, lapply(names(Dom), function(x) {substring(get(x), nchar(x)+2, nchar(get(x)))})] 
       setnames(nosr1, names(nosr1), names(Dom))
       setnames(nosr, names(Dom), paste0(names(Dom),"old"))
       nosr <- data.table(nosr, nosr1)
    }
  setkeyv(nosr, "variableD")
  setkeyv(all_result, "variableD")
  all_result <- merge(nosr, all_result)
  nosr <- nosr1 <- NULL

  all_result[!is.null(Z_nov), variable:=paste("R", get("variable"), sep="__", get("variableZ"))] 

  variab <- c("estim", "var", "se", "rse", "cv", "absolute_margin_of_error",
              "relative_margin_of_error", "CI_lower", "CI_upper", "var_srs_HT",
              "var_cur_HT", "var_srs_ca", "deff_sam", "deff_est", "deff")

  setkeyv(all_result, c("nr_names", names(Dom), names(period)))
  all_result <- all_result[, c("variable", names(Dom), names(period), variab), with=F]
  
  list(lin_out = linratio_outp,
       res_out = res_outp,
       all_result = all_result)
}

