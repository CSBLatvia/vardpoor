
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
      data <- data.frame(dataset)
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
  if (nrow(id) != n) stop("'id' length must be equal with 'Y' row count")
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(id))||(names(id)=="id")) setnames(id,names(id),"ID")
  if (names(id)==names(ID_household)) setnames(id,names(id),paste(names(id),"_id",sep=""))

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
    Z <- data.table(Z)
    if (nrow(Z) != n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' must be equal column count")
    if (any(is.na(Z))) stop("'Z' has unknown values")
    if (is.null(names(Z))) stop("'Z' must be colnames")
  }
      
 # X_ID_household
  if (!is.null(X)) {
    X_ID_household <- data.table(X_ID_household)
    if (ncol(X_ID_household) != 1) stop("'X_ID_household' must be 1 column data.frame, matrix, data.table")
    if (any(is.na(X_ID_household))) stop("'X_ID_household' has unknown values")

    IDh <- data.table(unique(ID_household))
    if (!is.null(period)) {X_ID_household <- data.table(periodX, X_ID_household)
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
        if (any(is.na(periodX))) stop("'periodX' has unknown values")
        if (any(peri != periX)) stop("'unique(period)' and 'unique(periodX)' records have different")
      }
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
       X2 <- X2[, !(names(X2) %in% names(ind_gr)), with=F]
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
  Dom1 <- Dom
  if (!is.null(period)) {
     if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 

  if (!is.null(Dom1)) Y1 <- domain(Y, Dom1) else Y1 <- Y

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
  lin_outp <- NULL
  if (!is.null(Z)) {
    if (!is.null(Dom1)) Z1 <- domain(Z, Dom1) else Z1 <- Z
    Y2 <- lin.ratio(Y1, Z1, w_final, Dom=NULL)
    Y2a <- lin.ratio(Y1, Z1, w_design, Dom=NULL)
    if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
    if (outp_lin) ratio_outp <- data.table(id, PSU, Y2) 
  } else {
           Y2 <- Y1
           Y2a <- Y1
         }

  # Calibration


  .SD <- NULL
  aH <- names(H)
  idper <- id
  if (!is.null(period)) idper <- data.table(idper, period)
  YY <- data.table(idper, ID_household, H, PSU, w_final, Y2)
  YYa <- data.table(idper, ID_household, H, PSU, w_design, Y2a)
  idper <- NULL
  
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

  w_final2 <- data.frame(YY2)[,np+4]
  w_design2 <- data.frame(YY2a)[,np+4]    
  YY <- YYa <- NULL
  YY2 <- YY2a <- NULL

  # Calibration
  res_outp <- NULL
  if (!is.null(X)) {
       if (np>0) IDh <- data.table(period, IDh)
       setnames(IDh, names(IDh), names(X_ID_household))
       X0 <- data.table(X_ID_household, g, q, X)
       D1 <- merge(IDh, X0, by=names(IDh))
       g <- data.frame(D1)[,np+2]
       q <- data.frame(D1)[,np+3]
       X <- D1[,(np+4):ncol(D1),with=F]
            
       Y4 <- residual_est(Y=Y3, X=X, weight=w_design2,
                          q=q, period=period, ind_gr=ind_gr)  
       if (outp_res) res_outp <- data.table(IDh, PSU, w_final2, Y4)
   } else Y4 <- Y3
  Y3 <- NULL

  var_est <- variance_est(Y=Y4, H=H, PSU=PSU, w_final=w_final2,
                          N_h=N_h, fh_zero=fh_zero, PSU_level=PSU_level,
                          period=period, dataset=NULL)
  np <- sum(ncol(period))
  if (np>0) var_est <- data.table(t(colSums(var_est[,-c(1:np),with=F], na.rm=T)))


  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y=Y3a, H=H, PSU=PSU, w_final=w_design2,
                             N_h=N_h, fh_zero=fh_zero, PSU_level=PSU_level,
                             period=period, dataset=NULL)
  if (np>0) var_cur_HT <- data.table(t(colSums(var_cur_HT[,-c(1:np),with=F], na.rm=T)))

  # Variance of HT estimator under SRS
  var_srs_HT <- var_srs(Y=Y3a, w = w_design2)
  
  # Variance of calibrated estimator under SRS
  var_srs_ca <- var_srs(Y=Y4, w = w_final2)

  # Total estimation
  Hstr <- data.table(rep.int(1,nrow(Y)))
  
  hY<-data.table(Hstr,Y1*w_final)
  .SD <- NULL
  Y_nov <- hY[, lapply(.SD, sum, na.rm = F), by = c(names(hY)[1]),.SDcols = names(hY)[-1]]

  if (!is.null(Z)) { hZ<-data.table(Hstr,Z1*w_final)          
                     Z_nov <- hZ[, lapply(.SD, sum, na.rm = T), by = c(names(hZ)[1])
                                 ,.SDcols = names(hZ)[-1]]
                     estim <- Y_nov[,-1,with=F]/Z_nov[,-1,with=F]
                   } else estim <- Y_nov[,-1,with=F]

  estim2 <- data.table(estim)
  var_est <- data.table(var_est)
  var_est2 <- var_est
 
  if (!is.null(Z)) setnames(estim2,names(estim2),paste0(names(estim2), "_div_", names(Z)))
 
  test_v <- (var_est2<0)
  test_v[is.na(test_v)] <- FALSE
  if (any(test_v)) stop("Estimation of variance are negative!")
 
  # Effect of sample design
  deff_sam <- var_cur_HT / var_srs_HT
  
  # Effect of estimator
  deff_est <- var_est2 / var_cur_HT
  
  # Overall effect of sample design and estimator
  deff <- deff_sam * deff_est

  var_est3 <- var_est2
  var_est3[xor(is.na(var_est3), test_v)] <- 0
  se <- sqrt(var_est3)
  se[xor(is.na(var_est3), test_v)] <- NA
  rse <- se/estim2
  rse[estim2==0] <- NA 
  cv <- rse*100
  tsad <- qnorm(0.5*(1+confidence))
  absolute_margin_of_error <- tsad*se 
  relative_margin_of_error <- tsad*cv
  CI_lower <- estim2 - tsad*se
  CI_upper <- estim2 + tsad*se
 
  setnames(var_est, names(var_est), paste0(names(estim2), "__var")) 
  setnames(se, names(se), paste0(names(estim2), "__se"))
  setnames(rse, names(rse), paste0(names(estim2), "__rse"))
  setnames(cv, names(cv), paste0(names(estim2), "__cv"))
  setnames(absolute_margin_of_error, names(absolute_margin_of_error), paste0(colnames(estim2), "__absolute_margin_of_error"))
  setnames(relative_margin_of_error, names(relative_margin_of_error), paste0(names(estim2), "__relative_margin_of_error"))
  setnames(CI_lower,names(CI_lower), paste0(names(estim2), "__CI_lower"))
  setnames(CI_upper,names(CI_upper), paste0(names(estim2), "__CI_upper"))
  setnames(var_srs_HT, names(var_srs_HT), paste0(names(estim2),"__var_srs_HT"))
  setnames(var_srs_ca, names(var_srs_ca), paste0(names(estim2),"__var_srs_ca"))
  setnames(var_cur_HT, names(var_cur_HT), paste0(names(estim2),"__var_cur_HT"))
  setnames(deff_sam, names(deff_sam), paste0(names(estim2),"__deff_sam"))
  setnames(deff_est, names(deff_est), paste0(names(estim2),"__deff_est"))
  setnames(deff, names(deff), paste0(names(estim2),"__deff"))

  all_result <- cbind(t(estim), t(var_est), t(se), t(rse), t(cv),
                      t(absolute_margin_of_error),
                      t(relative_margin_of_error),  
                      t(CI_lower),t(CI_upper), t(var_srs_HT),
                      t(var_cur_HT), t(var_srs_ca),
                      t(deff_sam), t(deff_est), t(deff))
 
  colnames(all_result) <- c("estim", "var", "se", "rse", "cv",
                            "absolute_margin_of_error",
                            "relative_margin_of_error",
                            "CI_lower", "CI_upper", "var_srs_HT",
                            "var_cur_HT", "var_srs_ca", 
                            "deff_sam", "deff_est", "deff")


  nosr <- data.table(t(data.frame(strsplit(names(estim), "__"))))
  setnames(nosr,names(nosr)[1],"variable")
  nosr <- as.matrix(nosr)
  sakum <- 2  
  if (!is.null(Dom)) {
       colnames(nosr)[sakum:ncol(nosr)] <- colnames(Dom)
       nch1 <- matrix(nchar(colnames(Dom))+2, nrow=1)
       nosr[,sakum:ncol(nosr)] <- substring(nosr[,sakum:ncol(nosr)],nch1[rep(1, nrow(nosr)),],nchar(nosr[,sakum:ncol(nosr)]))  
     }

  if (!is.null(Z)) { 
        Zmat <- data.frame(nosr[,1], names(Z))
        nosr[,1] <- paste("R", nosr[,1], sep="__", Zmat[,2]) }

  all_result <- data.frame(nosr, all_result)
  rownames(all_result) <- NULL
  setnames(estim, names(estim), paste0(names(estim2), "__est")) 
  list(estim = estim,
       var = var_est,
       se = se,
       rse = rse,
       cv = cv,
       absolute_margin_of_error = absolute_margin_of_error,
       relative_margin_of_error = relative_margin_of_error,
       CI_lower = CI_lower,
       CI_upper = CI_upper,
       var_srs_HT = var_srs_HT,
       var_cur_HT = var_cur_HT,
       var_srs_ca = var_srs_ca,
       deff_sam = deff_sam,
       deff_est = deff_est,
       deff = deff,
       lin_out = lin_outp,
       res_out = res_outp,
       all_result = all_result)
}

