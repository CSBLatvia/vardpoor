 
vardom <- function(Y, H, PSU, w_final,
                   id = NULL,  
                   Dom = NULL,
                   N_h = NULL,
                   fh_zero=FALSE,
                   PSU_level=TRUE,
                   Z = NULL,
                   X = NULL,
                   ind_gr = NULL,
                   g = NULL,
                   dataset = NULL, 
                   q = rep(1, if (is.null(dataset)) 
                           nrow(data.frame(X)) else nrow(dataset)),
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
                                Z <-data.frame(dataset[, aZ],check.names=FALSE, stringsAsFactors=FALSE)
                                names(Z) <- aZ }}
      if(!is.null(X)) {
          if (min(X %in% names(dataset))!=1) stop("'X' does not exist in 'dataset'!")
          if (min(X %in% names(dataset))==1) X <- dataset[, X] }

      if(!is.null(ind_gr)) {
          if (min(ind_gr %in% names(dataset))!=1) stop("'ind_gr' does not exist in 'dataset'!")
          if (min(ind_gr %in% names(dataset))==1) ind_gr <- dataset[, ind_gr] } 

      if(!is.null(g)) {
          if (min(g %in% names(dataset))!=1) stop("'g' does not exist in 'dataset'!")
          if (min(g %in% names(dataset))==1) g <- dataset[, g] }
     
      if(!is.null(q)) {
          if (min(q %in% names(dataset))!=1) {
              if (length(q)!=nrow(dataset))  stop("'q' does not exist in 'dataset'!") }
          if (min(q %in% names(dataset))==1) q <- dataset[, q] } 
     
      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom2], stringsAsFactors=FALSE) 
                  names(Dom) <- Dom2 }    }
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
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  
  # id
  if (is.null(id)) id <- PSU
  id <- data.table(id)
  if (any(is.na(id))) stop("'id' has unknown values")
  if (nrow(id) != n) stop("'id' length must be equal with 'Y' row count")
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(id))||(names(id)=="id")) setnames(id,names(id),"ID")

  # w_final 
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[,1]
  if (!is.numeric(w_final)) stop("'w_final' must be numerical")
  if (any(is.na(w_final))) stop("'w_final' has unknown values") 

  # N_h
  if (!is.null(N_h)) {
    N_h <- data.table(N_h)
    if (ncol(N_h) != 2) stop("'N_h' should be two columns")
    if (!is.numeric(data.frame(N_h)[, 2])) stop("The second column of 'N_h' should be numerical")
    if (any(is.na(N_h))) stop("'N_h' has unknown values")
    if (is.null(names(N_h))) stop("'N_h' must be colnames")
    if (names(H) != names(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
    if (any(is.na(merge(unique(H), N_h, by=names(H), all.x = T)))) stop("'N_h' is not defined for all stratas")
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
      
  # X
  if (!is.null(X)) {
    X <- data.table(X, check.names=T)
    if (nrow(X) != n) stop("'X' and 'Y' have different row count")
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
       X2 <- data.table(ind_gr, X1)
       X1 <- X2[, .N, keyby=names(ind_gr)][[ncol(ind_gr)+1]]
       X2 <- X2[,lapply(.SD, function(x) sum(!is.na(x))), keyby=names(ind_gr), .SDcols=nX1]
       X2 <- X2[, !(names(X2) %in% names(ind_gr)), with=F]
       if (!all(X2==0 | X1==X2)) stop("X has unknown values")
       nX1 <- X1 <- X2 <- NULL
   }

  # g
  if (!is.null(X)) {
    if (is.null(class(g))| class(g)=="function") stop("'g' must be numerical")
    g <- data.frame(g)
    if (nrow(g) != nrow(X)) stop("'g' length must be equal with 'X' row count")
    if (ncol(g) != 1) stop("'g' must be vector or 1 column data.frame, matrix, data.table")
    g <- g[,1]
    if (!is.numeric(g)) stop("'g' must be numerical")
    if (any(is.na(g))) stop("'g' has unknown values")
    if (any(g == 0)) stop("'g' value can not be 0")
  }
    
  # q
  if (!is.null(X)) {
    q <- data.frame(q)
    if (nrow(q) != nrow(X)) stop("'q' length must be equal with 'X' row count")
    if (ncol(q) != 1) stop("'q' must be vector or 1 column data.frame, matrix, data.table")
    q <- q[,1]
    if (!is.numeric(q)) stop("'q' must be numerical")
    if (any(is.na(q))) stop("'q' has unknown values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }


  ### Calculation
      
  # Domains
  if (!is.null(Dom)) Y1 <- domain(Y, Dom) else Y1 <- Y

  # Design weights
  if (!is.null(X)) w_design <- w_final / g else w_design <- w_final
      
  # Ratio of two totals
  lin_outp <- NULL
  if (!is.null(Z)) {
    if (!is.null(Dom)) Z1 <- domain(Z, Dom) else Z1 <- Z
    Y2 <- lin.ratio(Y1, Z1, w_final, Dom=NULL)
    Y2a <- lin.ratio(Y1, Z1, w_design, Dom=NULL)
    if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
    if (outp_lin) ratio_outp <- data.table(id, PSU, Y2) 
  } else {
          Y2 <- Y1
          Y2a <- Y1
         }

  # Calibration

  res_outp <- NULL
  if (!is.null(X)) {
      Y3 <- residual_est(Y=Y2, X=X, weight=w_design, q=q, period=NULL, ind_gr=ind_gr)  
      if (outp_res) res_outp <- data.table(id, PSU, Y3)
  } else Y3 <- Y2
 
  var_est <- variance_est(Y = Y3, H = H, PSU = PSU, w_final = w_final,
                          N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                          period = NULL, dataset = NULL)

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y2a, H = H, PSU = PSU, w_final = w_design,
                             N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                             period = NULL, dataset = NULL)

  # Variance of HT estimator under SRS
  var_srs_HT <- var_srs(Y2a, w = w_design)
  
  # Variance of calibrated estimator under SRS
  var_srs_ca <- var_srs(Y3, w = w_final)

  # Total estimation
  Hstr <- data.table(rep.int(1,nrow(Y)))
  hY <- data.table(Hstr,Y1*w_final)
  .SD <- NULL
  Y_nov <- hY[, lapply(.SD, sum, na.rm = T), by = c(names(hY)[1]),.SDcols = names(hY)[-1]]

  if (!is.null(Z)) { hZ<-data.table(Hstr,Z1*w_final)          
                     Z_nov <- hZ[, lapply(.SD, sum, na.rm = T), by = c(names(hZ)[1])
                                 ,.SDcols = names(hZ)[-1]]
                     estim <- Y_nov[,-1,with=F]/Z_nov[,-1,with=F]
                    }  else estim <- Y_nov[,-1,with=F]

  estim2 <- data.table(estim)
  var_est <- data.table(var_est)
  var_est2 <- var_est
  if (!is.null(Z)) setnames(estim2, names(estim2), paste0(names(estim2),"_div_",names(Z)))

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
 
  setnames(var_est, names(var_est), paste0(names(estim2),"__var")) 
  setnames(se, names(se), paste0(names(estim2),"__se"))
  setnames(rse, names(rse), paste0(names(estim2),"__rse"))
  setnames(cv, names(cv), paste0(names(estim2),"__cv"))
  setnames(absolute_margin_of_error, names(absolute_margin_of_error), paste0(names(estim2),"__absolute_margin_of_error"))
  setnames(relative_margin_of_error, names(relative_margin_of_error), paste0(names(estim2),"__relative_margin_of_error"))
  setnames(CI_lower, names(CI_lower), paste0(names(estim2),"__CI_lower"))
  setnames(CI_upper, names(CI_upper), paste0(names(estim2),"__CI_upper"))
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
  setnames(nosr, names(nosr)[1], "variable")
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

  all_result <- data.table(nosr, all_result)
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

