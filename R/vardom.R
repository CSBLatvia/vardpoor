vardom <- function(Y, H, PSU, w_final, 
                   Dom = NULL,
                   N_h = NULL,
                   Z = NULL,
                   X = NULL,
                   g = NULL,
                   q = rep(1, if (is.null(dataset)) 
                           nrow(as.data.frame(H)) else nrow(dataset)),
                   confidence = .95,
                   dataset = NULL, 
                   breakdown = "TOTAL") {
 
  ### Checking

  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {
      aY <- Y
      if (min(Y %in% names(dataset))!=1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset))==1) {
                                Y <- data.frame(dataset[, Y],check.names=FALSE)
                                names(Y) <- aY }
      if(!is.null(H)) {
          a0 <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- data.frame(dataset[, H])
                                names(H) <- aH }}
      if(!is.null(PSU)) {
          aPSU <- PSU  
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) {
                                PSU <- data.frame(dataset[, PSU])
                                names(PSU) <- aPSU }}
      if(!is.null(Z)) {
          aZ <- Z
          if (min(Z %in% names(dataset))!=1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset))==1) {
                                Z <- dataset[, Z]
                                names(Z) <- aZ }}
      if(!is.null(X)) {
          if (min(X %in% names(dataset))!=1) stop("'X' does not exist in 'dataset'!")
          if (min(X %in% names(dataset))==1) X <- dataset[, X] }
      if(!is.null(g)) {
          if (min(g %in% names(dataset))!=1) stop("'g' does not exist in 'dataset'!")
          if (min(g %in% names(dataset))==1) g <- dataset[, g] }
      if(!is.null(q)) {
          if (min(q %in% names(dataset))!=1) stop("'q' does not exist in 'dataset'!")
          if (min(q %in% names(dataset))==1) q <- dataset[, q] }

      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom]) 
                  names(Dom) <- Dom2 }    }
      }

  # Y
  Y <- as.matrix(Y)
  n <- nrow(Y)
  m <- ncol(Y)
  if (any(is.na(Y))) stop("'Y' has unknown values")
  if (is.null(colnames(Y))) stop("'Y' must be colnames")
  
  # H
  H <- as.matrix(H)
  if (nrow(H) != n) stop("'H' length is not equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' has more than 1 column")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(colnames(H))) stop("'H' must be colnames")
  
  # PSU
  PSU <- as.matrix(PSU)
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  if (nrow(PSU) != n) stop("'PSU' length is not equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  
  # w_final 
  w_final <- as.vector(w_final)
  if (!is.numeric(w_final)) stop("'w_final' must be a numeric vector")
  if (length(w_final) != n) stop("'w_final' length is not equal with 'Y' row count")
  if (any(is.na(w_final))) stop("'w_final' has unknown values")
  
  # N_h
  if (!is.null(N_h)) {
    N_h <- as.data.frame(N_h)
    if (ncol(N_h) != 2) stop("'N_h' should be two columns")
    if (!is.numeric(N_h[, 2])) stop("The second column of 'N_h' should be numerical")
    if (any(is.na(N_h))) stop("'N_h' has unknown values")
    if (is.null(colnames(N_h))) stop("'N_h' must be colnames")
    if (colnames(H) != colnames(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
    if (any(is.na(merge(unique(H), N_h, all.x = T)))) stop("'N_h' is not defined for all stratas")
  }  

  # Dom
  if (!is.null(Dom)) {
    Dom <- as.matrix(Dom)
    if (nrow(Dom) != n) stop("'Dom' and 'Y' have different row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(colnames(Dom))) stop("'Dom' must be colnames")
  }
  
  # Z
  if (!is.null(Z)) {
    Z <- as.matrix(Z)
    if (nrow(Z) != n) stop("'Z' and 'Y' have different row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' have different column count")
    if (any(is.na(Z))) stop("'Z' has unknown values")
    if (is.null(colnames(Z))) stop("'Z' must be colnames")
  }
      
  # X
  if (!is.null(X)) {
    X <- as.matrix(X)
    if (nrow(X) != n) stop("'X' and 'Y' have different row count")
    if (any(is.na(X))) stop("'X' has unknown values")
  }
      
  # g
  if (!is.null(X)) {
    g <- as.vector(g)
    if (!is.numeric(g)) stop("'g' must be a numeric vector")
    if (length(g) != n) stop("'g' length is not equal with 'Y' row count")
    if (any(is.na(g))) stop("'g' has unknown values")
    if (any(g == 0)) stop("'g' value can not be 0")
  }
    
  # q
  if (!is.null(X)) {
    q <- as.vector(q)
    if (!is.numeric(q)) stop("'q' must be a numeric vector")
    if (length(q) != n) stop("'q' length is not equal with 'Y' row count")
    if (any(is.na(q))) stop("'q' has unknown values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }

  # breakdown
  breakdown <- toupper(breakdown)
  if (!(breakdown %in% c("TOTAL","STRATA"))) stop("the present method is not in the list of breakdown")

  ### Calculation
      
  # Domains
  if (!is.null(Dom)) Y1 <- domain(Y, Dom,"Y") else Y1 <- Y
      
  # Ratio of two totals
  if (!is.null(Z)) {
    if (!is.null(Dom)) Z1 <- domain(Z, Dom,"Z") else Z1 <- Z
    Y2 <- lin.ratio(Y1, Z1, w_final)
    if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
  } else Y2 <- Y1
 

  # Calibration
  if (!is.null(X)) Y3 <- residual_est(Y2, X, w_final/g, q)  else Y3 <- Y2


  var_est <- variance_est(Y3, H, PSU, w_final, N_h, dataset=NULL, breakdown)

  # Var srs
  Nn <- sum(w_final)
  konst <- Nn*Nn*(1-n/Nn)/n

  z <- colSums(Y3*w_final)
  z_z <- colSums(Y3^2*w_final)
  zm <- do.call(cbind, lapply(1:NCOL(Y3), function(i) rep(z[i]/Nn,n)))

  var_srs_o <- konst/(Nn-1)*colSums(w_final*(Y3-zm)^2)
  var_srs_l <- konst/(Nn-1)*(z_z-1/Nn*z^2)

  # Total estimation
  Hstr <- as.matrix(H)
  if (breakdown=="TOTAL") Hstr <- as.matrix(rep(1,nrow(Y)))
  Y_nov <- aggregate(Y1*w_final, by=list(Hstr), sum, na.rm=FALSE)
 
  if (!is.null(Z)) { Z_nov <- aggregate(Z1*w_final, by=list(Hstr), sum, na.rm=FALSE)
                      if (breakdown == "TOTAL") { estim <- Y_nov/Z_nov
                           } else {estim <- Y_nov[,2:ncol(Y_nov)]/Z_nov[,2:ncol(Z_nov)]
                             estim <- data.frame(Y_nov[,1],estim) }
                   }  else estim <- Y_nov

  if (breakdown == "STRATA") names(estim)[1] <- "STRATA" else estim <- estim[-1]
  
  if (breakdown == "STRATA") { estim2 <- as.matrix(estim[,2:ncol(estim)])
                              if (!is.null(Z)) colnames(estim2) <- paste(colnames(estim2),paste("_div_",colnames(Z),sep = ""),sep = "") 
                              var_est2 <- as.matrix(var_est[,2:ncol(var_est)])
                              colnames(var_est)[2:ncol(var_est)] <- paste(colnames(estim2),sep = "","__var") 
                     } else { #estim <- t(estim)
                              #var_est <- as.matrix(var_est)
                              estim2 <- estim
                              if (!is.null(Z)) colnames(estim2) <- paste(colnames(estim2),paste("_div_",colnames(Z),sep = ""),sep = "") 
                              var_est2 <- var_est
                              names(var_est) <- paste(colnames(estim2),sep = "","__var") 
                            }

  test_v <- (var_est2<0)
  test_v[is.na(test_v)] <- FALSE
  if (any(test_v)) stop("Estimation of variance are negative!")


  deff_o <- var_est2/var_srs_o
  deff_l <- var_est2/var_srs_l
  se <- sqrt(ifelse(var_est2>=0,var_est2,NA))
  rse <- se/estim2
  rse[estim2==0] <- NA 
  cv <- rse*100
  tsad <- qnorm(0.5*(1+confidence))
  Absolute_margin_of_error <- tsad*se 
  Relative_margin_of_error <- tsad*cv
  CI_lower <- estim2 - tsad*se
  CI_upper <- estim2 + tsad*se
 
  colnames(rse) <- paste(colnames(estim2),sep = "","__rse")
  colnames(cv) <- paste(colnames(estim2),sep = "","__cv")
  colnames(Relative_margin_of_error) <- paste(colnames(estim2),sep = "","__rel_marg_err")
  colnames(CI_lower) <- paste(colnames(estim2),sep = "","__CI_lower")
  colnames(CI_upper) <- paste(colnames(estim2),sep = "","__CI_upper")


  if (breakdown == "STRATA") {
                              colnames(se) <- paste(colnames(estim2),sep = "","__se")
                              colnames(deff_o) <- paste(colnames(estim2),sep = "","__deff_o")
                              colnames(deff_l) <- paste(colnames(estim2),sep = "","__deff_l")  
                              names(var_srs_o) <- paste(colnames(estim2),sep = "","__var_srs_o")
                              names(var_srs_l) <- paste(colnames(estim2),sep = "","__var_srs_l")
                              
                              colnames(Absolute_margin_of_error) <- paste(colnames(estim2),sep = "","__abs_marg_err")
                              stratas <- data.frame(sort(unique(H))) 
                              names(stratas)[1] <- colnames(H)
                              all_result <- cbind(estim,var_est,se,rse,cv,Absolute_margin_of_error,Relative_margin_of_error,CI_lower,CI_upper)
                              se <- data.frame(stratas,se)
                              rse <- data.frame(stratas,rse)
                              cv <- data.frame(stratas,cv)
                              Absolute_margin_of_error <- data.frame(stratas,Absolute_margin_of_error)
                              Relative_margin_of_error <- data.frame(stratas,Relative_margin_of_error)
                              CI_lower <- data.frame(stratas,CI_lower)
                              CI_upper <- data.frame(stratas,CI_upper)
                              colnames(estim)[2:ncol(var_est)] <- paste(colnames(estim2),sep = "","__est")
                              } else {
                              names(se) <- paste(colnames(estim2),sep = "","__se")
                              names(var_srs_o) <- paste(colnames(estim2),sep = "","__var_srs_o")
                              names(var_srs_l) <- paste(colnames(estim2),sep = "","__var_srs_l")
                              names(deff_o) <- paste(colnames(estim2),sep = "","__deff_o")
                              names(deff_l) <- paste(colnames(estim2),sep = "","__deff_l")
                              names(Absolute_margin_of_error) <- paste(colnames(estim2),sep = "","__abs_marg_err")
                              }

if (breakdown == "TOTAL") { 
   all_result <- cbind(t(estim),as.matrix(var_est),as.matrix(se),t(rse),t(cv),
                     as.matrix(Absolute_margin_of_error),t(Relative_margin_of_error),
                     t(CI_lower),t(CI_upper),as.matrix(var_srs_o),as.matrix(var_srs_l),
                     as.matrix(deff_o),as.matrix(deff_l))
   colnames(all_result) <- c("estim","var","se","rse","cv","Absolute_margin_of_error",
                            "Relative_margin_of_error","CI_lower","CI_upper",
                            "Var_SRS_o","Var_SRS_l","Deff_o","Deff_l")
 
   nosr <- data.frame(t(data.frame(strsplit(colnames(estim), "__"))))
   colnames(nosr)[1] <- "variable"
   rownames(nosr) <- NULL
   nosr <- as.matrix(nosr)
   sakum <- 2  
   if (!is.null(Dom)) {
        colnames(nosr)[sakum:ncol(nosr)] <- colnames(Dom)
        nch1 <- matrix(nchar(colnames(Dom))+2, nrow=1)
        nosr[,sakum:ncol(nosr)] <- substring(nosr[,sakum:ncol(nosr)],nch1[rep(1, nrow(nosr)),],nchar(nosr[,sakum:ncol(nosr)]))
        }

   if (!is.null(Z)) { 
         Zmat <- data.frame(nosr[,1],colnames(Z))
         nosr[,1] <- paste("R",nosr[,1],sep="__",Zmat[,2]) }

   all_result <- data.frame(nosr,all_result)
   names(estim) <- paste(colnames(estim2),sep = "","__est") 
   }

list(estim=estim,
       var=var_est,
       se=se,
       rse=rse,
       cv=cv,
       absolute_margin_of_error=Absolute_margin_of_error,
       relative_margin_of_error=Relative_margin_of_error,
       CI_lower=CI_lower,
       CI_upper=CI_upper,
       var_srs_o=var_srs_o,
       var_srs_l=var_srs_l,
       deff_o=deff_o,
       deff_l=deff_l,
       all_result=all_result)
}

