variance_est <- function(Y, H, PSU, w_final, N_h, dataset=NULL, breakdown = "TOTAL") {

  ### Checking
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
      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset))!=1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset))==1) w_final <- dataset[, w_final] }

      }

  # Y
  Y <- as.matrix(Y)
  n <- nrow(Y)
  m <- ncol(Y)
  if (any(is.na(Y))) print("'Y' has unknown values")
  if (is.null(colnames(Y))) stop("'Y' must be the column names")
  
  # H
  H <- as.matrix(H)
  if (nrow(H) != n) stop("'H' length is not equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' has more than 1 column")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(colnames(H))) stop("'H' must be colnames")
  
  # PSU
  PSU <- as.matrix(PSU)
  if (nrow(PSU) != n) stop("'PSU' length is not equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  
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
  } else {
    N_h <- aggregate(w_final, by = list(H[,]), sum, na.rm = FALSE)
    names(N_h)[1:2] <- c(colnames(H), "N_h")
  }
  
  ### Calculation
  
  # z_hi
  z_hi <- aggregate(Y*w_final, by = list(H[,], PSU[,]), sum, na.rm = FALSE)
  names(z_hi)[1:2] <- c(colnames(H), colnames(PSU))

  # n_h
  n_h <- aggregate(rep(1, nrow(z_hi)), by = list(z_hi[,1]), sum)
  names(n_h)[1:2] <- c(colnames(H), "n_h")
  
  if (any(n_h$n_h == 1)) {
    print("There is stratas, where n_h == 1")
    print("Not possible to estimate the variance in these stratas!")
    print("At these stratas estimation of variance was not calculated")
    print(n_h[n_h$n_h == 1, ])
  }
  
  # var_z_hi
  var_z_hi <- aggregate(z_hi[-(1:2)], by = list(z_hi[,1]), var)
  names(var_z_hi)[1] <- colnames(H)

  # f_h
  F_h <- merge(N_h, n_h, by = colnames(H), sort = T)
  f_h <- F_h[, 3] / F_h[, 2]
  
  if (any(f_h > 1)) {    
    print("There is stratas, where f_h > 1")
    print("At these stratas estimation of variance will be 0")
    print(F_h[f_h > 1, ])
    f_h[f_h > 1] <- 1
  }

  # var_h
  var_h <- as.matrix((1 - f_h) * n_h$n_h * var_z_hi[, -1])
  colnames(var_h) <- colnames(var_z_hi)[-1]   

  # Variance_est

  if (breakdown == "STRATA") { var_est <- data.frame(sort(unique(H)),var_h)
                              colnames(var_est)[1] <- "STRATA"
                            } else var_est <- colSums(var_h, na.rm=T)

  return(var_est)
}
