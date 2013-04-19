
var_srs <- function(Y, w_final) {
 
  ### Checking
  # Y
  Y <- as.matrix(Y)
  n <- nrow(Y)
  if (any(is.na(Y))) print("'Y' has unknown values")
  if (is.null(colnames(Y))) stop("'Y' must be colnames")
    
  # w_final 
  w_final <- as.vector(w_final)
  if (!is.numeric(w_final)) stop("'w_final' should be numerical")
  if (length(w_final) != n) stop("'w_final' length îs not equal with 'Y' row count")
  if (any(is.na(w_final))) stop("'w_final' has unknown values")
    
  ### Calculation
   
  # z
  z <- colSums(Y*w_final)
  z_z <- colSums(Y^2*w_final)

  # N
  Nn <- sum(w_final)
  konst <- Nn*Nn*(1-n/Nn)/((Nn-1)*n)
 
  var_srs_osier <- konst*(colSums((Y-z/Nn)^2*w_final))
  var_srs_lapins <- konst*(z_z-1/Nn*z^2)
  list(var_srs_osier <- var_srs_osier, var_srs_lapins=var_srs_lapins)
}

