round2 <- function(x, n) {
  sign(x) * trunc(abs(x) * 10 ^ n + 0.5) / 10 ^ n
}


variance_est <- function(Y, H, PSU, w_final, N_h = NULL, fh_zero = FALSE,
                         PSU_level = TRUE, PSU_sort = NULL, period = NULL, 
                         dataset = NULL, msg = "", checking = TRUE) {

  ### Checking

  if (checking) {
        fh_zero <- check_var(vars = fh_zero, varn = "fh_zero", varntype = "logical") 
        PSU_level <- check_var(vars = PSU_level, varn = "PSU_level", varntype = "logical") 
  
        Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                       check.names = TRUE, isnumeric = TRUE)
        Ynrow <- nrow(Y)
        Yncol <- ncol(Y)
    
        H <- check_var(vars = H, varn = "H", dataset = dataset,
                       ncols = 1, Ynrow = Ynrow, isnumeric = FALSE,
                       ischaracter = TRUE)
    
        w_final <- check_var(vars = w_final, varn = "w_final",
                             dataset = dataset, ncols = 1, Ynrow = Ynrow,
                             isnumeric = TRUE, isvector = TRUE)
        
        period <- check_var(vars = period, varn = "period",
                           dataset = dataset, Ynrow = Ynrow,
                           ischaracter = TRUE, mustbedefined = FALSE,
                           duplicatednames = TRUE)

        PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                         ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                         namesID1 = names(id))
     
        PSU_sort <- check_var(vars = PSU_sort, varn = "PSU_sort", dataset = dataset,
                              ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                              isvector = TRUE, mustbedefined = FALSE, PSUs = PSU)
    }

  np <- sum(ncol(period))
  vars <- names(period)


  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (anyNA(N_h)) stop("'N_h' has missing values") 
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", toString(np + 2)," columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numeric")
      nams <- c(names(period), names(H))
      if (all(nams %in% names(N_h))) {N_h[, (nams) := lapply(.SD, as.character), .SDcols = nams]
             } else stop(paste0("All strata titles of 'H'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h'"))
   
      if (is.null(period)) {
             if (any(is.na(merge(unique(H), N_h, by = names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
             if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                }
    setnames(N_h, names(N_h)[ncol(N_h)], "N_h")
    setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  } else {
    Nh <- data.table(H, w_final)
    if (!is.null(period)) Nh <- data.table(period, Nh)
    N_h <- Nh[, .(N_h = sum(w_final, na.rm = TRUE)), keyby = c(names(Nh)[1 : (1 + np)])]
  }
  psuag <- pH <- NULL  

  ### Calculation
  namY <- names(Y)

  # z_hi
  ids <- nhc <- f_h <- .SD <- N <- NULL
  hpY <- data.table(H, PSU, Y * w_final)
  if (!is.null(PSU_sort)) hpY <- data.table(H, PSU, PSU_sort, Y * w_final)
  if (!is.null(period)) hpY <- data.table(period, hpY)
  psusn <- as.integer(!is.null(PSU_sort))
  z_hi <- hpY[, lapply(.SD, sum, na.rm = TRUE), 
                       keyby = c(names(hpY)[1 : (2 + np + psusn)]),
                       .SDcols = names(hpY)[-(1 : (2 + np + psusn))]]
  setkeyv(z_hi, names(z_hi)[c(1 : (1 + np))]) 

  # n_h
  n_h <- data.table(z_hi[, c(1 : (1 + np)), with = FALSE])
  n_h <- n_h[, .(n_h = .N), keyby = c(names(n_h)[1 : (1 + np)])]

  # var_z_hi
  var_z_hi <- z_hi[, lapply(.SD, var, na.rm = FALSE), keyby = c(names(z_hi)[1 : (1 + np)]), .SDcols = namY]

  if (!is.null(PSU_sort)) {
       setkeyv(z_hi, c(names(z_hi)[c(1:(1 + np),3 + np)]))
       z_hi[, (paste0("lag_", namY)) := lapply(.SD, function(x) shift(x, 1)),
                               by=c(names(z_hi)[1 : (1 + np)]), .SDcols = namY]


       laY <- paste0("lag_", namY[1])
       z_hi <- z_hi[!is.na(get(laY))]

       var_z_hi <- z_hi[, lapply(namY, function(x) 
                                 sum((get(x) - get(paste0("lag_", x)))^2)),
                                 keyby = c(names(z_hi)[1 : (1 + np)])]
       setnames(var_z_hi, names(var_z_hi)[(2 + np) : ncol(var_z_hi)], namY)
   }

  # f_h
  F_h <- merge(N_h, n_h, by = names(hpY)[c(1 : (1 + np))], sort = TRUE)
  F_h[, N_h := round2(N_h, 8)]
  F_h[, f_h := n_h / N_h]

  if (nrow(F_h[n_h == 1 & f_h != 1]) > 0) {
    print(msg)
    print("There are strata, where n_h == 1 and f_h <> 1")
    print("Not possible to estimate the variance in these strata!")
    print("At these strata estimation of variance was not calculated")
    nh <- F_h[n_h == 1 & f_h != 1]
    print(nh)
  }
  
  if (nrow(F_h[f_h > 1]) > 0) {    
     print(msg)
     print("There are strata, where f_h > 1")
     print("At these strata estimation of variance will be 0")
     print(F_h[f_h > 1])
     F_h[f_h > 1, f_h := 1]
   }

  # fh1
  if (!(PSU_level)) {
       n_h1 <- Nh1 <- NULL
       fh1 <- data.table(hpY[, c(1 : (1 + np)), with = FALSE], w_final)
       fh1 <- fh1[, .(n_h1 = .N, Nh1 = sum(w_final, na.rm = TRUE)), keyby = c(names(fh1)[1 : (1 + np)])]
       F_h <- merge(F_h, fh1, by = c(names(fh1)[1 : (1 + np)]))
       F_h[, f_h := n_h1 / Nh1]
     }

  var_z_hi <- merge(F_h, var_z_hi, by = c(names(F_h)[1 : (1 + np)]))
  fh1 <- F_h <- NULL

  # var_h

  if (!is.null(PSU_sort)) {
         var_z_hi[, nhc := ifelse(n_h > 1, n_h / (2 * (n_h - 1)), NA)]
    } else var_z_hi[, nhc := n_h]

  var_z_hi[, ids := 1 : .N]
  var_z_hi[, (paste0("var_", namY)) := lapply(.SD[, namY, with = FALSE],
                                           function(x) (1 - f_h * (1 - fh_zero)) * nhc * x), by = "ids"]
  # Variance_est 
  
  var_est <- var_z_hi[, lapply(.SD, sum, na.rm = TRUE), 
                                  keyby = vars, .SDcols = paste0("var_", namY)]
  setnames(var_est, paste0("var_", namY), namY)
  return(var_est)
}



