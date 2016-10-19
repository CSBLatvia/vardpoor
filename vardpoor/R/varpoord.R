
varpoord <- function(Y, w_final,
                     age = NULL,
                     pl085 = NULL,
                     month_at_work=NULL,
                     Y_den = NULL,
                     Y_thres = NULL,
                     wght_thres = NULL,                    
                     ID_level1,
                     ID_level2 = NULL, 
                     H, PSU, N_h,
                     fh_zero = FALSE,
                     PSU_level=TRUE,
                     sort = NULL,
                     Dom = NULL,
                     period = NULL,
                     gender = NULL,
                     dataset = NULL,
                     X = NULL,
                     periodX = NULL,
                     X_ID_household = NULL,
                     ind_gr = NULL,
                     g = NULL,
                     q = NULL,
                     datasetX = NULL,
                     percentage = 60,
                     order_quant = 50,
                     alpha = 20,
                     confidence = .95,
                     outp_lin = FALSE,
                     outp_res = FALSE,
                     type="linrmpg") {

  ### Checking

  if (length(fh_zero) != 1 | !any(is.logical(fh_zero))) stop("'fh_zero' must be logical")
  if (length(PSU_level) != 1 | !any(is.logical(PSU_level))) stop("'PSU_level' must be logical")
  if (length(outp_lin) != 1 | !any(is.logical(outp_lin))) stop("'outp_lin' must be logical")
  if (length(outp_res) != 1 | !any(is.logical(outp_res))) stop("'outp_res' must be logical")

  all_choices <- c("linarpr","linarpt","lingpg","linpoormed",
                   "linrmpg","lingini","lingini2", "linqsr", "linrmir", "linarr")
  choices <- c("all_choices", all_choices)
  type <- tolower(type)
  type <- match.arg(type, choices, length(type) > 1) 
  if (any(type == "all_choices")) type <- all_choices 

  # check 'p'
  p <- percentage
   if(length(p) != 1 |  any(!is.numeric(p) | p < 0 | p > 100)) {
          stop("'percentage' must be a numeric value in [0, 100]")  }

  # check 'order_quant'
  oq <- order_quant
   if(length(oq) != 1 | any(!is.numeric(oq) | oq < 0 | oq > 100)) {
          stop("'order_quant' must be a numeric value in [0, 100]")  }

  if(length(alpha) != 1 | any(!is.numeric(alpha) | alpha < 0 | alpha > 100)) {
         stop("'alpha' must be a numeric value in [0,100]")  }

  if(length(confidence) != 1 | any(!is.numeric(confidence) | confidence < 0 | confidence > 1)) {
         stop("'confidence' must be a numeric value in [0, 1]")  }

  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset)) == 1) Y <- dataset[, Y, with = FALSE]
      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset)) != 1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset)) == 1) w_final <- dataset[, w_final, with = FALSE] }
      if(!is.null(age)) {
          if (min(age %in% names(dataset)) != 1) stop("'age' does not exist in 'dataset'!")
          if (min(age %in% names(dataset)) == 1) age <- dataset[, age, with = FALSE] }
      if(!is.null(pl085)) {
          if (min(pl085 %in% names(dataset)) != 1) stop("'pl085' does not exist in 'dataset'!")
          if (min(pl085 %in% names(dataset)) == 1) pl085 <- dataset[, pl085, with = FALSE] }
      if(!is.null(month_at_work)) {
          if (min(month_at_work %in% names(dataset)) != 1) stop("'month_at_work' does not exist in 'dataset'!")
          if (min(month_at_work %in% names(dataset)) == 1) month_at_work <- dataset[, month_at_work, with = FALSE] }
      if(!is.null(Y_den)) {
          if (min(Y_den %in% names(dataset)) != 1) stop("'Y_den' does not exist in 'dataset'!")
          if (min(Y_den %in% names(dataset)) == 1) Y_den <- dataset[, Y_den, with = FALSE] }
      if(!is.null(Y_thres)) {
          if (min(Y_thres %in% names(dataset)) != 1) stop("'Y_thres' does not exist in 'dataset'!")
          if (min(Y_thres %in% names(dataset)) == 1) Y_thres <- dataset[, Y_thres, with = FALSE] }    
      if(!is.null(wght_thres)) {
          if (min(wght_thres %in% names(dataset)) != 1) stop("'wght_thres' does not exist in 'dataset'!")
          if (min(wght_thres %in% names(dataset)) == 1) wght_thres <- dataset[, wght_thres, with = FALSE] }
      if(!is.null(id)) {
          if (min(id %in% names(dataset)) != 1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset)) == 1) id <- dataset[, id, with = FALSE]  }
      if(!is.null(ID_household)) {
          if (min(ID_household %in% names(dataset)) != 1) stop("'ID_household' does not exist in 'dataset'!")
          if (min(ID_household %in% names(dataset)) == 1) ID_household <- dataset[, ID_household, with = FALSE] }
      if(!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE] }
      if(!is.null(PSU)) {
          if (min(PSU %in% names(dataset)) != 1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset)) == 1) PSU <- dataset[, PSU, with = FALSE]  }
      if(!is.null(gender)) {
          if (min(gender %in% names(dataset)) != 1) stop("'gender' does not exist in 'dataset'!")
          if (min(gender %in% names(dataset)) == 1) gender <- dataset[, gender, with = FALSE] }
      if(!is.null(sort)) {
          if (min(sort %in% names(dataset)) != 1) stop("'sort' does not exist in 'dataset'!")
          if (min(sort %in% names(dataset)) == 1) sort <- dataset[, sort, with = FALSE] }
      if (!is.null(period)) {
            if (min(period %in% names(dataset)) != 1) stop("'period' does not exist in 'dataset'!")
            if (min(period %in% names(dataset)) == 1) period <- dataset[, period, with = FALSE] }
      if (!is.null(Dom)) {
          if (min(Dom %in% names(dataset)) != 1) stop("'Dom' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset)) == 1) Dom <- dataset[, Dom, with = FALSE] }
    }

  if (is.null(datasetX)) datasetX <- copy(dataset)
  if(!is.null(datasetX)) {
      dataset <- data.table(datasetX)
       if (!is.null(periodX)) {
            if (min(periodX %in% names(datasetX)) != 1) stop("'periodX' does not exist in 'datasetX'!")
            if (min(periodX %in% names(datasetX)) == 1) periodX <- datasetX[, periodX, with = FALSE] }     
      if(!is.null(X_ID_household)) {
          if (min(X_ID_household %in% names(datasetX)) != 1) stop("'X_ID_household' does not exist in 'datasetX'!")
          if (min(X_ID_household %in% names(datasetX)) == 1) X_ID_household <- datasetX[, X_ID_household, with = FALSE] }
      if(!is.null(X)) {
          if (min(X %in% names(datasetX)) != 1) stop("'X' does not exist in 'datasetX'!")
          if (min(X %in% names(datasetX)) == 1) X <- datasetX[, X, with = FALSE] }
      if(!is.null(ind_gr)) {
          if (min(ind_gr %in% names(datasetX)) != 1) stop("'ind_gr' does not exist in 'datasetX'!")
          if (min(ind_gr %in% names(datasetX)) == 1) ind_gr <- datasetX[, ind_gr, with = FALSE] }
      if(!is.null(g)) {
          if (min(g %in% names(datasetX)) != 1) stop("'g' does not exist in 'datasetX'!")
          if (min(g %in% names(datasetX)) == 1) g <- datasetX[, g, with = FALSE] }
      if(!is.null(q)) {
          if (min(q %in% names(datasetX)) != 1) {
               if (length(q) != nrow(datasetX))  stop("'q' does not exist in 'datasetX'!") }
          if (min(q %in% names(datasetX)) == 1) q <- datasetX[, q, with = FALSE]  }
    }
  N <- dataset <- datasetX <- NULL

  # Y
  Y <- data.frame(Y)
  n <- nrow(Y)
  if (ncol(Y) != 1) stop("'Y' must be vector or 1 column data.frame, matrix, data.table")
  Y <- Y[, 1]
  if (!is.numeric(Y)) stop("'Y' must be numeric")
  if (any(is.na(Y))) stop("'Y' has missing values")
  
   if (!is.null(Y_den)) {
          Y_den <- data.frame(Y_den)
          if (ncol(Y_den) != 1) stop("'Y_den' must be vector or 1 column data.frame, matrix, data.table")
          if (nrow(Y_den) != n) stop("'Y_den' must be the same length as 'Y'")
          Y_den <- Y_den[, 1]
          if(!is.numeric(Y_den)) stop("'Y_den' must be numeric")
          if (any(is.na(Y_den))) stop("'Y_den' has missing values")
  }
 
  # period     
  if (!is.null(period)) {
      period <- data.table(period)
      if (any(duplicated(names(period)))) 
                stop("'period' are duplicate column names: ", 
                     paste(names(period)[duplicated(names(period))], collapse = ","))
      if (nrow(period) != n) stop("'period' must be the same length as 'Y'")
      if(any(is.na(period))) stop("'period' has missing values")  
  } 
  np <- sum(ncol(period))

  # ID_level2
  if (is.null(ID_level2)) ID_level2 <- 1:n
  ID_level2 <- data.table(ID_level2)
  if (any(is.na(ID_level2))) stop("'ID_level2' has missing values")
  if (ncol(ID_level2) != 1) stop("'ID_level2' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_level2) != n) stop("'ID_level2' must be the same length as 'Y'")
  if (is.null(names(ID_level2))||(names(D_level2) == "D_level2")) setnames(ID_level2, names(ID_level2), "ID")
  if (is.null(period)){ if (any(duplicated(ID_level2))) stop("'ID_level2' are duplicate values") 
                       } else {dd <- data.table(period, ID_level2)
                               if (any(duplicated(dd, by=names(dd)))) stop("'ID_level2' by period are duplicate values")
                               dd <- NULL}

  # age
  if (!is.null(age)) {
       age <- data.frame(age)
       if (nrow(age) != n) stop("'age' must be the same length as 'Y'")
       if (ncol(age) != 1) stop("'age' must be vector or 1 column data.frame, matrix, data.table")
      age <- age[, 1]
      if (!is.numeric(age)) stop("'age' must be numeric")
      if (any(is.na(age))) stop("'age' has missing values")
   }

   # pl085
   if (!is.null(pl085)) {
       pl085 <- data.frame(pl085)
       if (nrow(pl085) != n) stop("'pl085' must be the same length as 'Y'")
       if (ncol(pl085) != 1) stop("'pl085' must be vector or 1 column data.frame, matrix, data.table")
       pl085 <- pl085[, 1]
       if (!is.numeric(pl085)) stop("'pl085' must be numeric")
       if (any(is.na(pl085))) stop("'pl085' has missing values")
   }

   # month_at_work
   if (!is.null(month_at_work)) {
        month_at_work <- data.frame(month_at_work)
        if (nrow(month_at_work) != n) stop("'month_at_work' must be the same length as 'Y'")
        if (ncol(month_at_work) != 1) stop("'month_at_work' must be vector or 1 column data.frame, matrix, data.table")
        month_at_work <- month_at_work[, 1]
        if (!is.numeric(pl085)) stop("'month_at_work' must be numeric")
        if (any(is.na(pl085))) stop("'month_at_work' has missing values")
  }

  # ID_level1
  if (is.null(ID_level1)) stop("'ID_level1' must be defined")
  ID_level1 <- data.table(ID_level1)
  if (ncol(ID_level1) != 1) stop("'ID_level1' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_level1) != n) stop("'ID_level1' must be the same length as 'Y'")
  if (is.null(names(ID_level1))) setnames(ID_level1, names(ID_level1), "ID_level1")
  if (names(ID_level1) == names(ID_level2)) setnames(names(ID_level2),names(ID_level2), paste0(names(ID_level2), "_id"))
  ID_level1[, (names(ID_level1)) := lapply(.SD, as.character)]
  if (any(is.na(ID_level1))) stop("'ID_level1' has missing values")

  # w_final 
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must have the same length as 'Y'")
  if (ncol(w_final) != 1) stop("'w_final' must have vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[, 1]
  if (!is.numeric(w_final)) stop("'w_final' must be numeric")
  if (any(is.na(w_final))) stop("'w_final' has missing values") 
  
  # Y_thres
  if (!is.null(Y_thres)) {
       Y_thres <- data.frame(Y_thres)
       if (nrow(Y_thres) != n) stop("'Y_thres' must have the same length as 'Y'")
       if (ncol(Y_thres) != 1) stop("'Y_thres' must have vector or 1 column data.frame, matrix, data.table")
       Y_thres <- Y_thres[, 1]
       if (!is.numeric(Y_thres)) stop("'Y_thres' must be numeric")
       if (any(is.na(Y_thres))) stop("'Y_thres' has missing values") 
     } else Y_thres <- Y

  # wght_thres
  if (is.null(wght_thres)) wght_thres <- w_final
  wght_thres <- data.frame(wght_thres)
  if (nrow(wght_thres) != n) stop("'wght_thres' must have the same length as 'Y'")
  if (ncol(wght_thres) != 1) stop("'wght_thres' must have vector or 1 column data.frame, matrix, data.table")
  wght_thres <- wght_thres[, 1]
  if (!is.numeric(wght_thres)) stop("'wght_thres' must be a numeric vector")
 
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' must have the same length as 'Y'")
  if (ncol(H) != 1) stop("'H' must have 1 column data.frame, matrix, data.table")
  if (is.null(names(H))) stop("'H' must have column names")
  H[, (names(H)) := lapply(.SD, as.character)]
  if (any(is.na(H))) stop("'H' has missing values")

  # PSU
  PSU <- data.table(PSU)
  if (nrow(PSU) != n) stop("'PSU' must have the same length as 'Y'")
  if (ncol(PSU) != 1) stop("'PSU' must have vector or 1 column data.frame, matrix, data.table")
  PSU[, (names(PSU)) := lapply(.SD, as.character)]
  if (any(is.na(PSU))) stop("'PSU' has missing values")

  # gender
  if (!is.null(gender)) {
      gender <- data.frame(gender)
      if (nrow(gender) != n) stop("'gender' must be the same length as 'Y'")
      if (ncol(gender) != 1) stop("'gender' must be vector or 1 column data.frame, matrix, data.table")
      gender <- gender[, 1]
      if (!is.numeric(gender)) stop("'gender' must be numeric")
      if (length(unique(gender)) != 2) stop("'gender' must be exactly two values")
      if (!all.equal(unique(gender), c(1, 2))) stop("'gender' must be value 1 for male, 2 for females")
   }

  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2, " columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numeric")
      if (any(is.na(N_h))) stop("'N_h' has missing values") 
      if (is.null(names(N_h))) stop("'N_h' must have column names")
      if (all(names(H) %in% names(N_h))) {N_h[, (names(H)) := lapply(.SD, as.character), .SDcols=names(H)]
             } else stop("All strata titles of 'H' have not in 'N_h'")
      if (is.null(period)) {
             if (names(H) != names(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
             if (any(is.na(merge(unique(H), N_h, by = names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
             if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(names(pH) != names(N_h)[c(1 : (1 + np))])) stop("Strata titles for 'period' with 'H' and 'N_h' is not equal")
                nperH <- names(period)
                if (pH[, class(get(nperH))] != N_h[, class(get(nperH))]) 
                                                       stop("Period class for 'period' and 'N_h' is not equal ")
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                pH <- NULL
     }
    setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  }

  # sort
  if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
        stop("'sort' must be a vector or ordered factor") }
  if (!is.null(sort) && length(sort) != n) stop("'sort' must have the same length as 'Y'")     

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' have different row count")
    if (is.null(names(Dom))) stop("'Dom' must have column names")
    Dom[, (names(Dom)) := lapply(.SD, as.character)]
    if (any(is.na(Dom))) stop("'Dom' has missing values")
    if (any(grepl("__", names(Dom)))) stop("'Dom' is not allowed column names with '__'")
  }

  # X
  if (!is.null(X)) {
    X <- data.table(X, check.names = TRUE)
    if (!all(sapply(X, is.numeric))) stop("'X' must be numeric values")
  }

  # periodX
  if (!is.null(X)) {
     if(!is.null(periodX)) {
        periodX <- data.table(periodX)
        periodX[, (names(periodX)) := lapply(.SD, as.character)]
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
        if (any(is.na(periodX))) stop("'periodX' has missing values")
        if (any(peri != periX)) stop("'unique(period)' and 'unique(periodX)' records have different")
        if (peri[, class(get(names(peri)))]!=periX[, class(get(names(periX)))])  stop("Class for 'periodX' and class for 'period' must be equal")
      } else if (!is.null(period)) stop("'periodX' must be defined")
   } 

 # X_ID_level1
  if (!is.null(X)) {
    X_ID_level1 <- data.table(ID_level1)
    if (nrow(X) != nrow(X_ID_level1)) stop("'X' and 'ID_level1' have different row count")
    if (ncol(ID_level1) != 1) stop("'X_ID_level1' must be 1 column data.frame, matrix, data.table")
    X_ID_level1[, (names(ID_level1)) := lapply(.SD, as.character)]
    if (any(is.na(X_ID_level1))) stop("'X_ID_level1' has missing values")

    ID_level1h <- data.table(unique(ID_level1))
    X_ID_level1h <- copy(X_ID_level1)
    if (!is.null(period)) { X_ID_level1h <- data.table(periodX, ID_level1h)
                            ID_level1h <- data.table(period, ID_level1)
                            ID_level1h <- ID_level1h[, .N, by=names(ID_level1h)][, N := NULL] }
    if (nrow(X_ID_level1h[, .N, by = names(X_ID_level1h)][N > 1]) > 0) stop("'X_ID_level1' have duplicates")

    nperIDh <- names(ID_level1h)
    setkeyv(ID_level1h, nperIDh)
    setkeyv(X_ID_level1h, names(X_ID_level1h))
    if (nperIDh != names(X_ID_level1h)) stop("'X_ID_level1' and 'ID_level1' must be equal names")
    if (ID_level1h[, class(get(nperIDh))] != X_ID_householdh[, class(get(nperIDh))])  stop("Class for 'X_ID_level1' and class for 'ID_level1' must be equal ")

    if (!is.null(period)) {
        if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'periodX' with 'X_ID_level1' and 'unique(period, ID_level1)' have different row count")
        if (any(ID_level1h != X_ID_level1h)) stop("'periodX' with 'X_ID_level1' and 'unique(period, ID_level1)' records have different")
      } else {
        if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'X_ID_household' and 'unique(ID_level1)' have different row count")
        if (any(ID_level1h != X_ID_level1h)) stop("'X_ID_household' and 'unique(ID_level1)' records have different")
    }}

  # ind_gr
  if (!is.null(X)) {
     if(is.null(ind_gr)) ind_gr <- rep.int(1, nrow(X)) 
     ind_gr <- data.table(ind_gr, check.names = TRUE)
     if (nrow(ind_gr) != nrow(X)) stop("'ind_gr' length must be equal with 'X' row count")
     if (ncol(ind_gr) != 1) stop("'ind_gr' must be 1 column data.frame, matrix, data.table")
     if (any(is.na(ind_gr))) stop("'ind_gr' has missing values")
   }

  # X
  if (!is.null(X)) {
       X1 <- data.table(X, check.names = TRUE)
       nX1 <- names(X1)
       ind_gr1 <- copy(ind_gr) 
       if (!is.null(periodX)) ind_gr1 <- data.table(periodX, ind_gr1, check.names=TRUE)
       X2 <- data.table(ind_gr1, X1)
       X1 <- X2[, .N, keyby = names(ind_gr1)][[ncol(ind_gr1) + 1]]
       X2 <- X2[, lapply(.SD, function(x) sum(!is.na(x))), keyby = names(ind_gr1), .SDcols = nX1]
       X2 <- X2[, !(names(X2) %in% names(ind_gr)), with = FALSE]
       if (!all(X2 == 0 | X1 == X2)) stop("X has missing values")
       ind_gr1 <- nX1 <- X1 <- X2 <- NULL
    }

  # g
  if (!is.null(X)) {
    if (is.null(class(g)) | all(class(g)=="function")) stop("'g' must be numerical")
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
    if (is.null(class(q)) | all(class(q)=="function")) stop("'q' must be numerical")
    q <- data.frame(q)
    if (nrow(q) != nrow(X)) stop("'q' length must be equal with 'X' row count")
    if (ncol(q) != 1) stop("'q' must be 1 column data.frame, matrix, data.table")
    q <- q[, 1]
    if (!is.numeric(q)) stop("'q' must be numeric")
    if (any(is.na(q))) stop("'q' has missing values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }

  # Design weights
  if (!is.null(X)) {
             ID_level1h <- data.table(ID_level1)
             if (!is.null(period)) { ID_level1h <- data.table(period, ID_level1h)
                                     X_ID_level1 <- data.table(period, X_ID_level1)
                                   }
             ID_level1hx <- data.table(X_ID_level1, g)
             setnames(ID_level1hx, names(ID_level1hx)[c(1:(ncol(ID_level1hx)-1))], names(ID_level1h))
             ID_level1g <- merge(ID_level1h, ID_level1hx, by=names(ID_level1h), sort=FALSE)
             w_design <- w_final / ID_level1g[[ncol(ID_level1g)]]
             ID_level1g <- data.table(ID_level1g, w_design=w_design)
             ID_level1h <- ID_level1g[, .N, keyby=c(names(idh), "w_design")]
             if (nrow(X) != nrow(ID_level1h))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- idh <- NULL
      } else w_design <- w_final

  ### Calculation
  sar_nr <- respondent_count <- pop_size <- n_nonzero <- NULL
  nhs <- data.table(respondent_count=1, pop_size=w_final, 
                               n_nonzero=as.integer(abs(Y) > .Machine$double.eps))
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm=TRUE),
                                                       keyby=eval(names(nhs)[0:2-ncol(nhs)]),
                                                      .SDcols=c("respondent_count", "pop_size", "n_nonzero")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm=TRUE),
                                                     .SDcols=c("respondent_count", "pop_size", "n_nonzero")]

  estim <- c()
  aH <- names(H)
  idper <- copy(ID_level2)
  Y1sort <- Y1asort <- NULL
  aPSU <- names(PSU)
  if (!is.null(period)) idper <- data.table(idper, period)
  Y1 <- data.table(idper, ID_level1, H, PSU, w_design, w_final, check.names=TRUE)
  Y1[, Y1sort := .I]
  setkeyv(Y1, names(idper))
  value <- NULL

  if ("linarpt" %in% type) {
       varpt <- linarpt(Y = Y, id = ID_level2, weight = w_final,
                        sort = sort, Dom = Dom, period = period,
                        dataset = NULL, percentage = percentage,
                        order_quant = order_quant, var_name = "lin_arpt")
       Y1 <- merge(Y1, varpt$lin, all.x = TRUE)

       esti <- data.table("ARPT", varpt$value, NA)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varpt <- esti <- NULL
     }
  if ("linarpr" %in% type) {
       varpr <- linarpr(Y = Y, id = ID_level2, weight = w_final,
                        Y_thres = Y_thres,
                        wght_thres = wght_thres, sort = sort, 
                        Dom = Dom, period = period, dataset = NULL, 
                        percentage = percentage,
                        order_quant = order_quant,
                        var_name = "lin_arpr")

       Y1 <- merge(Y1, varpr$lin, all.x = TRUE)

       esti <- data.table("ARPR", varpr$value, NA)  
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varpr <- esti <- NULL
     }
  if (("lingpg" %in% type) & all(!is.null(gender))) {
        vgpg <- lingpg(Y = Y, gender = gender, id = ID_level2,
                       weight = w_final, sort = sort,
                       Dom = Dom, period = period, dataset = NULL, 
                       var_name = "lin_gpg")

        Y1 <- merge(Y1, vgpg$lin, all.x = TRUE)
     
        esti <- data.table("GPG", vgpg$value, NA)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgpg <- esti <- NULL
     }
  if ("linpoormed" %in% type) {
        vporm <- linpoormed(Y = Y, id = ID_level2, weight = w_final,
                            sort = sort, Dom=Dom, period = period, 
                            dataset = NULL, percentage = percentage,
                            order_quant = order_quant, var_name = "lin_poormed")
        Y1 <- merge(Y1, vporm$lin, all.x = TRUE)
        
        esti <- data.table("POORMED", vporm$value, NA)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vporm <- esti <- NULL
     }
  if ("linrmpg" %in% type) {
        vrmpg <- linrmpg(Y = Y, id = ID_level2, weight = w_final,
                         sort = sort, Dom=Dom, period = period,
                         dataset = NULL, percentage = percentage,
                         order_quant = order_quant, var_name = "lin_rmpg")
        Y1 <- merge(Y1, vrmpg$lin, all.x = TRUE)

        esti <- data.table("RMPG", vrmpg$value, NA)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu")) 
        estim <- rbind(estim, esti)
        vrmpg <- esti <- NULL
      }
  if ("linqsr" %in% type) {
        vqsr <- linqsr(Y = Y, id = ID_level2, weight = w_final, 
                       sort = sort, Dom = Dom, period = period,
                       dataset = NULL, alpha = alpha, var_name = "lin_qsr") 
        Y1 <- merge(Y1, vqsr$lin, all.x = TRUE)

        esti <- data.table("QSR", vqsr$value)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vqsr <- esti <- NULL
     }
  if ("lingini" %in% type) {
        vgini <- lingini(Y = Y, id = ID_level2, weight = w_final,
                         sort = sort, Dom=Dom, period = period,
                         dataset = NULL, var_name = "lin_gini")
        Y1 <- merge(Y1, vgini$lin, all.x = TRUE)
     
        esti <- data.table("GINI", vgini$value)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgini <- esti <- NULL
     }
  if ("lingini2" %in% type) {
       vgini2 <- lingini2(Y = Y, id = ID_level2, weight = w_final,
                          sort = sort, Dom = Dom, period = period,
                          dataset = NULL, var_name = "lin_gini2")
       Y1 <- merge(Y1, vgini2$lin, all.x = TRUE)
     
       esti <- data.table("GINI2", vgini2$value)  
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       vgini2 <- esti <- NULL
     }
  if (("linrmir" %in% type) & all(!is.null(age))) {
       vrmir <- linrmir(Y = Y, id = ID_level2, age = age,
                        weight = w_final, sort = sort, Dom = Dom, 
                        period = period, dataset = NULL,
                        order_quant = order_quant,
                        var_name = "lin_rmir") 
       Y1 <- merge(Y1, vrmir$lin, all.x = TRUE)

       esti <- data.table("RMIR", vrmir$value, NA)  
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       vrmir <- esti <- NULL
    }
  if (("linarr" %in% type) & all(!is.null(age)
                & !is.null(pl085) & !is.null(month_at_work))) {

       varr <- linarr(Y = Y, Y_den = Y_den, id = ID_level2, age = age,
                      pl085 = pl085, month_at_work = month_at_work,
                      weight = w_final, sort = sort, Dom = Dom,
                      period = period, dataset = NULL,
                      order_quant = order_quant, 
                      var_name = "lin_arr") 
       Y1 <- merge(Y1, varr$lin, all.x = TRUE)

       esti <- data.table("ARR", varr$value, NA)  
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varr <- esti <- NULL
    }

  setkey(Y1, Y1sort)
  Y1[, Y1sort := NULL]

  .SD <- lin_outp <- NULL
  if (outp_lin) lin_outp <- Y1[, c(-(3 : 5) - np), with = FALSE]

  Y2 <- Y1[, lapply(.SD, sum, na.rm = TRUE), by = c(names(Y1)[c(2 : (6 + np))]), .SDcols = names(Y1)[- (1 : (6 + np))]] 
  Y3 <- Y2[, c(-(1 : (5 + np))), with = FALSE]
  
  idper <- period <- NULL
  if (np>0) period <- Y2[, c(1 : np), with = FALSE]

  IDh <- Y2[, np + 1, with = FALSE]
  H <- Y2[, np + 2, with = FALSE]
  setnames(H, names(H), aH)

  PSU <- Y2[, np + 3, with = FALSE]
  setnames(PSU, names(PSU), aPSU)

  w_design2 <- Y2[[np + 4]]
  w_final2 <- Y2[[np + 5]]

  Y1 <- Y2 <- NULL

  # Calibration

  res_outp <- variable <- NULL
  if (!is.null(X)) {
       if (np > 0) ID_level1h <- data.table(period, ID_level1h)
       setnames(ID_level1h, names(ID_level1h), names(X_ID_level1))
       X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
       D1 <- merge(ID_level1h, X0, by = names(ID_level1h), sort = FALSE)
       ind_gr <- D1[, np + 2, with = FALSE]
       if (!is.null(period)) ind_gr <- data.table(D1[, names(periodX), with = FALSE], ind_gr)
       ind_period <- do.call("paste", c(as.list(ind_gr), sep="_"))
    
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

  var_est <- variance_est(Y = Y4, H = H, PSU = PSU, w_final = w_final2,
                          N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                          period = period, dataset = NULL, 
                          msg = "Current variance estimation")
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- var_est

    
  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y3, H = H, PSU = PSU, w_final = w_design2, 
                             N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                             period = period, dataset = NULL,
                             msg = "Variance of HT estimator under current design")                          
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT)
  var_est <- var_cur_HT <- NULL
  H <- PSU <- N_h <- NULL

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
                          S2_y_ca <- var_srs(Y = Y3[ind], w = w_final2[ind])$S2p
                          list(S2p = data.table(period_agg[i,], varsrs$S2p),
                               varsrs = data.table(period_agg[i,], varsrs$varsrs),
                               S2_y_ca = data.table(period_agg[i,], S2_y_ca))
                        })
           S2_y_HT <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_HT <- rbindlist(lapply(lin1, function(x) x[[2]]))
           S2_y_ca <- rbindlist(lapply(lin1, function(x) x[[3]]))
      }
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT, all = TRUE)
  S2_y_HT <- transpos(S2_y_HT, is.null(period), "S2_y_HT", names(period))
  all_result <- merge(all_result, S2_y_HT, all = TRUE)
  S2_y_ca <- transpos(S2_y_ca, is.null(period), "S2_y_ca", names(period))
  all_result <- merge(all_result, S2_y_ca, all = TRUE)


  # Variance of calibrated estimator under SRS
   if (is.null(period)) {
           varsres <- var_srs(Y = Y4, w = w_final2)
           S2_res <- varsres$S2p
           var_srs_ca <- varsres$varsrs
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
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
  all_result <- merge(all_result, var_srs_ca, all = TRUE)
  S2_res <- transpos(S2_res, is.null(period), "S2_res", names(period), "variable")
  all_result <- merge(all_result, S2_res, all = TRUE)
  S2_y_HT <- S2_y_ca <- S2_res <- var_srs_HT <- NULL
  var_srs_ca <- Y3 <- Y4 <- NULL

  estim <- data.table(estim)
  estim[, variable := paste0("lin_", tolower(type))]
  nDom <- names(copy(Dom))
  if (!is.null(nDom)) estim[, (paste0(nDom,"at1at")) := lapply(nDom, function(x) paste(x, get(x), sep="."))]

  Dom <- estim[, "variable", with=F]
  if (!is.null(nDom)) Dom <- estim[, c("variable", paste0(nDom,"at1at")), with = FALSE]

  estim$variable <- do.call("paste", c(as.list(Dom), sep="__"))
  estim[, variable := str_replace_all(variable, "[ ]", ".")]
  if (!is.null(nDom)) estim[, (paste0(nDom, "at1at")) := NULL]
  
  if (nrow(all_result[var_est < 0]) > 0) stop("Estimation of variance are negative!")
  
  variables <- "variable"
  if (!is.null(period)) variables <- c(variables, names(period))
  all_result <- merge(estim, all_result, all = TRUE, by = variables)
  
  all_result[, variable := NULL]
  deff_sam <- deff_est <- deff <- n_eff <- var_est2 <- NULL
  se <- rse <- cv <- absolute_margin_of_error <- NULL
  relative_margin_of_error <- CI_lower <- CI_upper <- NULL

  # Design effect of sample design
  all_result[, deff_sam := var_cur_HT / var_srs_HT]
  
  # Design effect of estimator
  all_result[, deff_est := var_est / var_cur_HT]
  
  # Overall effect of sample design and estimator
  all_result[, deff := deff_sam * deff_est]
 
  all_result[, var_est2 := var_est]
  all_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := 0]
  all_result[, se := sqrt(var_est2)]
  all_result[xor(is.na(var_est2), var_est2 < 0), se := NA]
  all_result[(value!=0) & (!is.nan(value)), rse := se / value]
  all_result[value==0 | is.nan(value), rse := NA]
  all_result[, cv := rse * 100]

  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, absolute_margin_of_error := tsad * se]
  all_result[, relative_margin_of_error := tsad * cv]
  all_result[, CI_lower := value - tsad * se]
  all_result[, CI_upper := value + tsad * se]
  
  setnames(all_result, "var_est", "var")

  if (!is.null(c(nDom, period))) { all_result <- merge(all_result, nhs,
                                                       all = TRUE, by=c(nDom, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]
                                  all_result[, n_nonzero := nhs$n_nonzero]} 

  variabl <- c("respondent_count", "n_nonzero", "pop_size", 
               "value", "value_eu", "var", "se", "rse", "cv", 
               "absolute_margin_of_error", "relative_margin_of_error",
               "CI_lower", "CI_upper")
  if (is.null(Dom))  variabl <- c(variabl, "S2_y_HT", "S2_y_ca", "S2_res") 
  variabl <- c(variabl, "var_srs_HT",  "var_cur_HT", "var_srs_ca",
               "deff_sam", "deff_est", "deff")

  type <- "type"
  if (!is.null(period)) type <- c(type, names(period))
  setkeyv(all_result, c(type, nDom))
  list(lin_out = lin_outp,
       res_out = res_outp,
       all_result = all_result[, c(type, nDom, variabl), with = FALSE])
}