
vardcrospoor <- function(Y, age = NULL, pl085 = NULL,
                         month_at_work = NULL, Y_den = NULL,
                         Y_thres = NULL,  wght_thres = NULL, 
                         H, PSU, w_final, ID_level1, ID_level2,
                         Dom = NULL, country = NULL,
                         period, sort = NULL, gender = NULL,
                         dataset = NULL, X = NULL, 
                         countryX = NULL, periodX = NULL, 
                         X_ID_level1 = NULL, ind_gr = NULL,
                         g = NULL, q = NULL, datasetX = NULL,
                         percentage = 60, order_quant = 50,
                         alpha = 20, use.estVar = FALSE,
                         withperiod = TRUE, netchanges = TRUE,
                         confidence = .95, outp_lin = FALSE,
                         outp_res = FALSE, type = "linrmpg") {
  ### Checking

  all_choices <- c("linarpr", "linarpt", "lingpg",
                   "linpoormed", "linrmpg", "lingini",
                   "lingini2", "linqsr", "linrmir", "linarr")
  choices <- c("all_choices", all_choices)
  type <- tolower(type)

  type <- match.arg(type, choices, length(type)>1) 
  if (any(type == "all_choices"))  type <- all_choices

  # check 'p'
  p <- percentage
  if(length(p) != 1 | any(!is.numeric(p) | p < 0 | p > 100)) {
         stop("'percentage' must be a numeric value in [0, 100]")
     } else p <- percentage[1]

  # check 'order_quant'

  oq <- order_quant
   if(length(oq) != 1 | any(!is.numeric(oq) | oq < 0 | oq > 100)) {
          stop("'order_quant' must be a numeric value in [0, 100]")
      } else order_quant <- order_quant[1]

  if(length(alpha) != 1 | any(!is.numeric(alpha) | alpha < 0 | alpha > 100)) {
         stop("'alpha' must be a numeric value in [0, 100]")  }
 
  if (length(netchanges) != 1 | !any(is.logical(netchanges))) stop("'netchanges' must be logical")
  if (length(withperiod) != 1 | !any(is.logical(withperiod))) stop("'withperiod' must be logical")
  if (length(use.estVar) != 1 | !any(is.logical(use.estVar))) stop("'use.estVar' must be logical")

  if(length(confidence) != 1 | any(!is.numeric(confidence) | confidence < 0 | confidence > 1)) {
         stop("'confidence' must be a numeric value in [0, 1]")  }


  if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(Y %in% names(dataset)) != 1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset)) == 1) Y <- dataset[, Y, with = FALSE] 

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

      if(!is.null(H)) {
          if (min(H %in% names(dataset)) != 1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset)) == 1) H <- dataset[, H, with = FALSE]  }

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
  
     if(!is.null(country)) {
          if (min(country %in% names(dataset)) != 1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset)) == 1) country <- dataset[, country, with = FALSE]  }

     if(!is.null(period)) {
          if (min(period %in% names(dataset)) != 1) stop("'period' does not exist in 'dataset'!")
          if (min(period %in% names(dataset)) == 1) period <- dataset[, period, with = FALSE] }

      if(!is.null(gender)) {
          if (min(gender %in% names(dataset)) != 1) stop("'gender' does not exist in 'dataset'!")
          if (min(gender %in% names(dataset)) == 1) gender <- dataset[, gender, with = FALSE] }

      if(!is.null(sort)) {
          if (min(sort %in% names(dataset)) != 1) stop("'sort' does not exist in 'dataset'!")
          if (min(sort %in% names(dataset)) == 1) sort <- dataset[, sort, with = FALSE] }
     
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

  # Y
  Y <- data.frame(Y)
  n <- nrow(Y)
  if (ncol(Y) != 1) stop("'Y' must be a vector or 1 column data.frame, matrix, data.table")
  Y <- Y[, 1]
  if (!is.numeric(Y)) stop("'Y' must be numeric")
  if (anyNA(Y)) stop("'Y' has missing values")

  if (!is.null(Y_den)) {
          Y_den <- data.frame(Y_den)
          if (ncol(Y_den) != 1) stop("'Y_den' must be a vector or 1 column data.frame, matrix, data.table")
          if (nrow(Y_den) != n) stop("'Y_den' must be the same length as 'Y'")
          Y_den <- Y_den[, 1]
          if(!is.numeric(Y_den)) stop("'Y_den' must be numeric")
          if (anyNA(Y_den)) stop("'Y_den' has missing values")
    } else if (any(c("linrmir", "linarr") %in% type)) stop("'age' must be numeric")

  # age
  if (!is.null(age)) {
       age <- data.frame(age)
       if (nrow(age) != n) stop("'age' must be the same length as 'Y'")
       if (ncol(age) != 1) stop("'age' must be a vector or 1 column data.frame, matrix, data.table")
       age <- age[, 1]
       if (!is.numeric(age)) stop("'age' must be numeric")
       if (anyNA(age)) stop("'age' has missing values")
    } else if (any(c("linrmir", "linarr") %in% type)) stop("'age' must be numeric")

   # pl085
   if (!is.null(pl085)) {
       pl085 <- data.frame(pl085)
       if (anyNA(pl085)) stop("'pl085' has missing values")
       if (nrow(pl085) != n) stop("'pl085' must be the same length as 'Y'")
       if (ncol(pl085) != 1) stop("'pl085' must be a vector or 1 column data.frame, matrix, data.table")
       pl085 <- pl085[, 1]
       if (!is.numeric(pl085)) stop("'pl085' must be numeric")
    } else if (any(type == "linarr")) stop("'pl085' must be numeric")

   # month_at_work
   if (!is.null(month_at_work)) {
        month_at_work <- data.frame(month_at_work)
        if (anyNA(month_at_work)) stop("'month_at_work' has missing values")
        if (nrow(month_at_work) != n) stop("'month_at_work' must be the same length as 'Y'")
        if (ncol(month_at_work) != 1) stop("'month_at_work' must be a vector or 1 column data.frame, matrix, data.table")
        month_at_work <- month_at_work[, 1]
        if (!is.numeric(month_at_work)) stop("'month_at_work' must be numeric")
     } else if (any(type == "linarr")) stop("'month_at_work' must be numeric")

  # Y_thres
  if (!is.null(Y_thres)) {
       Y_thres <- data.frame(Y_thres)
       if (anyNA(Y_thres)) stop("'Y_thres' has missing values") 
       if (nrow(Y_thres) != n) stop("'Y_thres' must have the same length as 'Y'")
       if (ncol(Y_thres) != 1) stop("'Y_thres' must have a vector or 1 column data.frame, matrix, data.table")
       Y_thres <- Y_thres[, 1]
       if (!is.numeric(Y_thres)) stop("'Y_thres' must be numeric")
     } else Y_thres <- Y

  # wght_thres
  if (is.null(wght_thres)) wght_thres <- w_final
  wght_thres <- data.frame(wght_thres)
  if (anyNA(wght_thres)) stop("'w_final' has missing values") 
  if (nrow(wght_thres) != n) stop("'wght_thres' must have the same length as 'Y'")
  if (ncol(wght_thres) != 1) stop("'wght_thres' must be a vector or 1 column data.frame, matrix, data.table")
  wght_thres <- wght_thres[, 1]
  if (!is.numeric(wght_thres)) stop("'wght_thres' must be a numeric vector")
 
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
   H[, (names(H)) := lapply(.SD, as.character)]
  if (anyNA(H)) stop("'H' has missing values")

  # PSU
  PSU <- data.table(PSU)
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  PSU[, (names(PSU)) := lapply(.SD, as.character)]
  if (anyNA(PSU)) stop("'PSU' has missing values")
  
  # gender
  if (!is.null(gender)) {
      gender <- data.frame(gender)
      if (nrow(gender) != n) stop("'gender' must be the same length as 'Y'")
      if (ncol(gender) != 1) stop("'gender' must be a vector or 1 column data.frame, matrix, data.table")
      gender <- gender[, 1]
      if (!is.numeric(gender)) stop("'gender' must be numeric")
      if (length(unique(gender)) != 2) stop("'gender' must be exactly two values")
      if (!all(gender %in% 1:2)) stop("'gender' must be value 1 for male, 2 for females")
  } else if (any(type == "lingpg")) stop("'gender' must be numeric")

  # sort
  if (!is.null(sort)) {
        sort <- data.frame(sort)
        if (anyNA(sort)) stop("'sort' has missing values") 
        if (length(sort) != n) stop("'sort' must have the same length as 'Y'")
        if (ncol(sort) != 1) stop("'sort' must be a vector or 1 column data.frame, matrix, data.table")
        sort <- sort[, 1]
   }

  # w_final 
  w_final <- data.frame(w_final)
  if (anyNA(w_final)) stop("'w_final' has missing values") 
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be a vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[, 1]
  if (!is.numeric(w_final)) stop("'w_final' must be numeric")
  
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
    Dom[, (namesDom):= lapply(.SD, as.character)]
    if (anyNA(Dom)) stop("'Dom' has missing values")
    if (any(grepl("__", namesDom))) stop("'Dom' is not allowed column names with '__'")
  }

  # X
  if (!is.null(X)) {
      X <- data.table(X, check.names = TRUE)
      if (!all(sapply(X, is.numeric))) stop("'X' must be numeric values")
      if (anyNA(X)) stop("'X' has missing values")
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
        if (names(periodX) != names(period)) stop("'periodX' must be equal with 'period' names")

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

  # g
  if (!is.null(X)) {
    if (is.null(class(g)) | all(class(g) == "function")) stop("'g' must be numeric")
    g <- data.frame(g)
    if (nrow(g) != nrow(X)) stop("'g' length must be equal with 'X' row count")
    if (ncol(g) != 1) stop("'g' must be 1 column data.frame, matrix, data.table")
    g <- g[, 1]
    if (!is.numeric(g)) stop("'g' must be numeric")
    if (anyNA(g)) stop("'g' has missing values")
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
    if (anyNA(q)) stop("'q' has missing values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }
  
    
  # Calculation
  Dom1 <- n_h <- stratasf <- name1 <- nhcor <- n_h <- var <- NULL
  num <- count_respondents <- value <- estim <- pop_size <- NULL
  period_country <- N <- se <- rse <- cv <- namesY <- H_sk <- NULL 

  estim <- c()
  if (!is.null(country)) { countryper <- copy(country)
                        } else countryper <- data.table(percoun = rep("1", length(Y)))
  if (!is.null(period)) countryper <- data.table(period, countryper)
  idper <- data.table(ID_level1, ID_level2, countryper)
  countryperid2 <- c(names(countryper), names(ID_level2))
  
  size <- copy(countryper)
  if (!is.null(namesDom)) size <- data.table(size, Dom)
  names_size <- names(size)
  size <- data.table(size, sk = 1, w_final)
  size <- size[, .(count_respondents = .N,
                  pop_size = sum(w_final)), keyby = names_size]
 
  Y1 <- data.table(idper)
  Y1$period_country <- do.call("paste", c(as.list(Y1[, names(countryper), with = FALSE]), sep = "_"))
  Y1 <- data.table(Y1, H, PSU, w_final, check.names = TRUE)
  namesY1 <- names(Y1)

  if ("linarpt" %in% type) {
        varpt <- linarpt(Y = Y, id = ID_level2,
                         weight = w_final, sort = sort, 
                         Dom = Dom, period = countryper,
                         dataset = NULL, percentage = percentage,
                         order_quant = order_quant,
                         var_name = "lin_arpt")
        Y1 <- merge(Y1, varpt$lin, all.x = TRUE, by = countryperid2)
        esti <- data.table("ARPT", varpt$value, NA)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        varpt <- esti <- NULL
     }
  if ("linarpr" %in% type) {
        varpr <- linarpr(Y = Y, id = ID_level2,
                         weight = w_final, Y_thres = Y_thres,
                         wght_thres = wght_thres, sort = sort, 
                         Dom = Dom, period = countryper,
                         dataset = NULL, percentage = percentage,
                         order_quant = order_quant, var_name = "lin_arpr")
        Y1 <- merge(Y1, varpr$lin, all.x = TRUE, by = countryperid2)
        esti <- data.table("ARPR", varpr$value, NA)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        varpr <- esti <- NULL
      }
   if (("lingpg" %in% type) & (all(!is.null(gender)))) {
         vgpg <- lingpg(Y = Y, gender = gender, id = ID_level2,
                        weight = w_final, sort = sort, Dom = Dom,
                        period = countryper, dataset = NULL,
                        var_name = "lin_gpg")
         Y1 <- merge(Y1, vgpg$lin, all.x = TRUE, by = countryperid2)
         esti <- data.table("GPG", vgpg$value, NA)  
         setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                    c("type", "value", "value_eu"))
         estim <- rbind(estim, esti)
         vgpg <- esti <- NULL
      }
   if ("linpoormed" %in% type) {
         vporm <- linpoormed(Y = Y, id = ID_level2, weight = w_final,
                             sort = sort, Dom = Dom, period = countryper, 
                             dataset = NULL, percentage = percentage,
                             order_quant = order_quant, var_name = "lin_poormed")
         Y1 <- merge(Y1, vporm$lin, all.x = TRUE, by = countryperid2)
         esti <- data.table("POORMED", vporm$value, NA)  
         setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                    c("type", "value", "value_eu"))
         estim <- rbind(estim, esti)
         vporm <- esti <- NULL
      }
   if ("linrmpg" %in% type) {
         vrmpg <- linrmpg(Y = Y, id = ID_level2, weight = w_final,
                          sort = sort, Dom = Dom, period = countryper,
                          dataset = NULL, percentage = percentage,
                          order_quant = order_quant, var_name = "lin_rmpg")
         Y1 <- merge(Y1, vrmpg$lin, all.x = TRUE, by = countryperid2)
         esti <- data.table("RMPG", vrmpg$value, NA)  
         setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                    c("type", "value", "value_eu")) 
         estim <- rbind(estim, esti)
         vrmpg <- esti <- NULL
      }
   if ("linqsr" %in% type) {
        vqsr <- linqsr(Y = Y, id = ID_level2, weight = w_final, 
                       sort = sort, Dom = Dom, period = countryper,
                       dataset = NULL, alpha = alpha, var_name = "lin_qsr") 
        Y1 <- merge(Y1, vqsr$lin, all.x = TRUE, by = countryperid2)
        esti <- data.table("QSR", vqsr$value)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vqsr <- esti <- NULL
      }
   if ("lingini" %in% type) {
        vgini <- lingini(Y = Y, id = ID_level2, weight = w_final,
                         sort = sort, Dom = Dom, period = countryper,
                         dataset = NULL, var_name = "lin_gini")
        Y1 <- merge(Y1, vgini$lin, all.x = TRUE, by = countryperid2)
        esti <- data.table("GINI", vgini$value)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgini <- vginia <- esti <- NULL
      }
   if ("lingini2" %in% type) {
        vgini2 <- lingini2(Y = Y, id = ID_level2, weight=w_final,
                           sort = sort, Dom = Dom, period = countryper,
                           dataset = NULL, var_name = "lin_gini2")
        Y1 <- merge(Y1, vgini2$lin, all.x = TRUE, by = countryperid2)
        esti <- data.table("GINI2", vgini2$value)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgini2 <- esti <- NULL
      }
   if (("linrmir" %in% type) & all(!is.null(age))) {
        vrmir <- linrmir(Y = Y, id = ID_level2, age = age,
                         weight = w_final, sort = sort, Dom = Dom,
                         period = countryper, dataset = NULL,
                         order_quant = order_quant, var_name = "lin_rmir") 
        Y1 <- merge(Y1, vrmir$lin, all.x = TRUE, by = countryperid2)
 
        esti <- data.table("RMIR", vrmir$value, NA)  
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                   c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vrmir <-  esti <- NULL
      } 
   if (("linarr" %in% type) & all(!is.null(age)
                & !is.null(pl085) & !is.null(month_at_work))) {

       varr <- linarr(Y = Y, Y_den = Y_den, id = ID_level2, age = age,
                      pl085 = pl085, month_at_work = month_at_work, weight = w_final, 
                      sort = sort, Dom = Dom, period = countryper, dataset = NULL,
                      order_quant = order_quant, var_name = "lin_arr") 

       Y1 <- merge(Y1, varr$lin, all.x = TRUE, by = countryperid2)

       esti <- data.table("ARR", varr$value, NA)  
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varr <- esti <- NULL
     }

   lin_out <- copy(Y1)
   if (!outp_lin) lin_out <- NULL
   
   setnames(estim, "value", "estim")
   estim$period_country <- do.call("paste", c(as.list(estim[, names(countryper), with = FALSE]), sep = "_"))
   nams <- names(countryper)
   if (!is.null(namesDom)) nams <- c(nams, namesDom)
   estim <- merge(estim, size, all = TRUE, by = nams)

   namesY2 <- names(Y1)[!(names(Y1) %in% namesY1)]
   namesY2w <- paste0(namesY2, "w")
   
   
   # Calibration

   w_design <- res_outp <- NULL
   names_id <- names(ID_level1)
   names_H <- names(H)
   names_PSU <- names(PSU)

   namesperc <- c("period_country", names(countryper))
   namesDT1k <- c(namesperc, names_H, names_PSU)
   DTc <- Y1[, lapply(.SD, sum, na.rm = TRUE), keyby = c(namesDT1k, names(ID_level1)), .SDcols = namesY2]

   if (!is.null(X)) {
        X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
        DT1 <- merge(DTc, X0, by = names(ID_level1h))
        DT1[, w_design := w_final / g ]

        ind_gr <- DT1[, c(namesperc, names(ind_gr)), with = FALSE]
        ind_period <- do.call("paste", c(as.list(ind_gr), sep="_"))
     
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

   DTc[, (namesY2w) := .SD[, namesY2, with = FALSE] * get("w_final")]

   size <- ID_level1 <- ID_level2 <- Dom <- country <- NULL
   country <- H <- PSU <- nh <- nh_cor <- NULL
  
   #--------------------------------------------------------*
   # AGGREGATION AT PSU LEVEL ("ULTIMATE CLUSTER" APPROACH) |
   #--------------------------------------------------------*

   DTY2 <- DTc[, lapply(.SD, sum, na.rm = TRUE), keyby = namesDT1k, .SDcols = namesY2w]
   setnames(DTY2, namesY2w, namesY2)
   DT1 <- copy(DTY2)
   DT1[, period_country := NULL]
   if (!netchanges) DT1 <- NULL

   # NUMBER OF PSUs PER STRATUM
   setkeyv(DTY2, c(namesperc, names_H))
   DTY2[, nh := .N, by = c(namesperc, names_H)]

   #--------------------------------------------------------------------------*
   # MULTIVARIATE REGRESSION APPROACH USING STRATUM DUMMIES AS REGRESSORS AND |
   # STANDARD ERROR ESTIMATION 						      |
   #--------------------------------------------------------------------------*

   DTY2H <- DTY2[[names_H]]
   DTY2H <- factor(DTY2H)
   if (length(levels(DTY2H)) == 1) { DTY2[, stratasf := 1]
                                   DTY2H <- "stratasf"
                          } else { DTY2H <- data.table(model.matrix( ~ DTY2H - 1))
                                   DTY2 <- cbind(DTY2, DTY2H)
                                   DTY2H <- names(DTY2H) }
   namesY2m <-  make.names(namesY2)
   setnames(DTY2, namesY2, namesY2m)

   fits <- lapply(1 : length(namesY2), function(i) {
              fitss <- lapply(split(DTY2, DTY2$period_country), function(DTY2c) {
                           y <- namesY2m[i]
                           funkc <- as.formula(paste("cbind(", trim(toString(y)), ") ~ ",
                                          paste(c(- 1, DTY2H), collapse = "+")))
                   	   res1 <- lm(funkc, data = DTY2c)
                            
           	           if (use.estVar == TRUE) {res1 <- data.table(crossprod(res1$res))
                                  } else res1 <- data.table(res1$res)
                           setnames(res1, names(res1)[1], "num") 
                           res1[, namesY := y]
                           
                           if (use.estVar == TRUE) {
                                 setnames(res1, "num", "var") 
                                 res1 <- data.table(res1[1], DTY2c[1])
                             } else {
                                 res1 <- data.table(res1, DTY2c)
                                 res1[, nhcor := ifelse(nh > 1, nh / (nh - 1), 1)]
                                 res1[, var := nhcor * num * num]
                               }
                           fits <- res1[, lapply(.SD, sum), 
                                          keyby = c(namesperc, "namesY"),
                                          .SDcols = "var"]
                           return(fits)
                      })
             return(rbindlist(fitss))
       })
    res <- rbindlist(fits)
   
    estim[, namesY:= paste0("lin_", tolower(type))]
    if (!is.null(namesDom)) {
         Dom1 <- estim[, lapply(namesDom, function(x) make.names(paste0(x, ".", get(x))))]
         Dom1 <- Dom1[, Dom := Reduce(function(x, y) paste(x, y, sep = "__"), .SD)]    
         estim <- data.table(estim, Dom1 = Dom1[, Dom])
         estim[, namesY := paste0(namesY, "__", Dom1)]
      } 
  
    res <- merge(estim, res, all = TRUE, 
                  by = names(res)[!(names(res) %in% "var")])

    Dom1 <- estim <- DT3H <- NULL
    if (is.null(res$Dom1)) res[, Dom1 := "1"]
    res[, (c("namesY", "Dom1", "period_country")) := NULL]

    res[, se := sqrt(var)]
    res[, rse := se / estim]
    res[, cv := rse * 100]
   
    res <- res[, c(names(countryper), namesDom, "type", "count_respondents",
                   "pop_size", "estim", "se", "var", "rse", "cv"), with = FALSE]

    list(lin_out = lin_out, res_out = res_outp, data_net_changes = DT1, results = res)
 }   
