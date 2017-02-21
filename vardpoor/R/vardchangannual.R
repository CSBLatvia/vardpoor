vardchangannual <- function(Y, H, PSU, w_final,
                            ID_level1, ID_level2,
                            Dom = NULL, Z = NULL,
                            country = NULL, years,
                            subperiods, dataset = NULL,
                            year1, year2, X = NULL,
                            countryX = NULL, yearsX = NULL,
                            subperiodsX = NULL, X_ID_level1 = NULL,
                            ind_gr = NULL, g = NULL, q = NULL,
                            datasetX = NULL, percentratio = 1,
                            use.estVar = FALSE, confidence = 0.95) {

  ### Checking
  outp_res <- FALSE
  if (length(percentratio) != 1 | !any(is.numeric(percentratio) | percentratio > 0)) stop("'percentratio' must be a positive numeric value")
  if (length(use.estVar) != 1 | !any(is.logical(use.estVar))) stop("'use.estVar' must be logical")
  if(length(confidence) != 1 | any(!is.numeric(confidence) |  confidence < 0 | confidence > 1)) {
          stop("'confidence' must be a numeric value in [0, 1]")  }

  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, ncols = 0, Yncol = 0,
                 Ynrow = 0, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 isnumeric = FALSE, ascharacter = TRUE)

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1", dataset = dataset,
                         ncols = 1, Yncol = 0, Ynrow = Ynrow,
                         isnumeric = FALSE, ascharacter = TRUE)

  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2", dataset = dataset,
                         ncols = 1, Yncol = 0, Ynrow = Ynrow,
                         isnumeric = FALSE, ascharacter = TRUE,
                         namesID1 = names(ID_level1))

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   ncols = 0, Yncol = 0, Ynrow = Ynrow,
                   isnumeric = FALSE, ascharacter = TRUE,
                   mustdefined = FALSE, duplicatednames = TRUE,
                   grepls = "__")

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncol = 1, Yncol = 0, Ynrow = Ynrow,
                   ascharacter = TRUE, namesID1 = names(ID_level1))

  w_final <- check_var(vars = w_final, varn = "w_final", dataset = dataset,
                       ncols = 1, Yncol = 0, Ynrow = Ynrow,
                       isnumeric = TRUE, ascharacter = FALSE, asvector = TRUE)

  country <- check_var(vars = country, varn = "country", dataset = dataset,
                       ncols = 1, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                       ascharacter = TRUE, mustdefined = FALSE, 
                       dif_name = "percoun")

  Z <- check_var(vars = Z, varn = "Z", dataset = dataset, ncols = 0,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustdefined = FALSE)
  namesZ <- names(Z)

  years <- check_var(vars = years, varn = "years", dataset = dataset, ncols = 1,
                     Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                     ascharacter = TRUE, dif_name = "percoun")
  yearm <- names(years)

  year1 <- check_var(vars = year1, varn = "year1", dataset = NULL,
                     ncols = 1, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                     ascharacter = TRUE, periods = years, periods_varn = "years")

  year2 <- check_var(vars = year2, varn = "year2", dataset = NULL, ncols = 1,
                     Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                     ascharacter = TRUE, periods = years, periods_varn = "years")

  subperiods <- check_var(vars = subperiods, varn = "subperiods", dataset = dataset,
                          ncols = 1, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                          ascharacter = TRUE, dif_name = "percoun")
  subn <- data.table(years, subperiods)
  subn <- nrow(subn[, .N, by = names(subn)]) / nrow(unique(years))
  subpm <- names(subperiods)

  if(!is.null(X)) {
    if (is.null(datasetX)) datasetX <- copy(dataset)

    X <- check_var(vars = X, varn = "X", dataset = dataset,
                   ncols = 0, Yncol = 0, Ynrow = 0, Xnrow = 0,
                   isnumeric = TRUE, grepls = "__")
    Xnrow <- nrow(X)

    g <- check_var(vars = g, varn = "g", dataset = dataset,
                   ncols = 1, Yncol = 0, Ynrow = 0,
                   Xnrow = Xnrow, isnumeric = TRUE)

    q <- check_var(vars = q, varn = "q", dataset = dataset,
                   ncols = 1, Yncol = 0, Ynrow = 0,
                   Xnrow = Xnrow, isnumeric = TRUE)

    ind_gr <- check_var(vars = ind_gr, varn = "ind_gr", dataset = dataset,
                        ncols = 1, Yncol = 0, Ynrow = 0,
                        Xnrow = Xnrow, ascharacter = TRUE)

    if(!is.null(datasetX)) {
      datasetX <- data.table(datasetX)
      if (!is.null(countryX)) {
        if (min(countryX %in% names(datasetX)) != 1) stop("'countryX' does not exist in 'datasetX'!")
        if (min(countryX %in% names(datasetX)) == 1) periodX <- datasetX[, countryX,  with = FALSE] }

      if (!is.null(yearsX)) {
        if (min(yearsX %in% names(datasetX)) != 1) stop("'yearsX' does not exist in 'datasetX'!")
        if (min(yearsX %in% names(datasetX)) == 1) yearsX <- datasetX[, yearsX,  with = FALSE] }

      if (!is.null(subperiodsX)) {
        if (min(subperiodsX %in% names(datasetX)) != 1) stop("'subperiodsX' does not exist in 'datasetX'!")
        if (min(subperiodsX %in% names(datasetX)) == 1) subperiodsX <- datasetX[, subperiodsX,  with = FALSE] }

      if (!is.null(X_ID_level1)) {
        if (min(X_ID_level1 %in% names(datasetX)) != 1) stop("'ID_level1' does not exist in 'datasetX'!")
        if (min(X_ID_level1 %in% names(datasetX)) == 1) X_ID_level1 <- datasetX[, X_ID_level1,  with = FALSE]  }

      if (identical(dataset, datasetX)) { X_ID_level1 <- ID_level1
                                          countryX <- country }
      }

    # countryX
    if(!is.null(countryX)) {
       countryX <- data.table(countryX)
       if (nrow(countryX) != nrow(X)) stop("'countryX' length must be equal with 'X' row count")
       if (ncol(countryX) != 1) stop("'countryX' has more than 1 column")
       countryX[, (names(countryX)) := lapply(.SD, as.character)]
       if (anyNA(countryX)) stop("'countryX' has missing values")

       if (names(countryX) != names(country)) stop("'countryX' must be equal with 'country' names")
       countrX <- countryX[, .N, keyby = names(countryX)][, N := NULL]
       countr <- country[, .N, keyby = names(country)][, N := NULL]
       if (any(countr != countrX)) stop("'unique(country)' and 'unique(countryX)' records have different")
     } else if (!is.null(country)) stop("'countryX' must be defined") }

    # yearsX
      if (is.null(yearsX)) stop("'yearsX' must be defined")
      yearsX <- data.table(yearsX)
      yearsX[, (names(yearsX)) := lapply(.SD, as.character)]
      if (anyNA(yearsX)) stop("'yearsX' has missing values")
      if (any(duplicated(names(yearsX))))
                  stop("'yearsX' are duplicate column names: ",
                       paste(names(yearsX)[duplicated(names(yearsX))], collapse = ","))
      if (nrow(yearsX) != nrow(X)) stop("'yearsX' length must be equal with 'X' row count")
      if (ncol(yearsX) != ncol(years)) stop("'yearsX' length must be equal with 'years' column count")
      if (names(yearsX) != names(years)) stop("'yearsX' must be equal with 'years' names")

      peri <- copy(years)
      periX <- copy(yearsX)
      if (!is.null(country)) peri <- data.table(country, peri)
      if (!is.null(countryX)) periX <- data.table(countryX, periX)
      periX <- periX[, .N, keyby = names(periX)][, N := NULL]
      peri <- peri[, .N, keyby = names(peri)][, N := NULL]
      if (any(peri != periX) & is.null(country)) stop("'unique(years)' and 'unique(yearsX)' records have different")
      if (any(peri != periX) & !is.null(country)) stop("'unique(country, years)' and 'unique(countryX, yearsX)' records have different")

    # subperiodsX
      if (is.null(subperiodsX)) stop("'subperiodsX' must be defined")
      subperiodsX <- data.table(subperiodsX)
      subperiodsX[, (names(subperiodsX)) := lapply(.SD, as.character)]
      if (anyNA(subperiodsX)) stop("'subperiodsX' has missing values")
      if (any(duplicated(names(subperiodsX))))
                 stop("'subperiodsX' are duplicate column names: ",
                          paste(names(subperiodsX)[duplicated(names(subperiodsX))], collapse = ","))
      if (nrow(subperiodsX) != nrow(X)) stop("'subperiodsX' length must be equal with 'X' row count")
      if (ncol(subperiodsX) != ncol(years)) stop("'subperiodsX' length must be equal with 'subperiods' column count")
      if (names(subperiodsX) != names(subperiods)) stop("'subperiodsX' must be equal with 'subperiods' names")

      peri <- data.table(years, subperiods)
      periX <- data.table(yearsX, subperiodsX)
      if (!is.null(country)) peri <- data.table(country, peri)
      if (!is.null(countryX)) periX <- data.table(countryX, periX)
      periX <- periX[, .N, keyby = names(periX)][, N := NULL]
      peri <- peri[, .N, keyby = names(peri)][, N := NULL]
      if (any(peri != periX) & is.null(country)) stop("'unique(years, subperiods)' and 'unique(yearsX, subperiodsX)' records have different")
      if (any(peri != periX) & !is.null(country)) stop("'unique(country, years, subperiods)' and 'unique(countryX, yearsX, subperiodsX)' records have different")
     }

    # X_ID_level1
      if (is.null(X_ID_level1)) stop("'X_ID_level1' must be defined")
      X_ID_level1 <- data.table(X_ID_level1)
      X_ID_level1[, (names(X_ID_level1)) := lapply(.SD, as.character)]
      if (anyNA(X_ID_level1)) stop("'X_ID_level1' has missing values")
      if (nrow(X) != nrow(X_ID_level1)) stop("'X' and 'X_ID_level1' have different row count")
      if (ncol(X_ID_level1) != 1) stop("'X_ID_level1' must be 1 column data.frame, matrix, data.table")
      if (any(names(X_ID_level1) != names(ID_level1))) stop("'X_ID_level1' and 'ID_level1' must be equal names")

      ID_level1h <- data.table(years, subperiods, ID_level1)
      X_ID_level1h <- data.table(yearsX, subperiodsX, X_ID_level1)
      if (!is.null(countryX)) {X_ID_level1 <- data.table(countryX, X_ID_level1)
                               ID_level1h <- data.table(country, ID_level1h)}
      ID_level1h <- ID_level1h[, .N, by = names(ID_level1h)][, N := NULL]
      if (nrow(X_ID_level1h[,.N, by = names(X_ID_level1h)][N > 1]) > 0) stop("'X_ID_level1' have duplicates")

      setkeyv(X_ID_level1h, names(X_ID_level1h))
      setkeyv(ID_level1h, names(ID_level1h))
      if (!is.null(country)) {
             if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(countryX, yearsX, subperiodsX, X_ID_level1)' and 'unique(country, years, subperiods, ID_level1)' have different row count")
             if (any(ID_level1h != X_ID_level1h)) stop("''unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' records have different")
           } else {
             if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(yearsX, subperiodsX, X_ID_level1)' and 'unique(years, subperiods, ID_level1)' have different row count")
             if (any(ID_level1h != X_ID_level1h)) stop("''unique(yearsX, subperiodsX, X_ID_level1)' and 'unique(years, subperiods, ID_level1)' records have different")  }
      ID_level1h <- X_ID_level1h <- NULL
   }
   dataset <- datasetX <- NULL



   ids <- nams <- cros_se <- num1 <- totalY <- totalZ <- NULL
   estim_1 <- estim_2 <- avar <- N <- estim <- NULL
   var_est2 <- se  <- CI_lower <- CI_upper <- NULL

   pers <- data.table(years, subperiods,
                      pers = paste0(years[[names(years)]], "__", subperiods[[names(subperiods)]]))

   if (!is.null(X)) persX <- data.table(yearsX, subperiodsX,
                                  pers = paste0(yearsX[[names(yearsX)]], "__", subperiodsX[[names(subperiodsX)]]))

   sarak <- pers[,.N, keyby = names(pers)][, N := NULL]

   namesDom <- names(Dom)
   apst <- lapply(1 : nrow(year1), function(i) {
                 atsyear <- rbindlist(list(year1[i], year2[i]))
                 atsyear <- merge(atsyear, sarak, all.x = TRUE, by = yearm, sort = FALSE)
                 yr12 <- data.table(year1 = year1[i][[1]], year2 = year2[i][[1]])
                 setnames(yr12, paste0("year", c(1, 2)), paste0(yearm, c(1, 2)))
                 atsyrm <- names(atsyear)
                 atsyear[, ids := .I]

                 nr1 <- nrow(atsyear)
                 yrs <- rbindlist(lapply(1 : (nr1 - 1), function(j) {
                           atsy1 <- atsyear[j]
                           atsy2 <- atsyear[(j + 1) : nr1]
                           setnames(atsy1, names(atsy1), paste0(names(atsy1), "_1"))
                           setnames(atsy2, names(atsy2), paste0(names(atsy2), "_2"))
                           data.table(atsy1, atsy2)
                         }))
                 yrs[, ids := .I]

                 datas <- vardchanges(Y = Y, H = H, PSU = PSU, w_final = w_final,
                                      ID_level1 = ID_level1, ID_level2 = ID_level2,
                                      Dom = Dom, Z = Z, country = country,
                                      period = pers[, "pers"], dataset = NULL,
                                      period1 = yrs[["pers_1"]], period2 = yrs[["pers_2"]],
                                      X = X, countryX = countryX, periodX = persX[, "pers"],
                                      X_ID_level1 = X_ID_level1, ind_gr = ind_gr,
                                      g = g, q = q, datasetX = NULL, annual = TRUE,
                                      linratio = !is.null(Z), percentratio = percentratio,
                                      use.estVar = use.estVar, outp_res = outp_res,
                                      confidence = confidence, change_type = "absolute")

                 crossectional_results <- datas$crossectional_results
                 crossectional_results <- merge(sarak, crossectional_results, all.y = TRUE, by = "pers")

                 grad_var <- datas$grad_var
                 grad_var <- merge(yrs, grad_var, all.y = TRUE, by = c("pers_1", "pers_2"))

                 crossectional_var_grad <- datas$crossectional_var_grad
                 crossectional_var_grad <- merge(sarak, crossectional_var_grad,
                                                 all.y = TRUE, by = c("pers"))

                 var_tau <- datas$var_tau
                 var_tau <- merge(yrs, var_tau, all.y = TRUE, by = c("pers_1", "pers_2"))
                 setkeyv(var_tau, "ids")

                 vardchanges_results <- datas$changes_results
                 vardchanges_results <- merge(yrs, vardchanges_results, all.y = TRUE, by = c("pers_1", "pers_2"))

                 rho <- datas$rho
                 rho <- merge(yrs, rho, all.y = TRUE, by = c("pers_1", "pers_2"))
                 sar <- c("country", "namesY", "namesZ", namesDom)
                 sar <- sar[sar %in% names(rho)]
                 rhoj <- rho[,.N, keyby=sar][, N := NULL]

                 apstr <- lapply(1 : nrow(rhoj), function(j){
                               rho0 <- rhoj[j]
                               rho1 <- merge(rho0, rho, by = sar)[nams == "num2"]
                               A_matrix <- diag(1, nrow(atsyear), nrow(atsyear))

                               for (k in 1 : nrow(rho1)) {
                                     at <- rho1[k == ids]
                                     A_matrix[at[["ids_1"]], at[["ids_2"]]] <- at[["rho_num1"]]
                                     A_matrix[at[["ids_2"]], at[["ids_1"]]] <- at[["rho_num1"]]
                                     if (at[["ids_2"]] > subn & at[["ids_1"]] < subn + 1) {
                                                  A_matrix[at[["ids_1"]], at[["ids_2"]]] <- - at[["rho_num1"]]
                                                  A_matrix[at[["ids_2"]], at[["ids_1"]]] <- - at[["rho_num1"]]
                                           }
                                   }
                               rho1 <- merge(rho0, crossectional_var_grad, by = sar)
                               rho1 <- merge(atsyear[, c("pers", "ids"), with = FALSE], rho1,
                                               by = "pers", sort = FALSE, allow.cartesian = TRUE)
                               rho1[, cros_se := sqrt(num1)]
                               X <- rho1[["cros_se"]]

                               annual_var <- data.table(rho0, yr12, 1 / (subn)^2 * (t(X) %*% A_matrix) %*% X)
                               setnames(annual_var, "V1", "var")

                               A_matrix <- data.table(rho0, yr12, cols = paste0("V", 1 : nrow(A_matrix)), A_matrix)
                               rho1[, ids := paste0("V", ids)]
                               setnames(rho1, "ids", "cols")
                               rho1 <- data.table(yr12, rho1)

                               list(rho1, A_matrix, annual_var)})

                 rho1 <- rbindlist(lapply(apstr, function(x) x[[1]]))
                 A_matrix <- rbindlist(lapply(apstr, function(x) x[[2]]))
                 annual_var <- rbindlist(lapply(apstr, function(x) x[[3]]))

                 sars <- c(names(country), yearm, namesDom, "namesY", "namesZ")
                 sars <- sars[sars %in% names(crossectional_var_grad)]
                 sarsb <- sars[!(sars %in% yearm)]
                 sarc <- c("totalY", "totalZ")
                 sarc <- sarc[sarc %in% names(crossectional_var_grad)]
                 ysum <- crossectional_var_grad[, lapply(.SD, mean), by = sars, .SDcols = sarc]
                 if (!is.null(ysum$namesZ)) {
                              ysum[, estim := totalY / totalZ * percentratio]
                       } else ysum[, estim := totalY]
                 ysum1 <- ysum[get(yearm) == year1[i][[1]], c(sarsb, "estim"), with = FALSE]
                 ysum2 <- ysum[get(yearm) == year2[i][[1]], c(sarsb, "estim"), with = FALSE]
                 setnames(ysum1, "estim", "estim_1")
                 setnames(ysum2, "estim", "estim_2")
                 ysum1 <- data.table(yr12, merge(ysum1, ysum2, by = sarsb))
                 ysum1[, estim := estim_2 - estim_1]
                 annual_changes <- merge(ysum1, annual_var, by = c(sarsb, names(yr12)))

                 list(crossectional_results,
                      crossectional_var_grad, grad_var,
                      rho, var_tau, vardchanges_results,
                      rho1, A_matrix, annual_changes, ysum)
   })

  crossectional_results <- rbindlist(lapply(apst, function(x) x[[1]]))
  crossectional_var_grad <- rbindlist(lapply(apst, function(x) x[[2]]))
  grad_var <- rbindlist(lapply(apst, function(x) x[[3]]))
  rho <- rbindlist(lapply(apst, function(x) x[[4]]))
  var_tau <- rbindlist(lapply(apst, function(x) x[[5]]))
  vardchanges_results <- rbindlist(lapply(apst, function(x) x[[6]]))

  X_annual <- rbindlist(lapply(apst, function(x) x[[7]]))
  A_matrix <- rbindlist(lapply(apst, function(x) x[[8]]))
  annual_changes <- rbindlist(lapply(apst, function(x) x[[9]]))
  ysum <- rbindlist(lapply(apst, function(x) x[[10]]))

  crossectional_results[, pers := NULL]
  crossectional_var_grad[, pers := NULL]
  grad_var[, (c("pers_1", "pers_2", "ids_1", "ids_2", "ids")) := NULL]
  rho[, (c("pers_1", "pers_2", "ids_1", "ids_2", "ids")) := NULL]
  var_tau[, (c("pers_1", "pers_2", "ids_1", "ids_2", "ids")) := NULL]
  vardchanges_results[, (c("pers_1", "pers_2",
                           "ids_1", "ids_2", "ids")) := NULL]

  vars <- c(paste0(yearm, c(1,2)), yearm, names(country),
            namesDom, "namesY", "namesZ", "cols", "cros_se")
  X_annual <- X_annual[, vars[vars %in% names(X_annual)], with = FALSE]

  vars <- c(paste0(yearm, c(1,2)), names(country), namesDom,
            "namesY", "namesZ", "cols", paste0("V", 1 : 8))
  A_matrix <- A_matrix[, vars[vars %in% names(A_matrix)], with = FALSE]

  vars <- c(names(country), yearm, namesDom, "namesY",
            "namesZ", "totalY", "totalZ", "estim")
  ysum <- ysum[, vars[vars %in% names(ysum)], with = FALSE]

  vars <- c(paste0(yearm, c(1, 2)), names(country), namesDom, "namesY",
                 "namesZ", paste0("estim_", c(1, 2)), "estim", "var")
  annual_changes <- annual_changes[, vars[vars %in% names(annual_changes)], with = FALSE]

  annual_changes[, var_est2 := var]
  annual_changes[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  annual_changes[, se := sqrt(var_est2)]
  annual_changes[, var_est2 := NULL]

  tsad <- qnorm(0.5 * (1 + confidence))
  annual_changes[, CI_lower := estim - tsad * se]
  annual_changes[, CI_upper := estim + tsad * se]

  significant <- NULL
  annual_changes[, significant := TRUE]
  annual_changes[CI_lower <= 0 & CI_upper >= 0, significant := FALSE]

  list(crossectional_results = crossectional_results,
       crossectional_var_grad = crossectional_var_grad,
       vardchanges_grad_var = grad_var,
       vardchanges_rho = rho,
       vardchanges_var_tau = var_tau,
       vardchanges_results = vardchanges_results,
       X_annual = X_annual, A_matrix = A_matrix,
       annual_sum = ysum,
       annual_changes = annual_changes)

}
