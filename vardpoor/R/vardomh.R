vardomh <- function(Y, H, PSU, w_final,
                    ID_level1,
                    ID_level2 = NULL,
                    Dom = NULL,
                    period = NULL,
                    N_h = NULL,
                    PSU_sort = NULL,
                    fh_zero = FALSE,
                    PSU_level = TRUE,
                    Z = NULL,
                    dataset = NULL,
                    X = NULL,
                    periodX = NULL,
                    X_ID_level1 = NULL,
                    ind_gr = NULL,
                    g = NULL,
                    q = NULL,
                    datasetX = NULL,
                    confidence = .95,
                    percentratio = 1,
                    outp_lin = FALSE,
                    outp_res = FALSE) {

  ### Checking

  fh_zero <- check_var(vars = fh_zero, varn = "fh_zero", varntype = "logical") 
  PSU_level <- check_var(vars = PSU_level, varn = "PSU_level", varntype = "logical") 
  outp_lin <- check_var(vars = outp_lin, varn = "outp_lin", varntype = "logical") 
  outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical") 
  percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger") 
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 

  if(!is.null(X)) {
    if (is.null(datasetX)) datasetX <- copy(dataset)
    if (identical(dataset, datasetX) & !is.null(dataset)) X_ID_level1 <- ID_level1 }

  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                 dif_name = "dataH_stratas")

  w_final <- check_var(vars = w_final, varn = "w_final",
                       dataset = dataset, ncols = 1,
                       Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE)

  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustbedefined = FALSE)

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   grepls = "__")

  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, duplicatednames = TRUE,
                      mustbedefined = FALSE)

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, ischaracter = TRUE)

  ID_level12 <- check_var(vars = ID_level2, varn = "ID_level2",
                          dataset = dataset, ncols = 1, Ynrow = Ynrow,
                          ischaracter = TRUE, namesID1 = names(ID_level1),
                          periods = period)

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   namesID1 = names(ID_level1))

  PSU_sort <- check_var(vars = PSU_sort, varn = "PSU_sort", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        isvector = TRUE, mustbedefined = FALSE, PSUs = PSU)

  if(!is.null(X)) {
      X <- check_var(vars = X, varn = "X", dataset = datasetX,
                     check.names = TRUE, isnumeric = TRUE,
                     grepls = "__", dif_name = c(names(Y), names(period),
                                                 "g", "q", "weight"))
      Xnrow <- nrow(X)

      ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                          dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                          ischaracter = TRUE,
                          dif_name = c(names(Y), names(period), "g", "q", "weight"))

      g <- check_var(vars = g, varn = "g", dataset = datasetX,
                     ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                     isvector = TRUE)
  
      q <- check_var(vars = q, varn = "q", dataset = datasetX,
                     ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                     isvector = TRUE)

      periodX <- check_var(vars = periodX, varn = "periodX",
                           dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                           ischaracter = TRUE, mustbedefined = !is.null(period),
                           duplicatednames = TRUE, varnout = "period",
                           varname = names(period))

      X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                               dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                               ischaracter = TRUE, varnout = "ID_level1",
                               varname = names(ID_level1), periods = period,
                               periodsX = periodX, ID_level1 = ID_level1)
   }
  N <- dataset <- datasetX <- NULL


  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (anyNA(N_h)) stop("'N_h' has missing values")
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2, " columns"))
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
                pH <- NULL
              }

      setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  }


  ### Calculation
  # Domains
  np <- sum(ncol(period))
  psusn <- as.integer(!is.null(PSU_sort))
  namesDom <- names(Dom)
  aPSU <- names(PSU)

  if (!is.null(Dom)) Y1 <- domain(Y = Y, D = Dom,
                                  dataset = NULL,
                                  checking = FALSE) else Y1 <- Y
  Y <- NULL
  n_nonzero <- copy(Y1)
  if (!is.null(period)){ n_nonzero <- data.table(period, n_nonzero)
                         n_nonzero <- n_nonzero[, lapply(.SD, function(x)
                                                         sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         keyby = names(period),
                                                         .SDcols = names(Y1)]
                  } else n_nonzero <- n_nonzero[, lapply(.SD, function(x)
                                                         sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         .SDcols = names(Y1)]

  respondent_count <- sample_size <- pop_size <- NULL
  nhs <- data.table(respondent_count = 1, pop_size = w_final)
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                       keyby = eval(names(nhs)[0 : 1 - ncol(nhs)]),
                                                      .SDcols = c("respondent_count", "pop_size")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                     .SDcols = c("respondent_count", "pop_size")]

  # Design weights
  if (!is.null(X)) {
             ID_level1h <- data.table(ID_level1)
             if (!is.null(period)) { ID_level1h <- data.table(period, ID_level1h)
                                     X_ID_level1 <- data.table(period, X_ID_level1)
                                    }
             idhx <- data.table(X_ID_level1, g = g)
             setnames(idhx, names(idhx)[c(1 : (ncol(idhx) - 1))], names(ID_level1h))
             idg <- merge(ID_level1h, idhx, by = names(ID_level1h), sort = FALSE)
             w_design <- w_final / idg[["g"]]
             idg <- data.table(idg, w_design = w_design)
             idh <- idg[, .N, keyby = c(names(ID_level1h), "w_design")]
             if (nrow(X) != nrow(idh))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- ID_level1h <- NULL
      } else w_design <- w_final

  # Ratio of two totals
  sar_nr <- Z1 <- persort <- linratio_outp <- estim <- NULL
  var_est2 <- se <- rse <- cv <- absolute_margin_of_error <- NULL
  relative_margin_of_error <- CI_lower <- S2_y_HT <- NULL
  S2_y_ca <- S2_res <- CI_upper <- variable <- variableZ <- NULL
  .SD <- deff_sam <- deff_est <- deff <- n_eff <- NULL

  aH <- names(H)
  idper <- ID_level2
  period0 <- copy(period)
  if (!is.null(period)) idper <- data.table(idper, period)

  if (!is.null(Z)) {
     if (!is.null(Dom)) Z1 <- domain(Y = Z, D = Dom,
                                     dataset = NULL,
                                     checking = FALSE) else Z1 <- Z
     if (is.null(period)) {
          Y2 <- lin.ratio(Y = Y1, Z = Z1, weight = w_final, Dom = NULL,
                          dataset = NULL, percentratio = percentratio,
                          checking = FALSE)
        } else {
          periodap <- do.call("paste", c(as.list(period), sep="_"))
          lin1 <- lapply(split(Y1[, .I], periodap), function(i)
                         data.table(sar_nr = i,
                              lin.ratio(Y = Y1[i], Z = Z1[i],
                                        weight = w_final[i],
                                        Dom = NULL, dataset = NULL,
                                        percentratio = percentratio,
                                        checking = FALSE)))
          Y2 <- rbindlist(lin1)
          setkeyv(Y2, "sar_nr")
          Y2[, sar_nr := NULL]
        }
     if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
     if (outp_lin) linratio_outp <- data.table(idper, PSU, Y2)
    } else {
            Y2 <- Y1
          }


  # Total estimation
  lin1 <- Y_est <- Z_est <- .SD <- variableDZ <- NULL

  hY <- data.table(Y1 * w_final)
  if (is.null(period)) { Y_est <- hY[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Y1)]
                } else { hY <- data.table(period0, hY)
                         Y_est <- hY[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Y1)]
                       }
  Y_est <- transpos(Y_est, is.null(period), "Y_est", names(period))
  all_result <- Y_est

  if (!is.null(Z1)) {
         YZnames <- data.table(variable = names(Y1), variableDZ = names(Z1))
         setkeyv(YZnames, "variable")
         setkeyv(all_result, "variable")
         all_result <- merge(all_result, YZnames)

         hZ <- data.table(Z1 * w_final)
         if (is.null(period)) { Z_est <- hZ[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Z1)]
                       } else { hZ <- data.table(period, hZ)
                                Z_est <- hZ[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Z1)]
                              }
         Z_est <- transpos(Z_est, is.null(period), "Z_est", names(period), "variableDZ")
         all_result <- merge(all_result, Z_est, all = TRUE, by = c(names(period), "variableDZ"))
      }

  vars <- data.table(variable = names(Y1), nr_names = 1 : ncol(Y1))
  all_result <- merge(vars, all_result, by = "variable")

  n_nonzero <- transpos(n_nonzero, is.null(period), "n_nonzero", names(period))
  all_result <- merge(all_result, n_nonzero, all = TRUE, by = c(names(period), "variable"))
  n_nonzero <- vars <- Y1 <- Z1 <- Y_est <- Z_est <- hY <- hZ <- YZnames <- NULL


  # Calibration

  YY <- data.table(idper, ID_level1, H, PSU, check.names = TRUE)
  if (!is.null(PSU_sort)) YY <- data.table(YY, PSU_sort, check.names = TRUE)
  YY <- data.table(YY, w_design, w_final, Y2, check.names = TRUE)

  YY2 <- YY[, lapply(.SD, sum, na.rm = TRUE), by = c(names(YY)[c(2 : (6 + np + psusn))]),
                                             .SDcols = names(YY)[-(1 : (6 + np + psusn))]]
  Y3 <- YY2[, c(-(1 : (5 + np + psusn))), with = FALSE]

  idper <- period <- NULL
  if (np > 0) period <- YY2[, c(1 : np), with = FALSE]

  ID_level1h <- YY2[, np + 1, with = FALSE]
  H <- YY2[, np + 2, with = FALSE]
  setnames(H, names(H), aH)

  PSU <- YY2[, np + 3, with = FALSE]
  setnames(PSU, names(PSU), aPSU)

  if (!is.null(PSU_sort)) PSU_sort <- YY2[[np + 4]]

  w_design2 <- YY2[[np + 4 + psusn]]
  w_final2 <- YY2[[np + 5 + psusn]]
  YY <- YY2 <- NULL

  # Calibration
  res_outp <- NULL
  if (!is.null(X)) {
       if (np > 0) ID_level1h <- data.table(period, ID_level1h)
       setnames(ID_level1h, names(ID_level1h), names(X_ID_level1))
       X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
       if (!is.null(periodX)) X0 <- data.table(periodX, X0)
       D1 <- merge(ID_level1h, X0, by = names(ID_level1h), sort = FALSE)

       ind_gr <- D1[, np + 2, with = FALSE]
       if (!is.null(period)) ind_gr <- data.table(D1[, names(periodX), with = FALSE], ind_gr)
       ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))

       lin1 <- lapply(split(Y3[, .I], ind_period), function(i)
                 data.table(sar_nr = i,
                            residual_est(Y = Y3[i],
                                         X = D1[i, (np + 5) : ncol(D1), with = FALSE],
                                         weight = w_design2[i],
                                         q = D1[i][["q"]],
                                         checking = FALSE)))
       Y4 <- rbindlist(lin1)
       setkeyv(Y4, "sar_nr")
       Y4[, sar_nr := NULL]
       if (outp_res) res_outp <- data.table(ID_level1h, PSU, w_final2, Y4)
   } else Y4 <- Y3
  X0 <- D1 <- X_ID_level1 <- ID_level1h <- ind_gr <- lin1 <- X <- g <- q <- NULL

  var_est <- variance_est(Y = Y4, H = H, PSU = PSU,
                          w_final = w_final2,
                          N_h = N_h, fh_zero = fh_zero,
                          PSU_level = PSU_level,
                          PSU_sort = PSU_sort,
                          period = period, dataset = NULL,
                          msg = "Current variance estimation",
                          checking = FALSE)
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- merge(all_result, var_est, all = TRUE, by = c(names(period), "variable"))

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y3, H = H, PSU = PSU,
                             w_final = w_design2,
                             N_h = N_h, fh_zero = fh_zero,
                             PSU_level = PSU_level,
                             PSU_sort = PSU_sort,
                             period = period, dataset = NULL,
                             msg = "Variance of HT estimator under current design",
                             checking = FALSE)
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT, all = TRUE, by = c(names(period), "variable"))
  n_nonzero <- var_est <- var_cur_HT <- NULL
  H <- PSU <- PSU_sort <- N_h <- NULL

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
                          varsca <- var_srs(Y = Y3[ind], w = w_final2[ind])
                          list(S2p = data.table(period_agg[i,], varsrs$S2p),
                               varsrs = data.table(period_agg[i,], varsrs$varsrs),
                               S2ca = data.table(period_agg[i,], varsca$S2p))
                        })
           S2_y_HT <- rbindlist(lapply(lin1, function(x) x[[1]]))
           var_srs_HT <- rbindlist(lapply(lin1, function(x) x[[2]]))
           S2_y_ca <- rbindlist(lapply(lin1, function(x) x[[3]]))
      }
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_HT <- transpos(S2_y_HT, is.null(period), "S2_y_HT", names(period))
  all_result <- merge(all_result, S2_y_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_ca <- transpos(S2_y_ca, is.null(period), "S2_y_ca", names(period))
  all_result <- merge(all_result, S2_y_ca, all = TRUE, by = c(names(period), "variable"))
  Y3 <- w_design2 <- var_srs_HT <- S2_y_HT <- S2_y_ca <- NULL

  # Variance of calibrated estimator under SRS
  if (is.null(period)) {
           varsres <- var_srs(Y4, w = w_final2)
           S2_res <- varsres$S2p
           var_srs_ca <- varsres$varsrs
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1 : nrow(period_agg), function(i) {
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
  all_result <- merge(all_result, var_srs_ca, all = TRUE, by = c(names(period), "variable"))
  S2_res <- transpos(S2_res, is.null(period), "S2_res", names(period), "variable")
  all_result <- merge(all_result, S2_res, all = TRUE, by = c(names(period), "variable"))
  Y4 <- w_final2 <- var_srs_ca <- S2_res <- NULL


  all_result[, estim := Y_est]
  if (!is.null(all_result$Z_est)) all_result[, estim := Y_est / Z_est * percentratio]

  if (nrow(all_result[var_est < 0]) > 0) stop("Estimation of variance are negative!")

  # Design effect of sample design
  all_result[, deff_sam := var_cur_HT / var_srs_HT]

  # Design effect of estimator
  all_result[, deff_est := var_est / var_cur_HT]

  # Overall effect of sample design and estimator
  all_result[, deff := deff_sam * deff_est]

  all_result[, var_est2 := var_est]
  all_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  all_result[, se := sqrt(var_est2)]
  all_result[(estim != 0) & !is.nan(estim), rse := se / estim]
  all_result[estim == 0 | is.nan(estim), rse := NA]
  all_result[, cv := rse * 100]


  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, absolute_margin_of_error := tsad * se]
  all_result[, relative_margin_of_error := tsad * cv]
  all_result[, CI_lower := estim - tsad * se]
  all_result[, CI_upper := estim + tsad * se]

  variableD <- NULL
  setnames(all_result, c("variable", "var_est"), c("variableD", "var"))
  if (!is.null(all_result$Z_est)) {
                       nosrZ <- data.table(all_result[, "variableDZ"], all_result[, tstrsplit(variableDZ, "__")][, 1])
                       nosrZ <- nosrZ[!duplicated(nosrZ)]
                       setnames(nosrZ, "V1", "variableZ")
                       all_result <- merge(all_result, nosrZ, by = "variableDZ")
                       nosrZ <- NULL
                    }

  nosr <- data.table(all_result[, "variableD"], all_result[, tstrsplit(variableD, "__")])
  nosr <- nosr[!duplicated(nosr)]
  nosr <- nosr[, lapply(nosr, as.character)]
  setnames(nosr, names(nosr)[2], "variable")

  namesDom1 <- namesDom
  if (!is.null(Dom)) {
       setnames(nosr, names(nosr)[3 : ncol(nosr)], paste0(namesDom, "_new"))
       nhs[, (paste0(namesDom, "_new")) := lapply(namesDom, function(x) make.names(paste0(x,".", get(x))))]
       namesDom1 <- paste0(namesDom, "_new")
    }

  all_result <- merge(nosr, all_result, by="variableD")
  namesDom <- nosr <- NULL

  if (!is.null(all_result$Z_est)) {
       all_result[, variable := paste("R", get("variable"), get("variableZ"), sep="__")] }

  if (!is.null(c(Dom, period))) { all_result <- merge(all_result, nhs,
                                                      all = TRUE, by = c(namesDom1, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]}

  variab <- c("respondent_count", "n_nonzero", "pop_size")
  if (!is.null(all_result$Z_est)) variab <- c(variab, "Y_est", "Z_est")
  variab <- c(variab, "estim", "var", "se", "rse", "cv",
              "absolute_margin_of_error", "relative_margin_of_error",
              "CI_lower", "CI_upper")
  if (is.null(Dom))  variab <- c(variab, "S2_y_HT", "S2_y_ca", "S2_res")
  variab <- c(variab, "var_srs_HT",  "var_cur_HT", "var_srs_ca",
              "deff_sam", "deff_est", "deff")

  setkeyv(all_result, c("nr_names", names(Dom), names(period)))
  all_result <- all_result[, c("variable", names(Dom), names(period), variab), with = FALSE]

  list(lin_out = linratio_outp,
       res_out = res_outp,
       all_result = all_result)
}

