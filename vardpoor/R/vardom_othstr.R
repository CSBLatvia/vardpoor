vardom_othstr <- function(Y, H, H2, PSU, w_final,
                   id = NULL,  
                   Dom = NULL,
                   period = NULL,
                   N_h = NULL,
                   N_h2 = NULL,
                   Z = NULL,
                   X = NULL,
                   g = NULL,
                   q = NULL,
                   dataset = NULL, 
                   confidence = .95, 
                   percentratio = 1,
                   outp_lin = FALSE,
                   outp_res = FALSE) {
 
  ### Checking

  outp_lin <- check_var(vars = outp_lin, varn = "outp_lin", varntype = "logical") 
  outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical") 
  percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger") 
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 

  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, isnumeric = FALSE,
                 ischaracter = TRUE)

  H2 <- check_var(vars = H2, varn = "H2", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, isnumeric = FALSE,
                 ischaracter = TRUE, dif_name = names(H))

  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, mustbedefined = FALSE,
                      duplicatednames = TRUE)
  np <- sum(ncol(period))

  id <- check_var(vars = id, varn = "id", dataset = dataset,
                  ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                  periods = period)

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   namesID1 = names(id))

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   grepls = "__")
  namesDom <- names(Dom)

  w_final <- check_var(vars = w_final, varn = "w_final",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       isnumeric = TRUE, isvector = TRUE)

  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustbedefined = FALSE)

  if (!is.null(X)) {
         X <- check_var(vars = X, varn = "X", dataset = dataset,
                        check.names = TRUE, Ynrow = Ynrow,
                        isnumeric = TRUE,
                        dif_name = c(names(period), "g", "q"))
         Xnrow <- nrow(X)

         ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                             dataset = dataset, ncols = 1, Xnrow = Xnrow,
                             ischaracter = TRUE, dif_name = c(names(period), "g", "q"))

         g <- check_var(vars = g, varn = "g", dataset = dataset,
                        ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                        isvector = TRUE)

         q <- check_var(vars = q, varn = "q", dataset = dataset,
                        ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                        isvector = TRUE)
    }
  dataset <- NULL

  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (anyNA(N_h)) stop("'N_h' has missing values")
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2," columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numeric")
      nams <- c(names(period), names(H))
      if (all(nams %in% names(N_h))) {N_h[, (nams) := lapply(.SD, as.character), .SDcols = nams]
             } else stop(paste0("All strata titles of 'H'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h'"))

      if (is.null(period)) {
             if (names(H) != names(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
             if (any(is.na(merge(unique(H), N_h, by=names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
             if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(names(pH) != names(N_h)[c(1:(1 + np))])) stop("Strata titles for 'period' with 'H' and 'N_h' is not equal")
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                pH <- NULL
     }
    setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  } 

  # N_h2
  if (!is.null(N_h2)) {
      N_h2 <- data.table(N_h2)
      if (anyNA(N_h2)) stop("'N_h2' has missing values") 
      if (ncol(N_h2) != np + 2) stop(paste0("'N_h2' should be ", np + 2, " columns"))
      if (!is.numeric(N_h2[[ncol(N_h2)]])) stop("The last column of 'N_h2' should be numeric")

      nams2 <- c(names(period), names(H2))
      if (all(nams2 %in% names(N_h2))) {N_h2[, (nams2) := lapply(.SD, as.character), .SDcols = nams2]
             } else stop(paste0("All strata titles of 'H2'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h2'"))   
      if (is.null(period)) {
             if (names(H2) != names(N_h2)[1]) stop("Strata titles for 'H2' and 'N_h2' is not equal")
             if (any(is.na(merge(unique(H2), N_h2, by = names(H2), all.x = TRUE)))) stop("'N_h2' is not defined for all strata")
       } else { pH2 <- data.table(period, H2)
                if (any(names(pH2) != names(N_h2)[c(1 : (1 + np))])) stop("Strata titles for 'period' with 'H2' and 'N_h2' is not equal")
                if (any(is.na(merge(unique(pH2), N_h2, by = names(pH2), all.x = TRUE)))) stop("'N_h2' is not defined for all strata and periods")
                } 
    setkeyv(N_h2, names(N_h2)[c(1 : (1 + np))])
  } else stop ("N_h2 is not defined!")


  ### Calculation
      
  # Domains
  if (!is.null(Dom)) Y1 <- domain(Y = Y, D = Dom,
                                  dataset = NULL,
                                  checking = FALSE) else Y1 <- Y

  n_nonzero <- copy(Y1)
  if (!is.null(period)){ n_nonzero <- data.table(period, n_nonzero) 
                         n_nonzero <- n_nonzero[, lapply(.SD, function(x) 
                                                         sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         keyby = names(period),
                                                         .SDcols = names(Y1)]
                  } else n_nonzero <- n_nonzero[, lapply(.SD, function(x) 
                                                         sum(as.integer(abs(x) > .Machine$double.eps))),
                                                         .SDcols = names(Y1)]

  respondent_count <- pop_size <- NULL
  nhs <- data.table(respondent_count = 1, pop_size = w_final)
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                       keyby = eval(names(nhs)[0 : 1 - ncol(nhs)]),
                                                      .SDcols = c("respondent_count", "pop_size")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                     .SDcols = c("respondent_count", "pop_size")]


  # Design weights
  if (!is.null(X)) w_design <- w_final / g else w_design <- w_final
      
  # Ratio of two totals
  linratio_outp <- per <- variableZ <- estim <- deff_sam <- NULL
  deff_est <- deff <- var_est2 <- se <- rse <- cv <- NULL
  absolute_margin_of_error <- relative_margin_of_error <- NULL
  sar_nr <- CI_lower <- CI_upper <- variable <- n_eff <- NULL

  idper <- id
  if (!is.null(period)) idper <- data.table(idper, period)

  Z1 <- NULL
  if (!is.null(Z)) {
    if (!is.null(Dom)) Z1 <- domain(Y = Z, D = Dom,
                                    dataset = NULL,
                                    checking = FALSE) else Z1 <- Z
    if (is.null(period)) {
          Y2 <- lin.ratio(Y = Y1, Z = Z1, weight = w_final,
                          Dom = NULL, dataset = NULL,
                          percentratio = percentratio,
                          checking = FALSE)
        } else {
            periodap <- do.call("paste", c(as.list(period), sep = "_"))
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
  Y <- Z <- NULL

  # Calibration
  res_outp <- NULL
  if (!is.null(X)) {
        ind_gr <- data.table(nsk = rep(1, nrow(X)))
        if (!is.null(period)) ind_gr <- data.table(ind_gr, period)
        ind_gr <- do.call("paste", c(as.list(ind_gr), sep = "_"))

        lin1 <- lapply(split(Y2[, .I], ind_gr), function(i) 
                        data.table(sar_nr = i,
                                   residual_est(Y = Y2[i],
                                                X = X[i],
                                                weight = w_design[i],
                                                q = q[i],
                                                dataset = NULL,
                                                checking = FALSE)))
        Y3 <- rbindlist(lin1)
        setkeyv(Y3, "sar_nr")
        Y3[, sar_nr := NULL] 
      if (outp_res) res_outp <- data.table(idper, PSU, Y3)
  } else Y3 <- Y2

  var_est <- variance_othstr(Y = Y3, H = H, H2 = H2,  
                             w_final = w_final, N_h = N_h,
                             N_h2 = N_h2, period = period,
                             dataset = NULL, checking = FALSE)
  s2g <- var_est$s2g
  var_est <- var_est$var_est
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- var_est

  n_nonzero <- transpos(n_nonzero, is.null(period), "n_nonzero", names(period))
  all_result <- merge(all_result, n_nonzero, all = TRUE)

  # Variance of HT estimator under current design
  var_cur_HT <- variance_othstr(Y = Y2, H = H, H2 = H2, 
                                w_final = w_design, N_h = N_h,
                                N_h2 = N_h2, period = period,
                                dataset = NULL, checking = FALSE)
  var_cur_HT <- var_cur_HT$var_est
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT)
  n_nonzero <- var_est <- var_cur_HT <- NULL

  # Variance of HT estimator under SRS
  if (is.null(period)) {
           var_srs_HT <- var_srs(Y2, w = w_design)$varsrs
       } else {
           period_agg <- unique(period)
           lin1 <- lapply(1 : nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y2a)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          data.table(period_agg[i,], 
                                     var_srs(Y2a[ind], w = w_design[ind])$varsrs)
                        })
           var_srs_HT <- rbindlist(lin1)
      }
  var_srs_HT <- transpos(var_srs_HT, is.null(period), "var_srs_HT", names(period))
  all_result <- merge(all_result, var_srs_HT)


  # Variance of calibrated estimator under SRS
  if (is.null(period)) {
           var_srs_ca <- var_srs(Y3, w = w_final)$varsrs
      } else {
           period_agg <- unique(period)
           lin1 <- lapply(1:nrow(period_agg), function(i) {
                          per <- period_agg[i,][rep(1, nrow(Y2a)),]
                          ind <- (rowSums(per == period) == ncol(period))
                          data.table(period_agg[i,], 
                                     var_srs(Y3[ind], w = w_final[ind])$varsrs)
                        })
           var_srs_ca <- rbindlist(lin1)
        }
  Y3 <- Y2a <- NULL
  var_srs_ca <- transpos(var_srs_ca, is.null(period), "var_srs_ca", names(period))
  all_result <- merge(all_result, var_srs_ca)


  # Total estimation
  Y_nov <- Z_nov <- .SD <- confidence_level <- NULL

  hY <- data.table(Y1 * w_final)
  if (is.null(period)) { Y_nov <- hY[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Y1)]
                } else { hY <- data.table(period, hY)
                         Y_nov <- hY[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Y1)]
                       }
  Y_nov <- transpos(Y_nov, is.null(period), "Y_nov", names(period))
  all_result <- merge(all_result, Y_nov)
  
  if (!is.null(Z1)) {
         YZnames <- data.table(variable = names(Y1), variableDZ = names(Z1))
         all_result <- merge(all_result, YZnames, by = "variable")
         
         hZ <- data.table(Z1 * w_final)
         if (is.null(period)) { Z_nov <- hZ[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(Z1)]
                       } else { hZ <- data.table(period, hZ)
                                Z_nov <- hZ[, lapply(.SD, sum, na.rm = TRUE), keyby = names(period), .SDcols = names(Z1)]
                              }
         Z_nov <- transpos(Z_nov, is.null(period), "Z_nov", names(period), "variableDZ")
         all_result <- merge(all_result, Z_nov, by = "variableDZ")
      }

  vars <- data.table(variable = names(Y1), nr_names = 1 : ncol(Y1))
  all_result <- merge(vars, all_result, by = "variable")
                        
  vars <- idper <- Y1 <- Z1 <- Y_nov <- NULL
  Z_nov <- hY <- hZ <- YZnames <- dati <- NULL                            

  
  all_result[, estim := Y_nov]   
  if (!is.null(all_result$Z_nov)) all_result[, estim := Y_nov / Z_nov]

  if (nrow(all_result[var_est < 0]) > 0) print("Estimation of variance are negative!")
 
  # Design effect of sample design
  all_result[, deff_sam := var_cur_HT / var_srs_HT]
  
  # Design effect of estimator
  all_result[, deff_est := var_est / var_cur_HT]
  
  # Overall effect of sample design and estimator
  all_result[, deff := deff_sam * deff_est]

  all_result[, var_est2:= var_est]
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

  setnames(all_result, c("variable", "var_est"), c("variableD", "var"))
  if (!is.null(all_result$Z_nov)) {
                         nosrZ <- all_result$variableDZ
                         nosrZ <- nosrZ[!duplicated(nosrZ)]
                         nosrZ1 <- data.table(variableZ = t(data.frame(strsplit(nosrZ, "__")))[, c(1)])
                         nosrZ <- data.table(variableDZ = nosrZ, nosrZ1)
                         all_result <- merge(all_result, nosrZ, by = "variableDZ")
                         nosrZ <- nosrZ1 <- NULL
                       }

  nosr <- data.table(variableD = all_result$variableD, t(data.frame(strsplit(all_result$variableD, "__"))))
  nosr <- nosr[!duplicated(nosr)]
  nosr <- nosr[, lapply(nosr, as.character)]
  setnames(nosr, names(nosr)[2], "variable")

  namesDom1 <- namesDom
  if (!is.null(Dom)) {
       setnames(nosr, names(nosr)[3:ncol(nosr)], paste0(namesDom, "_new"))
       nhs[, (paste0(namesDom, "_new")) := lapply(namesDom, function(x) make.names(paste0(x, ".", get(x))))]
       namesDom1 <- paste0(namesDom, "_new")
    }

  all_result <- merge(nosr, all_result, by = "variableD")
  namesDom <- nosr <- NULL
  
  if (!is.null(all_result$Z_nov)) {
       all_result[, variable := paste("R", get("variable"), get("variableZ"), sep = "__")] }

  if (!is.null(c(Dom, period))) { all_result <- merge(all_result, nhs, all = TRUE, by = c(namesDom1, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]} 

  all_result[, confidence_level := confidence]
  variab <- c("respondent_count", "n_nonzero", "pop_size", "estim", "var", "se", 
              "rse", "cv", "absolute_margin_of_error", "relative_margin_of_error",
              "CI_lower", "CI_upper", "confidence_level", "var_srs_HT",  "var_cur_HT", 
              "var_srs_ca", "deff_sam", "deff_est", "deff")

  setkeyv(all_result, c("nr_names", names(Dom), names(period)))
  all_result <- all_result[, c("variable", names(Dom), names(period), variab), with = FALSE]
  list(lin_out = linratio_outp,
       res_out = res_outp,
       s2g = s2g,
       all_result = all_result)
}
