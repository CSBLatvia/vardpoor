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
                     PSU_sort = NULL,
                     fh_zero = FALSE,
                     PSU_level=TRUE,
                     sort = NULL,
                     Dom = NULL,
                     period = NULL,
                     gender = NULL,
                     dataset = NULL,
                     X = NULL,
                     periodX = NULL,
                     X_ID_level1 = NULL,
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
                     kern_method = "gaussian",
                     r = NULL,
                     ro = NULL,
                     h_breaks = NULL,
                     type="linrmpg") {

  ### Checking

  all_choices <- c("linarpr","linarpt","lingpg","linpoormed",
                   "linrmpg","lingini","lingini2", "linqsr", "linrmir", "linarr")
  type <- tolower(type)
  type <- match.arg(type, all_choices, length(type) > 1)

  fh_zero <- check_var(vars = fh_zero, varn = "fh_zero", varntype = "logical")
  PSU_level <- check_var(vars = PSU_level, varn = "PSU_level", varntype = "logical")
  outp_lin <- check_var(vars = outp_lin, varn = "outp_lin", varntype = "logical")
  outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical")

  percentage <- check_var(vars = percentage, varn = "percentage", varntype = "numeric0100")
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "numeric0100")
  alpha <- check_var(vars = alpha, varn = "alpha", varntype = "numeric0100")
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")

  kern_method <- check_var(vars = kern_method, varn = "kern_method", varntype = "kern_method")
  r <- check_var(vars = r, varn = "r", varntype = "pinteger", kern_method = kern_method)
  ro <- check_var(vars = ro, varn = "ro", varntype = "numeric01", kern_method = kern_method)
  h_breaks <- check_var(vars = h_breaks, varn = "h_breaks",
                        varntype = "pinteger", kern_method = kern_method)

  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 ncols = 1, isnumeric = TRUE,
                 isvector = TRUE, grepls = "__")
  Ynrow <- length(Y)

  w_final <- check_var(vars = w_final, varn = "weight",
                       dataset = dataset, ncols = 1,
                       Ynrow = Ynrow, isnumeric = TRUE,
                       isvector = TRUE)

  age <- check_var(vars = age, varn = "age", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE,
                   mustbedefined = any(c("linarr", "linrmir") %in% type))

  pl085 <- check_var(vars = pl085, varn = "pl085", dataset = dataset,
                     ncols = 1, Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE,
                     mustbedefined = any(type == "linarr"))

  month_at_work <- check_var(vars = month_at_work, varn = "month_at_work",
                             dataset = dataset, ncols = 1, Ynrow = Ynrow,
                             isnumeric = TRUE, isvector = TRUE,
                             mustbedefined = any(type == "linarr"))

  gender <- check_var(vars = gender, varn = "gender", dataset = dataset,
                      ncols = 1, Ynrow = Ynrow, isnumeric = TRUE,
                      isvector = TRUE, mustbedefined = any(type == "lingpg"))

  Y_den <- check_var(vars = Y_den, varn = "Y_den", dataset = dataset,
                     ncols = 1, Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE,
                     mustbedefined = any("linarr" == type))

  Y_thres <- check_var(vars = Y_thres, varn = "Y_thres",
                       dataset = dataset, ncols = 1,
                       Ynrow = Ynrow, mustbedefined = FALSE,
                       isnumeric = TRUE, isvector = TRUE)

  wght_thres <- check_var(vars = wght_thres, varn = "wght_thres",
                          dataset = dataset, ncols = 1,
                          Ynrow = Ynrow, mustbedefined = FALSE,
                          isnumeric = TRUE, isvector = TRUE)

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, ischaracter = TRUE)

  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, ischaracter = TRUE,
                         namesID1 = names(ID_level1), periods = period)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 ischaracter = TRUE, namesID1 = names(ID_level1),
                 dif_name = "dataH_stratas")

  sort <- check_var(vars = sort, varn = "sort",
                    dataset = dataset, ncols = 1,
                    Ynrow = Ynrow, mustbedefined = FALSE,
                    isnumeric = TRUE, isvector = TRUE)

  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, mustbedefined = FALSE,
                      duplicatednames = TRUE)

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   grepls = "__")

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, namesID1 = names(ID_level1))

  PSU_sort <- check_var(vars = PSU_sort, varn = "PSU_sort", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        isvector = TRUE, mustbedefined = FALSE, PSUs = PSU)

  if(!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) |
      !is.null(periodX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
       X <- check_var(vars = X, varn = "X", dataset = datasetX,
                      check.names = TRUE, isnumeric = TRUE,
                      dif_name = c(names(period) , "g", "q"), dX = "X")
       Xnrow <- nrow(X)

       ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                           dataset = datasetX, ncols = 1, Xnrow = Xnrow, dX = "X",
                           ischaracter = TRUE, dif_name = c(names(period) , "g", "q"))

       g <- check_var(vars = g, varn = "g", dataset = datasetX,
                      ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                      isvector = TRUE, dX = "X")

       q <- check_var(vars = q, varn = "q", dataset = datasetX,
                      ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                      isvector = TRUE, dX = "X")

       periodX <- check_var(vars = periodX, varn = "periodX",
                            dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                            ischaracter = TRUE, mustbedefined = !is.null(period),
                            duplicatednames = TRUE, varnout = "period",
                            varname = names(period), dX = "X")

       X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                ischaracter = TRUE, varnout = "ID_level1",
                                varname = names(ID_level1), periods = period,
                                periodsX = periodX, ID_level1 = ID_level1, dX = "X")
   }

 # N_h
 np <- sum(ncol(period))
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
                pH <- NULL }
     setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
 }

  N <- dataset <- datasetX <- NULL

  if (is.null(Y_thres)) Y_thres <- Y
  if (is.null(wght_thres)) wght_thres <- w_final
  psusn <- as.integer(!is.null(PSU_sort))


  # Design weights
  if (!is.null(X)) {
             ID_level1h <- data.table(ID_level1)
             if (!is.null(period)) { ID_level1h <- data.table(period, ID_level1h)
                                     X_ID_level1 <- data.table(period, X_ID_level1)
                              }
             idhx <- data.table(X_ID_level1, g)
             setnames(idhx, names(idhx)[c(1 : (ncol(idhx) - 1))], names(ID_level1h))
             idg <- merge(ID_level1h, idhx, by = names(ID_level1h), sort = FALSE)
             w_design <- w_final / idg[[ncol(idg)]]
             idg <- data.table(idg, w_design = w_design)
             idh <- idg[, .N, keyby = c(names(ID_level1h), "w_design")]
             if (nrow(X) != nrow(idh))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- ID_level1h <- NULL
      } else w_design <- w_final

  ### Calculation
  sar_nr <- respondent_count <- pop_size <- n_nonzero <- NULL
  nhs <- data.table(respondent_count = 1, pop_size = w_final,
                    n_nonzero = as.integer(abs(Y) > .Machine$double.eps))
  if (!is.null(period)) nhs <- data.table(period, nhs)
  if (!is.null(Dom)) nhs <- data.table(Dom, nhs)
  if (!is.null(c(Dom, period))) {nhs <- nhs[, lapply(.SD, sum, na.rm = TRUE),
                                                       keyby = eval(names(nhs)[0:2-ncol(nhs)]),
                                                      .SDcols = c("respondent_count", "pop_size", "n_nonzero")]
                          } else nhs <- nhs[, lapply(.SD, sum, na.rm=TRUE),
                                                     .SDcols=c("respondent_count", "pop_size", "n_nonzero")]

  estim <- c()
  aH <- names(H)
  idper <- copy(ID_level2)
  Y1sort <- Y1asort <- NULL
  aPSU <- names(PSU)
  if (!is.null(period)) idper <- data.table(idper, period)

  Y1 <- data.table(idper, ID_level1, H, PSU, check.names = TRUE)
  if (!is.null(PSU_sort)) Y1 <- data.table(Y1, PSU_sort, check.names = TRUE)
  Y1 <- data.table(Y1, w_design, w_final)


  Y1[, Y1sort := .I]
  setkeyv(Y1, names(idper))
  value <- NULL

  if ("linarpt" %in% type) {
       varpt <- linarpt(Y = Y, id = ID_level2, weight = w_final,
                        sort = sort, Dom = Dom, period = period,
                        dataset = NULL, percentage = percentage,
                        order_quant = order_quant, var_name = "lin_arpt",
                        kern_method = "gaussian", r = r, ro = ro,
                        h_breaks = h_breaks, checking = FALSE)
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
                        var_name = "lin_arpr",
                        kern_method = "gaussian", r = r,
                        ro = ro, h_breaks = h_breaks,
                        checking = FALSE)

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
                       var_name = "lin_gpg", checking = FALSE)

        Y1 <- merge(Y1, vgpg$lin, all.x = TRUE)

        esti <- data.table("GPG", vgpg$value, NA)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vgpg <- esti <- NULL
     }
  if ("linpoormed" %in% type) {
        vporm <- linpoormed(Y = Y, id = ID_level2, weight = w_final,
                            sort = sort, Dom = Dom, period = period,
                            dataset = NULL, percentage = percentage,
                            order_quant = order_quant, var_name = "lin_poormed",
                            checking = FALSE)
        Y1 <- merge(Y1, vporm$lin, all.x = TRUE)

        esti <- data.table("POORMED", vporm$value, NA)
        setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
        estim <- rbind(estim, esti)
        vporm <- esti <- NULL
     }
  if ("linrmpg" %in% type) {
        vrmpg <- linrmpg(Y = Y, id = ID_level2, weight = w_final,
                         sort = sort, Dom = Dom, period = period,
                         dataset = NULL, percentage = percentage,
                         order_quant = order_quant, var_name = "lin_rmpg",
                         checking = FALSE)
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
                       dataset = NULL, alpha = alpha, var_name = "lin_qsr",
                       checking = FALSE)
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
                         dataset = NULL, var_name = "lin_gini",
                         checking = FALSE)
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
                          dataset = NULL, var_name = "lin_gini2",
                          checking = FALSE)
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
                        var_name = "lin_rmir",
                        checking = FALSE)
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
                      var_name = "lin_arr",
                      checking = FALSE)
       Y1 <- merge(Y1, varr$lin, all.x = TRUE)

       esti <- data.table("ARR", varr$value, NA)
       setnames(esti, names(esti)[c(1, -1 : 0 + ncol(esti))],
                                  c("type", "value", "value_eu"))
       estim <- rbind(estim, esti)
       varr <- esti <- NULL
    }


  estim[, variable := paste0("lin_", tolower(type))]
  nDom <- names(Dom)
  if (!is.null(nDom)) estim[, (paste0(nDom, "at1at")) := lapply(nDom, function(x) paste(x, get(x), sep = "."))]

  Dom <- estim[, "variable"]
  if (!is.null(nDom)) Dom <- estim[, c("variable", paste0(nDom, "at1at")), with = FALSE]

  estim$variable <- do.call("paste", c(as.list(Dom), sep = "__"))
  estim[, variable := str_replace_all(variable, "[ ]", ".")]
  if (!is.null(nDom)) estim[, (paste0(nDom, "at1at")) := NULL]
  all_result <- estim


  setkey(Y1, Y1sort)
  Y1[, Y1sort := NULL]

  estim <- .SD <- lin_outp <- NULL
  if (outp_lin) lin_outp <- Y1[, c(-(3 : 5) - np), with = FALSE]

  Y2 <- Y1[, lapply(.SD, sum, na.rm = TRUE), by = c(names(Y1)[c(2 : (6 + np + psusn))]), .SDcols = names(Y1)[- (1 : (6 + np + psusn))]]
  Y3 <- Y2[, c(-(1 : (5 + np + psusn))), with = FALSE]

  idper <- period <- NULL
  if (np > 0) period <- Y2[, c(1 : np), with = FALSE]

  ID_level1h <- Y2[, np + 1, with = FALSE]
  H <- Y2[, np + 2, with = FALSE]
  setnames(H, names(H), aH)

  PSU <- Y2[, np + 3, with = FALSE]
  setnames(PSU, names(PSU), aPSU)

  if (!is.null(PSU_sort)) PSU_sort <- Y2[[np + 4]]

  w_design2 <- Y2[[np + 4 + psusn]]
  w_final2 <- Y2[[np + 5 + psusn]]

  Y1 <- Y2 <- NULL

  # Calibration

  res_outp <- betas <- variable <- NULL
  if (!is.null(X)) {
       if (np > 0) ID_level1h <- data.table(period, ID_level1h)
       setnames(ID_level1h, names(ID_level1h), names(X_ID_level1))
       X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
       D1 <- merge(ID_level1h, X0, by = names(ID_level1h), sort = FALSE)
       ind_gr <- D1[, np + 2, with = FALSE]
       if (!is.null(period)) ind_gr <- data.table(D1[, names(periodX), with = FALSE], ind_gr)
       ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))

       lin1 <- lapply(split(Y3[, .I], ind_period), function(i) {
                            resid <- residual_est(Y = Y3[i],
                                                  X = D1[i, (np + 5) : ncol(D1), with = FALSE],
                                                  weight = w_design2[i],
                                                  q = D1[i][["q"]],
                                                  dataset = NULL,
                                                  checking = FALSE)
                            pers0 <- ind_gr[i, .N, keyby = c(names(ind_gr))]
                            list(data.table(sar_nr = i, resid$residuals),
                                 data.table(pers0[, N := NULL], resid$betas))
                                    })
       Y4 <- rbindlist(lapply(lin1, function(x) x[[1]]))
       betas <- rbindlist(lapply(lin1, function(x) x[[2]]))
       setkeyv(Y4, "sar_nr")
       Y4[, sar_nr := NULL]
       if (outp_res) res_outp <- data.table(ID_level1h, PSU, w_final2, Y4)
   } else Y4 <- Y3
   lin1 <- X0 <- D1 <- ind_gr <- ID_level1h <- X_ID_level1 <- q <- g <- NULL

  var_est <- variance_est(Y = Y4, H = H, PSU = PSU, w_final = w_final2,
                          N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                          PSU_sort = PSU_sort, period = period, dataset = NULL,
                          msg = "Current variance estimation",
                          checking = FALSE)
  var_est <- transpos(var_est, is.null(period), "var_est", names(period))
  all_result <- merge(var_est, all_result, all = TRUE, by = c(names(period), "variable"))

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y = Y3, H = H, PSU = PSU, w_final = w_design2,
                             N_h = N_h, fh_zero = fh_zero, PSU_level = PSU_level,
                             PSU_sort = PSU_sort, period = period, dataset = NULL,
                             msg = "Variance of HT estimator under current design",
                             checking = FALSE)
  var_cur_HT <- transpos(var_cur_HT, is.null(period), "var_cur_HT", names(period))
  all_result <- merge(all_result, var_cur_HT, by = c(names(period), "variable"))
  H <- PSU <- PSU_sort <-  N_h <- var_est <- var_cur_HT <- NULL

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
  all_result <- merge(all_result, var_srs_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_HT <- transpos(S2_y_HT, is.null(period), "S2_y_HT", names(period))
  all_result <- merge(all_result, S2_y_HT, all = TRUE, by = c(names(period), "variable"))
  S2_y_ca <- transpos(S2_y_ca, is.null(period), "S2_y_ca", names(period))
  all_result <- merge(all_result, S2_y_ca, all = TRUE, by = c(names(period), "variable"))

  Y3 <- w_design2 <- var_srs_HT <- S2_y_HT <- S2_y_ca <- NULL

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
  all_result <- merge(all_result, var_srs_ca, all = TRUE, by = c(names(period), "variable"))
  S2_res <- transpos(S2_res, is.null(period), "S2_res", names(period), "variable")
  all_result <- merge(all_result, S2_res, all = TRUE, by = c(names(period), "variable"))
  var_srs_ca <- S2_res <- Y4 <- w_final2 <- NULL

  all_result[, variable := NULL]
  deff_sam <- deff_est <- deff <- n_eff <- var_est2 <- NULL
  se <- rse <- cv <- absolute_margin_of_error <- NULL
  relative_margin_of_error <- CI_lower <- CI_upper <- NULL

  if (nrow(all_result[var_est < 0]) > 0) stop("Estimation of variance are negative!")

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
  all_result[(value != 0) & (!is.nan(value)), rse := se / value]
  all_result[value == 0 | is.nan(value), rse := NA]
  all_result[, cv := rse * 100]

  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, absolute_margin_of_error := tsad * se]
  all_result[, relative_margin_of_error := tsad * cv]
  all_result[, CI_lower := value - tsad * se]
  all_result[, CI_upper := value + tsad * se]

  setnames(all_result, "var_est", "var")

  if (!is.null(c(nDom, period))) { all_result <- merge(all_result, nhs,
                                                       all = TRUE, by = c(nDom, names(period)))
                         } else { all_result[, respondent_count := nhs$respondent_count]
                                  all_result[, pop_size := nhs$pop_size]
                                  all_result[, n_nonzero := nhs$n_nonzero]}

  variabl <- c("respondent_count", "n_nonzero", "pop_size",
               "value", "value_eu", "var", "se", "rse", "cv",
               "absolute_margin_of_error", "relative_margin_of_error",
               "CI_lower", "CI_upper")

  if (is.null(nDom))  variabl <- c(variabl, "S2_y_HT", "S2_y_ca", "S2_res")
  variabl <- c(variabl, "var_srs_HT",  "var_cur_HT", "var_srs_ca",
               "deff_sam", "deff_est", "deff")

  type <- "type"
  if (!is.null(period)) type <- c(type, names(period))
  setkeyv(all_result, c(type, nDom))
  list(lin_out = lin_outp,
       res_out = res_outp,
       betas = betas,
       all_result = all_result[, c(type, nDom, variabl), with = FALSE])
}
