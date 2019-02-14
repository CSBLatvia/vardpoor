#******************************************************************************************
#******************************************************************************************
#******************************************************************************************
#***                                                                                    ***
#***                                                                                    ***
#***                    LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE                    ***
#***                                                                                    ***
#***                                                                                    ***
#******************************************************************************************
#******************************************************************************************
#******************************************************************************************

linarpr <- function(Y, id = NULL, weight = NULL, Y_thres = NULL,
                    wght_thres = NULL, sort = NULL, Dom = NULL,
                    period = NULL, dataset = NULL, percentage = 60,
                    order_quant = 50, var_name = "lin_arpr",
                    kern_method = "gaussian", r = NULL, ro = NULL,
                    h_breaks = NULL, checking = TRUE) {

   ## initializations
   if (min(dim(data.table(var_name)) == 1) != 1) {
       stop("'var_name' must have defined one name of the linearized variable")}

   if (checking) {
        percentage <- check_var(vars = percentage, varn = "percentage",
                                varntype = "numeric0100")

        order_quant <- check_var(vars = order_quant, varn = "order_quant",
                                 varntype = "numeric0100")

        kern_method <- check_var(vars = kern_method, varn = "kern_method", varntype = "kern_method")

        r <- check_var(vars = r, varn = "r", varntype = "pinteger",
                       kern_method = kern_method)

        ro <- check_var(vars = ro, varn = "ro", varntype = "numeric01",
                        kern_method = kern_method)

        h_breaks <- check_var(vars = h_breaks, varn = "h_breaks",
                              varntype = "pinteger", kern_method = kern_method)

        Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                       ncols = 1, isnumeric = TRUE,
                       isvector = TRUE, grepls = "__")
        Ynrow <- length(Y)

        Y_thres <- check_var(vars = Y_thres, varn = "Y_thres",
                             dataset = dataset, ncols = 1,
                             Ynrow = Ynrow, mustbedefined = FALSE,
                             isnumeric = TRUE, isvector = TRUE)

        weight <- check_var(vars = weight, varn = "weight",
                            dataset = dataset, ncols = 1,
                            Ynrow = Ynrow, isnumeric = TRUE,
                            isvector = TRUE)

        wght_thres <- check_var(vars = wght_thres, varn = "wght_thres",
                                dataset = dataset, ncols = 1,
                                Ynrow = Ynrow, mustbedefined = FALSE,
                                isnumeric = TRUE, isvector = TRUE)

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

        id <- check_var(vars = id, varn = "id", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        periods = period)
    }
   dataset <- NULL

   if (is.null(Y_thres)) Y_thres <- Y
   if (is.null(wght_thres)) wght_thres <- weight


   ## computations
   ind0 <- rep.int(1, length(Y))
   period_agg <- period1 <- NULL
   if (!is.null(period)) { period1 <- copy(period)
                           period_agg <- data.table(unique(period))
                       } else period1 <- data.table(ind = ind0)
   period1_agg <- data.table(unique(period1))

   # ARPR by domain (if requested)
   quantile <- incPercentile(Y = Y_thres,
                             weights = wght_thres,
                             sort = sort, Dom = NULL,
                             period = period,
                             k = order_quant,
                             dataset = NULL,
                             checking = FALSE)

    setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
    if (ncol(quantile) > 1) setkeyv(quantile, head(names(quantile), -1))
    threshold <- copy(quantile)
    threshold[, threshold := percentage / 100 * quantile]
    threshold[, quantile := NULL]

    arpr_id <- id
    if (!is.null(period)) arpr_id <- data.table(arpr_id, period)

    if (!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))

        arpr_v <- c()
        arpr_m <- copy(arpr_id)
        for(i in 1 : nrow(Dom_agg)) {
              g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
              var_nams <- do.call(paste, as.list(c(g, sep = "__")))
              ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

              arprl <- lapply(1 : nrow(period1_agg), function(j) {
                               if (!is.null(period)) {
                                       rown <- cbind(period_agg[j], Dom_agg[i])
                                       setkeyv(rown, names(rown))
                                       rown2 <- copy(rown)
                                       rown <- merge(rown, quantile, all.x = TRUE)
                                     } else {rown <- quantile
                                             rown2 <- Dom_agg[i] }

                               indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                               arpr_l <- arprlinCalc(Y1 = Y[indj],
                                                     ids = arpr_id[indj],
                                                     wght1 = weight[indj],
                                                     indicator = ind[indj],
                                                     Y_thresh = Y_thres[indj],
                                                     wght_thresh = wght_thres[indj],
                                                     percent = percentage,
                                                     order_quants = order_quant,
                                                     quant_val = rown[["quantile"]],
                                                     kern_method = kern_method, r = r,
                                                     ro = ro, h_breaks = h_breaks)
                      list(arpr = data.table(rown2, arpr = arpr_l$rate_val_pr), lin = arpr_l$lin)
                      })
                 arprs <- rbindlist(lapply(arprl, function(x) x[[1]]))
                 arprlin <- rbindlist(lapply(arprl, function(x) x[[2]]))

                 setnames(arprlin, names(arprlin), c(names(arpr_id), var_nams))
                 arpr_m <- merge(arpr_m, arprlin, all.x = TRUE, by = names(arpr_id))
                 arpr_v <- rbind(arpr_v, arprs)
           }
     } else { arprl <- lapply(1:nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         rown <- merge(rown, quantile, all.x = TRUE,
                                                       by = names(rown))
                                       } else rown <- quantile
                           ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           arpr_l <- arprlinCalc(Y1 = Y[ind2],
                                                 ids = arpr_id[ind2],
                                                 wght1 = weight[ind2],
                                                 indicator = ind0[ind2],
                                                 Y_thresh = Y_thres[ind2],
                                                 wght_thresh = wght_thres[ind2],
                                                 percent = percentage,
                                                 order_quants = order_quant,
                                                 quant_val = rown[["quantile"]],
                                                 kern_method = kern_method, r = r,
                                                 ro = ro, h_breaks = h_breaks)
                          if (!is.null(period)) {
                                   arprs <- data.table(period_agg[j], arpr = arpr_l$rate_val_pr)
                             } else arprs <- data.table(arpr = arpr_l$rate_val_pr)
                          list(arpr = arprs, lin = arpr_l$lin)
                       })
               arpr_v <- rbindlist(lapply(arprl, function(x) x[[1]]))
               arpr_m <- rbindlist(lapply(arprl, function(x) x[[2]]))
               setnames(arpr_m, names(arpr_m), c(names(arpr_id), var_name))
            }
     arpr_m[is.na(arpr_m)] <- 0
     setkeyv(arpr_m, names(arpr_id))
     return(list(quantile = quantile, threshold = threshold, value = arpr_v, lin = arpr_m))
}



## workhorse
arprlinCalc <- function(Y1, ids, wght1, indicator, Y_thresh,
                        wght_thresh, percent, order_quants = NULL,
                        quant_val, kern_method, r, ro, h_breaks) {

    N <- dat <- eqIncome1 <- NULL

    #---- 1. Linearization of the poverty threshold ----
    arpt_calcs <- arptlinCalc(inco = Y_thresh, ids = ids,
                              wght =  wght_thresh,
                              indicator = rep(1, length(ids)),
                              order_quants = order_quants,
                              quant_val = quant_val, percentag = percent,
                              kern_method = kern_method, r = r,
                              ro = ro, h_breaks = h_breaks)
    lin_thres <- arpt_calcs[[names(arpt_calcs)[2]]]

    thres_val <- percent / 100 * quant_val
    wt <- indicator * wght1
    N <- sum(wt)       # Estimated (sub)population size

    poor <- (Y1 <= thres_val)
    rate_val <- sum(wt * poor) / N  # Estimated poverty rate */
    rate_val_pr <- 100 * rate_val


    #---- 2. Linearization of the poverty rate -----

    h <- bandwith_plug(y = Y1, w = wt)

    if (kern_method == "gaussian") {f_quant2 <- gaussian_kern(inco = Y1, wt = wt,
                                                             quant_val = quant_val, hh = h)}
    if (kern_method == "smooth_splines") {f_quant2 <- smooth_spline(inco = Y1, wght = wt,
                                                                   quant_val = quant_val,
                                                                   r = r, ro = ro,
                                                                   h_breaks = h_breaks) }

 #****************************************************************************************
 #                       LINEARIZED VARIABLE OF THE POVERTY RATE (IN %)                  *
 #****************************************************************************************
    lin <- 100 * ((1 / N) * indicator * ((Y1 <= thres_val) - rate_val) + f_quant2 * lin_thres)

    lin_id <- data.table(ids, lin)
    return(list(rate_val = rate_val, rate_val_pr = rate_val_pr, lin = lin_id))
}

