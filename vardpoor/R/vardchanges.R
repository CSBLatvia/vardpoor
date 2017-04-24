vardchanges <- function(Y, H, PSU, w_final,
                        ID_level1, ID_level2,
                        Dom = NULL, Z = NULL,
                        gender = NULL,
                        country = NULL, period,
                        dataset = NULL,
                        period1, period2,
                        X = NULL, countryX = NULL,
                        periodX = NULL, X_ID_level1 = NULL,
                        ind_gr = NULL, g = NULL,
                        q = NULL, datasetX = NULL,
                        linratio = FALSE,
                        percentratio = 1,
                        use.estVar = FALSE,
                        outp_res = FALSE,
                        confidence = 0.95,
                        change_type = "absolute",
                        checking = TRUE) {

  ### Checking
  change_type <- check_var(vars = change_type, varn ="change_type", varntype = "change_type") 
         
  if (checking) { 
        percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger") 
        linratio <- check_var(vars = linratio, varn = "linratio", varntype = "logical") 
        use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical")
        outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical") 
        confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")

        if(!is.null(X)) {
            if (is.null(datasetX)) datasetX <- copy(dataset)
            equal_dataset <- identical(dataset, datasetX) & !is.null(dataset)
            if (equal_dataset) { X_ID_level1 <- ID_level1
            countryX <- country }}

        Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                       check.names = TRUE, isnumeric = TRUE, grepls = "__")
        Ynrow <- nrow(Y)
        Yncol <- ncol(Y)

        H <- check_var(vars = H, varn = "H", dataset = dataset,
                       ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        dif_name = "dataH_stratas")

        w_final <- check_var(vars = w_final, varn = "w_final",
                             dataset = dataset, ncols = 1, Ynrow = Ynrow,
                             isnumeric = TRUE, isvector = TRUE)
   
        gender <- check_var(vars = gender, varn = "gender",
                            dataset = dataset, ncols = 1, Ynrow = Ynrow,
                            isnumeric = TRUE, isvector = TRUE,
                            mustbedefined = FALSE)

        Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                       check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                       isnumeric = TRUE, mustbedefined = FALSE)

        Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                         ncols = 0, Yncol = 0, Ynrow = Ynrow,
                         ischaracter = TRUE, mustbedefined = FALSE,
                         duplicatednames = TRUE, grepls = "__")

        country <- check_var(vars = country, varn = "country",
                             dataset = dataset, ncols = 1, Ynrow = Ynrow,
                             ischaracter = TRUE, mustbedefined = FALSE,
                             dif_name = c("percoun", "period_country"))

        period <- check_var(vars = period, varn = "period",
                            dataset = dataset, ncols = 1, Ynrow = Ynrow,
                            ischaracter = TRUE, duplicatednames = TRUE,
                            dif_name = c("percoun", "period_country", names(country)))

        period1 <- check_var(vars = period1, varn = "period1", dataset = NULL,
                             ncols = 1, ischaracter = TRUE, periods = period)

        period2 <- check_var(vars = period2, varn = "period2", dataset = NULL,
                              ncols = 1, ischaracter = TRUE, periods = period)

        ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                               dataset = dataset, ncols = 1, Ynrow = Ynrow,
                               ischaracter = TRUE)

        ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                               dataset = dataset, ncols = 1, Ynrow = Ynrow,
                               ischaracter = TRUE, namesID1 = names(ID_level1),
                               country = country, periods = period)

        PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                         ncols = 1, Yncol = 0, Ynrow = Ynrow,
                         ischaracter = TRUE, namesID1 = names(ID_level1))

        if(!is.null(X)) {
                X <- check_var(vars = X, varn = "X", dataset = datasetX,
                               check.names = TRUE, isnumeric = TRUE,
                               dif_name = c(names(period), names(country), names(H),
                                            names(PSU), names(ID_level1), "w_final",
                                            names(Y), "w_design", "g", "q"))
                Xnrow <- nrow(X)

                ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                                    dataset = datasetX, ncols = 1,
                                    Xnrow = Xnrow, ischaracter = TRUE, 
                                    dif_name = c(names(period), names(country), names(H),
                                                 names(PSU), names(ID_level1), "w_final",
                                                 names(Y), names(X), "w_design", "g", "q"))

                g <- check_var(vars = g, varn = "g", dataset = datasetX,
                               ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                               isvector = TRUE)

                q <- check_var(vars = q, varn = "q", dataset = datasetX,
                               ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                               isvector = TRUE)

                countryX <- check_var(vars = countryX, varn = "countryX",
                                      dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                      ischaracter = TRUE, mustbedefined = !is.null(country),
                                      varnout = "country", varname = names(country),
                                      country = country)

                periodX <- check_var(vars = periodX, varn = "periodX",
                                     dataset = datasetX, ncols = 1,
                                     Xnrow = Xnrow, ischaracter = TRUE,
                                     mustbedefined = !is.null(period),
                                     duplicatednames = TRUE, varnout = "period",
                                     varname = names(period), country = country,
                                     countryX = countryX, periods = period)

                X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                         dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                         ischaracter = TRUE, varnout = "ID_level1",
                                         varname = names(ID_level1), country = country,
                                         countryX = countryX, periods = period,
                                         periodsX = periodX, ID_level1 = ID_level1)
             }
    }
  percoun <- dataset <- datasetX <- NULL
  namesZ <- names(Z)

  cros_calc <- vardcros(Y = Y, H = H, PSU = PSU, w_final = w_final,
                        ID_level1 = ID_level1, ID_level2 = ID_level2,
                        Dom = Dom, gender = gender, Z = Z,
                        country = country, period = period,
                        dataset = NULL, X = X, countryX = countryX,
                        periodX = periodX, X_ID_level1 = X_ID_level1,
                        ind_gr = ind_gr, g = g, q = q, datasetX = NULL,
                        linratio = linratio,
                        percentratio = percentratio,
                        use.estVar = use.estVar,
                        ID_level1_max = is.null(X),
                        outp_res = outp_res,
                        withperiod = TRUE,
                        netchanges = TRUE,
                        confidence = confidence,
                        checking = FALSE)

  countryX <- periodX <- X_ID_level1 <- NULL
  X_ID_level1 <- ind_gr <- g  <- q  <- NULL
  gender <- dataset <- w_final <- NULL

  if (!is.null(Dom)) {
            Y1 <- namesD(Y, Dom)
            if (!is.null(Z)) Z1 <- namesD(Z, Dom)
       } else { Y1 <- names(Y)
                Z1 <- names(Z) }

  
  changes_calc <- vardchanges_calculation(Y = names(Y), Z = names(Z), Y1 = Y1, Z1 = Z1,
                                          Dom = names(Dom), names_country = names(country),
                                          per = names(period), PSU = names(PSU), H = names(H),
                                          period1 = period1, period2 = period2,
                                          cros_var_grad = cros_calc$var_grad, change_type = change_type,
                                          data = cros_calc$data_net_changes, linratio = linratio,
                                          annual = FALSE, percentratio = percentratio,
                                          use.estVar = use.estVar, confidence = confidence)
  Y <- Z <- Y1 <- Z1 <- Dom <- period <- PSU <- H <- period1 <- period2 <- NULL
 
  crossectional_results <- cros_calc$results
  if (is.null(names(country))) crossectional_results[, percoun := NULL]
 
  list(res_out = cros_calc$res_out,
       crossectional_results = crossectional_results,
       crossectional_var_grad = changes_calc$cros_var_grad,
       grad_var = changes_calc$grad_var,
       rho = changes_calc$rho_matrix,
       var_tau = changes_calc$var_tau,
       changes_results = changes_calc$changes_results)
 }


vardchanges_calculation <- function(Y, Z, Y1, Z1, Dom, names_country,
                                    per, PSU, H, period1, period2,
                                    cros_var_grad, change_type, data,
                                    linratio, annual, percentratio, 
                                    use.estVar, confidence){

  country <- ifelse(!is.null(names_country), names_country, "percoun")
  sarp <- c(country, H, PSU)

  namesY <- namesZ <- ind <- nameYs <- nameZs <- grad1 <- grad2 <- NULL
  rot_1 <- rot_2 <- rot_1_rot_2 <- stratasf <- name1 <- NULL
  num1 <- num1num1 <- den1den1 <- den1 <- num2num2 <- NULL
  den2den2 <- den2 <- num1den1 <- num1num2 <- num2 <- NULL
  num1den2 <- den1num2 <- den1den2 <- num2den2 <- num1_1 <- NULL
  den1_1 <- num1den1 <- den1den1 <- num1_2 <- den1_2 <- NULL
  estim <- estim_1 <- estim_2 <- grad1_1 <- grad1_2 <- NULL
  CI_upper <- grad2_1 <- ids_nr <- rot <- grad2_2 <- NULL
  se <-  CI_lower <- valueY1_1 <- valueZ1_1 <- valueY1_2 <- NULL
  valueZ1_2 <- nh <- period_country_1 <- period_country_2 <- NULL
  nhcor <- significant <- id_nams <- nams <- ids_nr <- NULL
  N <- percoun <- NULL

  per1 <- paste0(per, "_1")
  per2 <- paste0(per, "_2")
  period1[, ind := .I]
  period2[, ind := .I]
  setnames(period1, per, per1)
  setnames(period2, per, per2)
  period1 <- merge(period1, period2, by = "ind")
  period2 <- NULL
  var_grad1 <- merge(period1, cros_var_grad, all.x = TRUE,
                              by.x = per1, by.y = per,
                              allow.cartesian = TRUE)
  var_grad2 <- merge(period1, cros_var_grad, all.x = TRUE,
                              by.x = per2, by.y = per,
                              allow.cartesian = TRUE)

  sarc <- c("ind", per1, per2, country, Dom, "namesY", "namesZ")
  sarc <- sarc[sarc %in% names(var_grad1)]
  sar <- names(var_grad1)[!(names(var_grad1) %in% sarc)]
  setnames(var_grad1, sar, paste0(sar, "_1"))
  setnames(var_grad2, sar, paste0(sar, "_2"))

  var_grad <- merge(var_grad1, var_grad2, all = TRUE, by = sarc)
  var_grad[, ids_nr := 1 : .N]

  if (change_type == "relative"){
        if (!linratio & !is.null(Z)){
             var_grad[, grad1_1 := - valueY1_2 * valueZ1_1 / (valueZ1_2 * (valueY1_1)^2)]
             var_grad[, grad1_2 := valueY1_2 / (valueZ1_2 * valueY1_1)]
             var_grad[, grad2_1 := valueZ1_1 / (valueZ1_2 * valueY1_1)]
             var_grad[, grad1_1 := - valueY1_2 * valueZ1_1 / ((valueZ1_2)^2 * valueY1_1)]
          } else {
             var_grad[, grad1_1 := - valueY1_2 / (valueY1_1)^2]
             var_grad[, grad1_2 := 1 / valueY1_1] }
     } else {
        if (!is.null(var_grad$grad1_1)){
                var_grad[, grad1_1 := - grad1_1]
                var_grad[, grad2_1 := - grad2_1]
           } else {var_grad[, grad1_1 := - 1]
                   var_grad[, grad1_2 := 1] }}

  var_grad11 <- copy(var_grad)
  var_grad12 <- copy(var_grad)
  var_grad11[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad1_1, num1_1, 1, "num1")]
  var_grad12[, (c("grad", "cros_var", "nams")) := list(grad1_2, num1_2, "num2")]
  var_grad12[, id_nams := 2 + as.numeric(!is.null(var_grad$grad2_1))]

  var_grad21 <- var_grad22 <- NULL
  if (!is.null(var_grad$grad2_1)) {
        var_grad21 <- copy(var_grad)
        var_grad22 <- copy(var_grad)
        var_grad21[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad2_1, den1_1, 2, "den1")]
        var_grad22[, (c("grad", "cros_var", "id_nams", "nams")) := list(grad2_2, den1_2, 4, "den2")]
   }
  var_gradn <- rbindlist(list(var_grad11, var_grad12,
                              var_grad21, var_grad22), fill = TRUE)

  var_gradn <- var_gradn[, c(sarc, "ids_nr", "id_nams",
                             "nams", "grad", "cros_var"), with = FALSE]

  var_grad11 <- var_grad12 <- NULL
  var_grad21 <- var_grad22 <- NULL


  data[, rot := 1]
  data1 <- merge(period1, data, all.x = TRUE,
                    by.x = per1, by.y = per,
                    allow.cartesian = TRUE)
  data2 <- merge(period1, data, all.x = TRUE,
                    by.x = per2, by.y = per,
                    allow.cartesian = TRUE)
  sard <- names(data)[!(names(data) %in% c(sarp, per))]

  setnames(data1, sard, paste0(sard, "_1"))
  setnames(data2, sard, paste0(sard, "_2"))

  data <- merge(data1, data2, all = TRUE, by = c("ind", per1, per2, sarp))

  if (country == "country") {
               data[is.na(period_country_1), period_country_1 := paste(get(per1), country, sep = "_")] 
               data[is.na(period_country_2), period_country_2 := paste(get(per2), country, sep = "_")]
          } else {
               data[is.na(period_country_1), period_country_1 := paste(get(per1), get(country), sep = "_")] 
               data[is.na(period_country_2), period_country_2 := paste(get(per2), get(country), sep = "_")] }

  data1 <- data2 <- NULL

  recode.NA <- function(DT, cols = seq_len(ncol(DT))) {
     for (j in cols) if (is.numeric(DT[[j]]))
      set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]), 0L, 0))
   }

  data[, nh := .N, by = c("ind", "period_country_1", "period_country_2", H)]

  dataH <- data[[H]]
  dataH <- factor(dataH)
  if (length(levels(dataH)) == 1) { data[, stratasf := 1]
                                  dataH <- "stratasf"
                         } else { dataH <- data.table(model.matrix( ~ dataH - 1))
                                  data <- cbind(data, dataH)
                                  dataH <- names(dataH) }
  den1 <- den2 <- NULL
  sard <- sard[!(sard %in% "period_country")]
  recode.NA(data, c(paste0(sard, "_1"), paste0(sard, "_2")))

  fit <- lapply(1 : length(Y1), function(i) {
       fitd <- lapply(split(data, data[["ind"]]), function(data1) {
            fits <- lapply(split(data1, data1[[country]]), function(DT3c) {

                      y1 <- paste0(Y1[i], "_1")
                      y2 <- paste0(Y1[i], "_2")
                      if (!is.null(namesZ) & !linratio) {
                                    z1 <- paste0(",", Z1[i], "_1")
	                              z2 <- paste0(",", Z1[i], "_2")
                               } else z1 <- z2 <- ""

                      funkc <- as.formula(paste0("cbind(", y1, z1, ", ",
                                                           y2, z2, ") ~ 0 + ",
                                                           paste(t(unlist(lapply(dataH, function(x)
                                                                     paste0("rot_1 : ", toString(x), "+",
                                                                            "rot_2 : ", toString(x), "+",
                                                                            "rot_1 : rot_2 : ", toString(x))))),
                                                                            collapse= "+")))
                      res <- lm(funkc, data = DT3c)
                      if (use.estVar) { res <- data.table(estVar(res))
                                  } else res <- data.table(res$res)

                      if (!is.null(namesZ) & !linratio) {
                                   setnames(res, names(res), c("num1", "den1", "num2", "den2"))
                                   res[, nameZs := Z1[i]]
                            } else setnames(res, names(res), c("num1", "num2"))

                      nosv <- c("num1", "den1", "num2", "den2")
                      nosv <- names(res)[names(res) %in% nosv]
                      Zvn <- as.integer(!is.null(namesZ) & !linratio)
                      res[, nameYs := Y1[i]]

                      keynames <- c(country, "ind", paste0(per, "_1"),
                                    paste0(per, "_2"), "nameYs", "nameZs")
                      keynames <- keynames[keynames %in% c(names(DT3c), names(res))]

                      if (use.estVar) {
                            res <- data.table(id_nams = 1 : nrow(res), nams = nosv, res, DT3c[1])
                        } else {
                            res <- data.table(res, DT3c)
                            if (annual) { res[, nhcor:=ifelse(nh > 1, nh / (nh - 1), 1)]
                                        } else res[, nhcor := 1]

                            res[, num1num1 := num1 * num1 * nhcor]
                            res[, num2num2 := num2 * num2 * nhcor]
                            res[, num1num2 := num1 * num2 * nhcor]
                            res[, id_nams := 0]
                            res[, nams := ""]
                            if (!is.null(namesZ) & !linratio) {
                                  res[, den1den1 := den1 * den1 * nhcor]
                                  res[, den2den2 := den2 * den2 * nhcor]
                                  res[, num1den1 := num1 * den1 * nhcor]
                                  res[, num1den2 := num1 * den2 * nhcor]
                                  res[, den1num2 := den1 * num2 * nhcor]
                                  res[, den1den2 := den1 * den2 * nhcor]
                                  res[, num2den2 := num2 * den2 * nhcor] }

                            varsp <- c("num1num1", "den1den1",
                                       "num2num2", "den2den2",
                                       "num1den1", "num1num2",
                                       "num1den2", "den1num2",
                                       "den1den2", "num2den2")
                            varsp <- varsp[varsp %in% names(res)]
                            fits <- res[, lapply(.SD, sum), keyby = c(keynames,
                                                              "id_nams", "nams"),
                                                           .SDcols = varsp]
                            fits1 <- copy(fits)
                            fits1[, (c("id_nams", "nams")) := list(1, "num1")]
                            setnames(fits1, (c("num1num1", "num1num2")), c("num1", "num2"))

                            fits2 <- copy(fits)
                            fits2[, id_nams := 2 + as.numeric(!is.null(fits$den2den2))]
                            fits2[, nams := "num2"]
                            setnames(fits2, c("num1num2", "num2num2"), c("num1", "num2"))

                            fits3 <- fits4 <- NULL
                             if (!is.null(fits$den2den2)){
                                 setnames(fits1, c("num1den1", "num1den2"), c("den1", "den2"))
                                 setnames(fits2, c("den1num2", "num2den2"), c("den1", "den2"))

                                 fits3 <- copy(fits)
                                 fits3[, (c("id_nams", "nams")) := list(2, "den1")]
                                 setnames(fits3, c("num1den1", "den1num2",
                                                   "den1den1", "den1den2"),
                                                 c("num1", "num2", "den1", "den2"))

                                 fits4 <- copy(fits)
                                 fits4[, (c("id_nams", "nams")) := list(4, "den2")]
                                 setnames(fits4, c("num1den2", "num2den2",
                                                   "den1den2", "den2den2"),
                                                 c("num1", "num2", "den1", "den2"))
                               }
                            res <- rbindlist(list(fits1, fits2, fits3, fits4), fill = TRUE)
                            fits <- fits1 <- fits2 <- fits3 <- fits4 <- NULL
                        }
                      fits <- res[, lapply(.SD, sum),
                                     keyby = c(keynames, "id_nams", "nams"),
                                    .SDcols = nosv]
                     return(fits)
                })
            rbindlist(fits)
         })
       rbindlist(fitd)
     })
   res <- rbindlist(fit)

   set(res, j = country, value = as.character(res[[country]]))

   if (!is.null(Dom)) {
          var_gradn[, paste0(Dom, "_ss"):=lapply(Dom, function(x) paste0(x,".", get(x)))]
          var_gradn[, nameYs:=Reduce(function(x, y)
                                      paste(x, y, sep = "__"), .SD),
                                     .SDcols=c("namesY", paste0(Dom, "_ss"))]
          if (!is.null(namesZ)) { var_gradn[, nameZs := Reduce(function(x, y)
                                                                 paste(x, y, sep = "__"), .SD),
                                                               .SDcols = c("namesZ", paste0(Dom, "_ss"))]
                                }
       } else { var_gradn[, nameYs := namesY]
                if (!is.null(namesZ)) var_gradn[, nameZs := namesZ]}

   nameYZ <- c("nameYs", "nameZs")
   nameYZ <- nameYZ[nameYZ %in% names(res)]

   sars <- c(country, "ind", paste0(per, "_1"),
             paste0(per, "_2"), nameYZ)

   data <- merge(res, var_gradn, all = TRUE, by = c(sars, "id_nams", "nams"))
   res <- fit <- var_gradn <- NULL

   rmax <- max(data[, .N, by = "ids_nr"][["ids_nr"]])

   nosv <- c("num1", "den1", "num2", "den2")
   nosv <- names(data)[names(data) %in% nosv]


   dat <- lapply(1:rmax, function(i) {
             res <- data[get("ids_nr") == i] 

             res1 <- as.matrix(res[, nosv, with = FALSE])
             rhod <- diag(sqrt(1 / diag(res1)), length(nosv), length(nosv))
             rhod <- data.table((t(rhod) %*% res1) %*% rhod)

             setnames(rhod, names(rhod), paste0("rho_", nosv))
             dmatr <- diag(sqrt(res[["cros_var"]] / diag(res1)),
                                length(nosv), length(nosv))
             var_tau <- data.table((t(dmatr) %*% res1) %*% dmatr)
             dmatr <- data.table(dmatr)
             setnames(dmatr, names(dmatr), paste0("d_", nosv))
             setnames(var_tau, names(var_tau), paste0("var_tau_", nosv))
             res <- data.table(res, rhod, dmatr, var_tau)

             var_t <- (t(res[["grad"]]) %*% as.matrix(var_tau)) %*% res[["grad"]]
             var_t <- var_t[, 1]
             var_grads <- var_grad[get("ids_nr") == i]

             if (change_type == "absolute") {
                          var_grads[, estim := estim_2 - estim_1]
                     } else var_grads[, estim := estim_2 / estim_1 * percentratio]

             var_grads[, var := var_t]
             list(matricas = res, data = var_grads) })

   matricas <- rbindlist(lapply(dat, function(x) x[[1]]))
   datas <- rbindlist(lapply(dat, function(x) x[[2]]))

   if (change_type == "relative" | (!is.null(datas$namesZ) & !linratio)) {
                  datas[, var:=var * (percentratio)^2] }

   datas[var >= 0, se := sqrt(var)]
   tsad <- qnorm(0.5 * (1 + confidence))
   datas[, CI_lower := estim - tsad * se]
   datas[, CI_upper := estim + tsad * se]

   sarc <- c(sarc, "nams")
   sarc <- sarc[!(sarc %in% "ind")]

   rho_matrix <- matricas[, c(sarc, paste0("rho_", nosv)), with = FALSE]
   var_tau <- matricas[, c(sarc, paste0("var_tau_", nosv)), with = FALSE]
   grad_var <- matricas[, c(sarc, "grad", "cros_var"), with = FALSE]

   namesYZ <- c("namesY", "namesZ")
   namesYZ <- names(datas)[(names(datas) %in% namesYZ)]


   changes_results <- datas[, c(paste0(per,"_", c(1, 2)), country, Dom,
                                namesYZ, "estim_1",  "estim_2", "estim",
                                "var", "se", "CI_lower", "CI_upper"), with = FALSE]

   changes_results[, significant := TRUE]
   boundss <- as.numeric(change_type == "relative")
   changes_results[CI_lower <= boundss & CI_upper >= boundss, significant := FALSE]

   if (is.null(names_country)) {
            cros_var_grad[, percoun := NULL]
            grad_var[, percoun := NULL]
            rho_matrix[, percoun := NULL]
            var_tau[, percoun := NULL]
            changes_results[, percoun := NULL]  }

   list(cros_var_grad = cros_var_grad,
        grad_var = grad_var,
        rho_matrix = rho_matrix,
        var_tau = var_tau,
        changes_results = changes_results)
 } 