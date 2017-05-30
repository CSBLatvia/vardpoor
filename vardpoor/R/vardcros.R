
vardcros <- function(Y, H, PSU, w_final,
                     ID_level1,
                     ID_level2,
                     Dom = NULL,
                     Z = NULL,
                     gender = NULL,
                     country = NULL,
                     period,
                     dataset = NULL,
                     X = NULL,
                     countryX = NULL,
                     periodX = NULL,
                     X_ID_level1 = NULL,
                     ind_gr = NULL,
                     g = NULL,
                     q = NULL,
                     datasetX = NULL,
                     linratio = FALSE,
                     percentratio=1,
                     use.estVar = FALSE,
                     ID_level1_max = TRUE,
                     outp_res = FALSE,
                     withperiod = TRUE,
                     netchanges = TRUE,
                     confidence = .95,
                     checking = TRUE) {

  ### Checking
  if (checking) {
    percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger") 
    linratio <- check_var(vars = linratio, varn = "linratio", varntype = "logical") 
    netchanges <- check_var(vars = netchanges, varn = "netchanges", varntype = "logical") 
    withperiod <- check_var(vars = withperiod, varn = "withperiod", varntype = "logical") 
    use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical") 
    ID_level1_max <- check_var(vars = ID_level1_max, varn = "ID_level1_max", varntype = "logical") 
    outp_res <- check_var(vars = outp_res, varn = "outp_res", varntype = "logical") 
    confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 

    if (all(ID_level1_max, !is.null(X))) stop("'ID_level1_max' must be ", !ID_level1_max, "!", call. = FALSE)
    if (all(!is.null(Z), !is.null(X), !linratio)) stop("'linratio' must be TRUE", call. = FALSE)
    if (all(!is.null(gender), !is.null(Z), !linratio)) stop("'linratio' must be TRUE", call. = FALSE)
    if (all(is.null(Z), linratio)) stop("'linratio' must be FALSE", call. = FALSE)

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
                        dataset = dataset, Ynrow = Ynrow,
                        ischaracter = TRUE, duplicatednames = TRUE,
                        withperiod = withperiod,
                        dif_name = c("percoun", "period_country", names(country)))

    ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                           dataset = dataset, ncols = 1, Yncol = 0,
                           Ynrow = Ynrow, ischaracter = TRUE)

    ID_level12 <- check_var(vars = ID_level2, varn = "ID_level2",
                           dataset = dataset, ncols = 1, Yncol = 0,
                           Ynrow = Ynrow, ischaracter = TRUE,
                           namesID1 = names(ID_level1), country = country,
                           periods = period)

    PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                     ncols = 1, Yncol = 0, Ynrow = Ynrow,
                     ischaracter = TRUE, namesID1 = names(ID_level1))


    if(!is.null(X)) {
        X <- check_var(vars = X, varn = "X", dataset = datasetX,
                       check.names = TRUE, isnumeric = TRUE,
                       grepls = "__",
                       dif_name = c(names(period), names(country), names(H),
                                    names(PSU), names(ID_level1), names(Y),
                                    "w_final", "w_design", "g", "q"))
        Xnrow <- nrow(X)


        ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                            dataset = datasetX, ncols = 1,
                            Xnrow = Xnrow, ischaracter = TRUE,
                            dif_name = c(names(period), names(country), names(H),
                                         names(PSU), names(ID_level1), names(Y),
                                         names(X), "w_final", "w_design", "g", "q"))

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
                             dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                             ischaracter = TRUE, mustbedefined = !is.null(period),
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
  dataset <- datasetX <- NULL


  # Calculation

  sar_nr <- N <- nameY <- nameZ <- variable <- NULL
  sample_size <- totalY <- totalZ <- Z1 <- percoun <- NULL
  totalY_male <- totalZ_male <- totalY_female <- NULL
  totalZ_female <- gender2 <- i <- NULL  

  # Design weights
  if (!is.null(X)) {
             idh <- data.table(ID_level1)
             idhx <- data.table(X_ID_level1)
             if (!is.null(countryX)) {idh <- data.table(country, idh)
                                      idhx <- data.table(countryX, idhx)}
             if (!is.null(periodX)) {idh <- data.table(period, idh)
                                     idhx <- data.table(periodX, idhx)}
             idhx <- data.table(idhx, g)
             setnames(idhx, names(idhx)[c(1 : (ncol(idhx)-1))], names(idh))
             idg <- merge(idh, idhx,  by = names(idh), sort = FALSE)
             w_design <- w_final / idg[[ncol(idg)]]
             idg <- data.table(idg, w_design = w_design)
             idh <- idg[, .N, keyby = c(names(idh), "w_design")]
             if (nrow(X) != nrow(idh))  stop("Aggregated 'w_design' length must the same as matrix 'X'")
             idg <- idhx <- idh <- NULL
      } else w_design <- w_final


  # Domains
  size <- data.table(size = rep(1, nrow(Y)))
  if (!is.null(Dom)) { size1 <- domain(Y = size, D = Dom,
                                       dataset = NULL,
                                       checking = FALSE) 
                       Y1 <- domain(Y = Y, D = Dom, 
                                    dataset = NULL,
                                    checking = FALSE) 
                 } else { size1 <- copy(size)
                          Y1 <- Y }

  namesDom <- names(Dom)
  if (!is.null(country)) { DTp <- data.table(country)
                        } else DTp <- data.table(percoun = rep("1", nrow(size)))
  if (withperiod) DTp <- data.table(period, DTp)
  namesperc <- names(DTp)
  namesperc2 <- c("period_country", namesperc)
  period_country <- do.call("paste", c(as.list(DTp), sep = "_"))

  if (!is.null(Z)) {
       if (!is.null(Dom)) Z1 <- domain(Y = Z, D = Dom, 
                                       dataset = NULL,
                                       checking = FALSE) else Z1 <- Z

       if (linratio){
                   sorts <- unlist(split(Y1[, .I], period_country))
                   lin1 <- lapply(split(Y1[, .I], period_country),
                                 function(i)  
                                      if (!is.null(gender)) {
                                                data.table(sar_nr = i,
                                                           lin.ratio(Y = Y1[i] * (gender == 1),
                                                                     Z = Z1[i] * (gender == 1),
                                                                     weight = w_final[i],
                                                                     Dom = NULL, dataset = NULL,
                                                                     percentratio = percentratio,
                                                                     checking = FALSE)-
                                                           lin.ratio(Y = Y1[i] * (gender == 2),
                                                                     Z = Z1[i] * (gender == 2),
                                                                     weight = w_final[i],
                                                                     Dom = NULL, dataset = NULL,
                                                                     percentratio = percentratio,
                                                                     checking = FALSE))
                                         } else { data.table(sar_nr = i,
                                                             lin.ratio(Y = Y1[i], Z = Z1[i],
                                                                       weight = w_final[i],
                                                                       Dom = NULL, dataset = NULL,
                                                                       percentratio = percentratio,
                                                                       checking = FALSE))})
                   Y2 <- rbindlist(lin1)
                   setkeyv(Y2, "sar_nr")

                   Y2[, sar_nr := NULL]
                   if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
                  } else Y2 <- data.table(copy(Y1), copy(Z1))
   } else if (!is.null(gender)) { Y2 <- Y1[i] * (gender == 1) - Y1[i] * (gender == 2)
                   } else Y2 <- copy(Y1)

   namesY2 <- names(Y2)
   namesY2w <- paste0(namesY2, "w")
   namesY <- names(Y)
   namesZ <- names(Z)
   names_H <- names(H)
   namesY1 <- names(Y1)
   namesZ1 <- names(Z1)
   names_id1 <- names(ID_level1)
   names_id2 <- names(ID_level2)
   names_PSU <- names(PSU)
   names_size1 <- names(size1)
   namesYZ <- c(namesY, namesZ)
   namesY1Z1 <- c(namesY1, namesZ1)
   names_country <- names(country)
   names_size1w <- paste0(names_size1, "w")

   size1w <- size1 * w_final
   setnames(size1w, names_size1, names_size1w)

   DT <- data.table(period_country, DTp, H, PSU, ID_level1, ID_level2,
                    w_final, w_design, size1, size1w, Y2)

   DTc <- DT[, lapply(.SD, sum, na.rm = TRUE),
                            by = c(namesperc2, names_H, names_PSU,
                                   names_id1, "w_final", "w_design"),
                          .SDcols = c(names_size1, names_size1w, namesY2)]

   H <- PSU <- id <- DTp <- country <- NULL
   DTagg <- data.table(DT[, namesperc,  with = FALSE], w_final)
   if (!is.null(Dom)) DTagg <- data.table(DTagg, Dom)
   DTagg <- data.table(DTagg, sample_size = 1,
                       pop_size = w_final, w_final * Y)
   if (!is.null(gender)) DTagg <- data.table(DTagg, gender)
   if (!is.null(Z)) DTagg <- data.table(DTagg, w_final * Z)

   gnamesDom <- namesDom
   if (!is.null(gender)) gnamesDom <- c("gender", gnamesDom)
   DTaggs <- DTagg[, lapply(.SD, sum, na.rm = TRUE),
                            keyby = c(namesperc, namesDom),
                          .SDcols = c("sample_size", "pop_size")]

   DTagg <- DTagg[, lapply(.SD, sum, na.rm = TRUE),
                            keyby = c(namesperc, gnamesDom),
                           .SDcols = namesYZ]

   vars <- data.table(variable = namesY, namesY = namesY)
   if (!is.null(namesZ)) vars <- data.table(variable = as.character(1 : length(namesY)),
                                            namesY = namesY, namesZ = namesZ)

   varsYZ <- list(namesY)
   if (!is.null(namesZ)) varsYZ <- list(namesY, namesZ)
   DTagg <- melt(DTagg, id = c(namesperc, gnamesDom),
                        measure = varsYZ,
                        variable.factor = FALSE)

   setnames(DTagg, ifelse(!is.null(DTagg$value1), "value1", "value"), "totalY")
   totYZ <- "totalY"
   if (!is.null(Z)) {totYZ <- c(totYZ, "totalZ")
                     setnames(DTagg, "value2", "totalZ")}

   if (!is.null(gender)) {
           funkc <- as.formula(paste0(paste(c(namesperc, namesDom, "variable"), collapse= "+"), "~ gender2"))
           DTagg[gender == 1, gender2 := "male"]
           DTagg[gender == 2, gender2 := "female"]
           DTagg <- dcast(DTagg, funkc, sum, value.var = totYZ)
         }

   DTagg <- merge(DTagg, vars,  by = "variable")[, variable := NULL]

   DTagg <- merge(DTagg, DTaggs, all.x = TRUE,
                                 by = c(namesperc, namesDom))

   if (!is.null(namesDom)) DTagg[,(paste0(namesDom, "_new")) := lapply(namesDom,
                      function(x) make.names(paste0(x,".", get(x))))]

    varsYZ <- vars <- nameY1 <- nameZ1 <- valueY1 <- valueZ1 <- Dom <- NULL
    Z1 <- Y1 <- period_country <- Y2 <- total <- pop_size <- NULL
    stderr_nw <- nhcor <- num1 <- num <- den1 <- den <- num_den1 <- NULL
    grad1 <- grad2 <- estim <- sd_nw <- stderr_w <- sd_w <- se <- rse <- NULL
    cv <- CI_lower <- absolute_margin_of_error <- CI_upper <- totalZ <- NULL
    relative_margin_of_error <- NULL


    # Calibration
    res_outp <- NULL
    if (!is.null(X)) {
         X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
         if (!is.null(countryX)) X0 <- data.table(countryX, X0)
         if (!is.null(periodX)) X0 <- data.table(periodX, X0)
         nos <- c(names(periodX), names(countryX), names(ID_level1))
         DT1 <- merge(DTc, X0, by = nos, sort = FALSE)

         ind_gr <- DT1[, c(namesperc, names(ind_gr)), with = FALSE]
         ind_period <- do.call("paste", c(as.list(ind_gr), sep = "_"))

         res <- lapply(split(DT1[, .I], ind_period), function(i)
                        data.table(DT1[i, nos, with = FALSE],
                                   res <- residual_est(Y = DT1[i, namesY2, with = FALSE],
                                                       X = DT1[i, names(X), with = FALSE],
                                                       weight = DT1[i][["w_design"]],
                                                       q = DT1[i][["q"]], dataset = NULL,
                                                       checking = FALSE)))
         res <- rbindlist(res)
         setnames(res, namesY2, namesY2w)
         DTc <- merge(DTc, res, by = nos)
         if (outp_res) res_outp <- DTc[, c(nos, names_PSU, "w_final", namesY2w), with = FALSE]
     } else DTc[, (namesY2w) := .SD[, namesY2, with = FALSE]]


   DTc[, (namesY2w) := .SD[, namesY2w, with = FALSE] * get("w_final")]

   #--------------------------------------------------------*
   # AGGREGATION AT PSU LEVEL ("ULTIMATE CLUSTER" APPROACH) |
   #--------------------------------------------------------*

   DT1 <- DTc[, lapply(.SD, sum, na.rm = TRUE), keyby = c(namesperc2,
                           names_H, names_PSU), .SDcols = namesY2w]
   setnames(DT1, namesY2w, namesY2)

   DTnet <- copy(DT1)
   if (!netchanges)  DTnet <- NULL

   DT2 <- DT1[, lapply(.SD, sum, na.rm = TRUE),
                        keyby = namesperc, .SDcols = namesY2]
   varsYZ <- list(namesY1)
   if (!is.null(namesZ1) & !linratio) varsYZ <- list(namesY1, namesZ1)

   DT2 <- melt(DT2, id = namesperc,
                    measure = varsYZ,
                    variable.factor = FALSE)
   if (!is.null(namesZ1) & !linratio) {setnames(DT2, c("value1", "value2"),
                                                     c("valueY1", "valueZ1"))
                     } else setnames(DT2, ifelse(!is.null(DT2$value1), "value1", "value"), "valueY1")

   if (!is.null(namesZ1) & !linratio) {
                    vars <- data.table(variable = 1 : length(namesY1))
                        } else vars <- data.table(variable = namesY1)

   if (!is.null(namesDom)) { vars <- data.table(vars, nameY1 = namesY1,
                                               t(data.frame(strsplit(namesY1, "__"))))
                            setnames(vars, names(vars)[3 : length(vars)],
                                        c("namesY", paste0(namesDom, "_new")))
                     } else {vars <- data.table(vars, nameY1 = namesY1, namesY = namesY1) }

   if (!is.null(namesZ1)) { vars <- data.table(vars, nameZ1 = namesZ1)
                            if (!is.null(namesDom)) {
                                      varsZ <- data.table(nameZ1 = namesZ1,
                                                          t(data.frame(strsplit(namesZ1, "__"))))
                                      setnames(varsZ, names(varsZ)[2 : length(varsZ)],
                                                    c("namesZ", paste0(namesDom, "_new")))
                                      varsZ[, (paste0(namesDom, "_new")) := NULL]
                                      vars <- merge(vars, varsZ,  by = "nameZ1")
                               } else vars[, namesZ := nameZ1]  }

  vars <- vars[, lapply(vars, as.character)]

  DT2 <- merge(DT2, vars, by = "variable")
  DT2[, variable := NULL]
  vars <- varsZ <- NULL

  vars <- c(namesperc, paste0(namesDom, "_new"), "namesY", "namesZ")
  vars <- names(DT2)[names(DT2) %in% vars]

  DTagg <- merge(DTagg, DT2,  by = vars)
  DT2 <- vars <- NULL


  # VECTOR OF THE PARTIAL DERIVATIVES (GRADIENT FUNCTION)

  if (!is.null(namesZ1) & !linratio) {
      DTagg[, grad1 := 1 / valueZ1]
      DTagg[, grad2 := - valueY1 / valueZ1^2]
  }

  # NUMBER OF PSUs PER STRATUM
  setkeyv(DT1, c(namesperc2, names_H))

  stratasf <- nh <- nhcor <- NULL
  DT1[, nh := .N, by = c(namesperc2, names_H)]



 #--------------------------------------------------------------------------*
 # MULTIVARIATE REGRESSION APPROACH USING STRATUM DUMMIES AS REGRESSORS AND |
 # STANDARD ERROR ESTIMATION 						      |
 #--------------------------------------------------------------------------*

  DT1[, (names_H) := as.factor(get(names_H))]
  DT1[, paste0(names_H, "_", levels(get(names_H)))] -> DT1H
  DT1[, (DT1H) := transpose(lapply(get(names_H), FUN = function(x){as.numeric(x == levels(get(names_H)))})) ]

  fits <-lapply(1 : length(namesY1), function(i) {
           fitss <- lapply(split(DT1, DT1$period_country), function(DT1c) {

                        y <- namesY1[i]
                        if ((!is.null(namesZ1))&(!linratio)) z <- paste0(",", toString(namesZ1[i])) else z <- ""

                        funkc <- as.formula(paste("cbind(", trim(toString(y)), z, ")~ 0 + ",
                                       paste(c(0, DT1H), collapse= "+")))

                        res1 <- lm(funkc, data = DT1c)
                        if (use.estVar == TRUE) {res1 <- data.table(crossprod(res1$res))
                                } else res1 <- data.table(res1$res)
                        setnames(res1, names(res1)[1], "num")
                        res1[, nameY1 := y]
                        if (!is.null(namesZ1) & !linratio) {
                              setnames(res1, names(res1)[2], "den")
                              res1[, nameZ1 := namesZ1[i]]
                            }

                        if (use.estVar == TRUE) {
                              setnames(res1, "num", "num1")
                              if (!is.null(namesZ1) & !linratio) {
                                       res1[, num_den1 := res1[["den"]][1]]
                                       res1[, den1 := res1[["den"]][2]] }
                              res1 <- data.table(res1[1], DT1c[1])
                          } else {
                              res1 <- data.table(res1, DT1c)
                              res1[, nhcor := ifelse(nh > 1, nh / (nh - 1), 1)]
                              res1[, num1 := nhcor * num * num]
                              if (!is.null(namesZ1) & !linratio) {
                                   res1[, num_den1 := nhcor * num * den]
                                   res1[, den1 := nhcor * den * den]
                               }}
                         namep <- c("nameY1", "nameZ1")
                         namep <- namep[namep %in% names(res1)]
                         varsp <- c("num1", "den1", "num_den1")
                         varsp <- varsp[varsp %in% names(res1)]

                        fits <- res1[, lapply(.SD, sum),
                                       keyby = c("period_country",
                                               namesperc, namep),
                                       .SDcols = varsp]
                        return(fits)
                    })
            return(rbindlist(fitss))
        })
  res <- rbindlist(fits)
  DT1 <- fits <- DT1H <- NULL

  vars <- c(namesperc, namesDom, "nameY1", "nameZ1")
  vars <- names(res)[names(res) %in% vars]

  res <- merge(DTagg, res, by = vars)

  DTagg <- total <- NULL

  res[, var := num1]

  if (!is.null(gender)) {
           res[, estim := totalY_male - totalY_female]
           if (!is.null(res$totalZ)) res[, estim := (totalY_male / totalZ_male - totalY_female / totalZ_female) * percentratio]
          } else {
           res[, estim := totalY]
           if (!is.null(res$totalZ)) res[, estim := totalY / totalZ * percentratio] }

  if (!is.null(res$totalZ) & !linratio) {
                    res[, var :=  (grad1 * grad1 * num1) +
                                  (grad2 * grad2 * den1) +
                              2 * (grad1 * grad2 * num_den1)]
                    res[, var := var * (percentratio)^2] }

  main <- c(namesperc, namesDom, "namesY", "nameY1")
  if (!is.null(namesDom)) main <- c(main, paste0(namesDom, "_new"))
  if (!is.null(res$namesZ)) main <- c(main, "namesZ", "nameZ1")
  main <- c(main, "sample_size", "pop_size")
  if (is.null(gender)) { main <- c(main, "totalY")
           } else main <- c(main, c("totalY_male", "totalY_female"))
  if (!is.null(res$namesZ)) { if (is.null(gender)) { main <- c(main, "totalZ")
                                             } else main <- c(main, c("totalZ_male", "totalZ_female")) }
  main2 <- c(main, "estim", "valueY1")
  if (!is.null(namesZ1) & !linratio) main2 <- c(main2, "valueZ1")
  main2 <- c(main2, "num1")
  if (!is.null(namesZ1) & !linratio) main2 <- c(main2, "den1", "grad1", "grad2")

  if (netchanges) { res1 <- res[, main2[!(main2 %in% c("sample_size",
                                                       "pop_size", "nameY1",
                                                       paste0(namesDom, "_new"),
                                                       "nameZ1"))], with = FALSE]
                                                     } else res1 <- NULL

  main <- c(main, "estim", "var")
  res22 <- res[, main, with = FALSE]

  #-------------------------------------------------------------------------*
  # DESIGN EFFECT (DEFF) ESTIMATION - VARIANCE UNDER SIMPLE RANDOM SAMPLING |
  #-------------------------------------------------------------------------*

  # We aggregate the target variables at household level

  DTs <- DT[, lapply(.SD, sum, na.rm = TRUE),
                          keyby = c(namesperc2, names_id1, "w_final"),
                         .SDcols = c(names_size1, names_size1w, namesY2)]
  if (ID_level1_max) {
           DTm <- DT[, lapply(.SD, max, na.rm = TRUE), keyby = c(namesperc2, names_id1), .SDcols = names_size1]
       } else {
          DTm <- DT[, lapply(.SD, sum, na.rm = TRUE), keyby = c(namesperc2, names_id1), .SDcols = names_size1]
     }

  setnames(DTm, names_size1, paste0(names_size1, "m"))
  DTs <- merge(DTs, DTm, by = c(namesperc2, names_id1))

  # Linearised variables

  if (!is.null(namesZ1) & !linratio) {
                   lin1 <- lapply(split(DTs[, .I], DTs$period_country), function(i)
                                lin.ratio(Y = DTs[i, namesY1,  with = FALSE],
                                          Z = DTs[i, namesZ1,  with = FALSE],
                                          weight = DTs[["w_final"]][i], Dom = NULL,
                                          percentratio = percentratio))
                   Y2a <- rbindlist(lin1)
                   setnames(Y2a, names(Y2a), paste0("lin___", namesY1))
                   DTs <- data.table(DTs, Y2a)
                   Y2a <- paste0("lin___", namesY1)
               } else Y2a <- namesY1

  w_final <- DTs[["w_final"]]
  DTsd <- DTs[, lapply(.SD[, Y2a, with = FALSE], function(x)
                       sum(w_final*((x-sum(w_final*x)/sum(w_final))^2))/(sum(w_final)-1)),
                       keyby = "period_country"]

  setnames(DTsd, Y2a, paste0("sd_w__", namesY1))
  DTs <- merge(DTs, DTsd, by = "period_country")

  DTm <- DTs[, lapply(.SD[, paste0(names_size1, "m"), with = FALSE], function(x) sum(w_final * x, na.rm = TRUE)),
                       keyby = "period_country"]
  setnames(DTm, paste0(names_size1, "m"), paste0("pop_", names_size1))
  DTs <- merge(DTs, DTm, by = "period_country")

  DTsd <- DTs[, lapply(.SD, sd, na.rm = TRUE), keyby = "period_country", .SDcols = Y2a]
  setnames(DTsd, Y2a, paste0("sd_nw__", namesY1))
  DTs <- merge(DTs, DTsd, by = "period_country")

  DTm <- DTs[, lapply(.SD, sum, na.rm = TRUE), keyby = "period_country", .SDcols = names_size1]
  setnames(DTm, names_size1, paste0("samp_", names_size1))
  DTs <- merge(DTs, DTm, by = "period_country")

  DTx <- DTs[, .N, keyby = c(namesperc, paste0("sd_w__", namesY1),
                               paste0("sd_nw__", namesY1),
                               paste0("pop_", names_size1),
                               paste0("samp_", names_size1))]
  DTx[, N := NULL]

  main <- melt(DTx[, c(namesperc, paste0("sd_w__", namesY1)), with = FALSE], id = namesperc)
  main[, nameY1 := substr(variable, 7, nchar(trim(as.character(variable))))]
  main[, variable := NULL]
  setnames(main, "value", "sd_w")
  res <- merge(res, main, all.x = TRUE, by = c(namesperc, "nameY1"))

  main <- melt(DTx[, c(namesperc, paste0("sd_nw__", namesY1)), with = FALSE], id = namesperc)
  main[, nameY1 := substr(variable, 8, nchar(trim(as.character(variable))))]
  main[, variable := NULL]
  setnames(main, "value", "sd_nw")
  res <- merge(res, main, all = TRUE, by = c(namesperc, "nameY1"))

  main <- melt(DTx[, c(namesperc, paste0("pop_", names_size1)), with = FALSE], id = namesperc)
  if (!is.null(namesDom)){
                   main[, Dom := substr(variable, 11, nchar(trim(as.character(variable))))]
                   vars <- unique(main[["Dom"]])
                   vars <- data.table(Dom=vars, t(data.frame(strsplit(vars, "__"))))
                   setnames(vars, names(vars)[2 : length(vars)], paste0(namesDom, "_new"))
                   main <- merge(main, vars, all.x = TRUE,  by = "Dom")  }
  main[, variable := NULL]
  setnames(main, "value", "pop")
  nds <- namesperc
  if (!is.null(namesDom)) nds <- c(namesperc, paste0(namesDom, "_new"))
  res <- merge(res, main, all.x = TRUE, by = nds)

  main <- melt(DTx[, c(namesperc, paste0("samp_", names_size1)), with = FALSE], id = namesperc)
  if (!is.null(namesDom)) main[, Dom := substr(variable, 12, nchar(trim(as.character(variable))))]
  main[, variable := NULL]
  setnames(main, "value", "sampl_siz")
  if (is.null(namesDom)) nds <- namesperc else nds <- c(namesperc, "Dom")
  res <- merge(res, main, all = TRUE, by = nds)

  res[sample_size < pop_size, stderr_nw := 100 * sqrt((1 - (sample_size / pop_size)) / pop_size * sd_nw * sd_nw / sample_size)]
  res[sample_size < pop_size, stderr_w := 100 * sqrt((1 - (sample_size / pop_size)) / pop_size * sd_w * sd_w / sample_size)]

  DT <- DTw <- DTx <- DTs <- DTsd <- sd1 <- nds <- NULL

  res[, se := sqrt(var)]
  res[, rse := se / estim]
  res[, cv := rse * 100]
  tsad <- qnorm(0.5 * (1 + confidence))
  res[, absolute_margin_of_error := tsad * se]
  res[, relative_margin_of_error := tsad * cv]
  res[, CI_lower := estim - tsad * se]
  res[, CI_upper := estim + tsad * se]

  main <- namesperc
  if (!is.null(namesDom))  main <- c(main, namesDom)
  main <- c(main, "namesY")
  if (!is.null(res$namesZ)) main <- c(main, "namesZ")

  main <- c(main, "sample_size", "pop_size", "estim", "se",
            "var", "rse", "cv", "absolute_margin_of_error",
            "relative_margin_of_error", "CI_lower", "CI_upper",
            "sd_w", "sd_nw", "pop", "sampl_siz", "stderr_nw",
            "stderr_w")

  main <- main[main %in% names(res)]
  res <- res[, main,  with = FALSE]
  if (!netchanges & is.null(names_country)) {
      if (!is.null(DTnet)) DTnet[, percoun := NULL]
      res1[, percoun := NULL]
      res[, percoun := NULL]  }
  list(data_net_changes = DTnet, res_out = res_outp, var_grad = res1, results = res)

}



