
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
                         percentage = 60, order_quant = 50L,
                         alpha = 20, use.estVar = FALSE,
                         withperiod = TRUE, netchanges = TRUE,
                         confidence = .95, outp_lin = FALSE,
                         outp_res = FALSE, type = "linrmpg",
                         checking = TRUE) {
  ### Checking

  all_choices <- c("linarpr", "linarpt", "lingpg",
                   "linpoormed", "linrmpg", "lingini",
                   "lingini2", "linqsr", "linrmir", "linarr")
  type <- tolower(type)
  type <- match.arg(type, all_choices, length(type)>1) 

  percentage <- check_var(vars = percentage, varn = "percentage", varntype = "numeric0100") 
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "integer0100") 
  alpha <- check_var(vars = alpha, varn = "alpha", varntype = "numeric0100") 
  netchanges <- check_var(vars = netchanges, varn = "netchanges", varntype = "logical") 
  withperiod <- check_var(vars = withperiod, varn = "withperiod", varntype = "logical") 
  use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical") 
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 

  if (checking){
         if(!is.null(X)) {
              if (is.null(datasetX)) datasetX <- copy(dataset)
              equal_dataset <- identical(dataset, datasetX) & !is.null(dataset)
              if (equal_dataset) { X_ID_level1 <- ID_level1
                                   countryX <- country }}
    
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
                            mustbedefined = any(type == "linarr"))
    
         Y_thres <- check_var(vars = Y_thres, varn = "Y_thres",
                              dataset = dataset, ncols = 1,
                              Ynrow = Ynrow, mustbedefined = FALSE,
                              isnumeric = TRUE, isvector = TRUE)
    
         wght_thres <- check_var(vars = wght_thres, varn = "wght_thres",
                                 dataset = dataset, ncols = 1,
                                 Ynrow = Ynrow, mustbedefined = FALSE,
                                 isnumeric = TRUE, isvector = TRUE)
    
         H <- check_var(vars = H, varn = "H", dataset = dataset,
                        ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                        dif_name = "dataH_stratas")
    
         sort <- check_var(vars = sort, varn = "sort",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, mustbedefined = FALSE,
                           isnumeric = TRUE, isvector = TRUE)
    
         Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                          Ynrow = Ynrow, ischaracter = TRUE,
                          mustbedefined = FALSE, duplicatednames = TRUE,
                          grepls = "__")
    
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
         
         ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                                 dataset = dataset, ncols = 1, Ynrow = Ynrow,
                                 ischaracter = TRUE, namesID1 = names(ID_level1),
                                 country = country, periods = period)
         
         PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                          ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                          namesID1 = names(ID_level1))
         
        if(!is.null(X)) {
              X <- check_var(vars = X, varn = "X", dataset = datasetX,
                             check.names = TRUE, isnumeric = TRUE,
                             dif_name = c(names(period), names(country), names(H),
                                           names(PSU), names(ID_level1), "w_final",
                                           "w_design", "g", "q"))
              Xnrow <- nrow(X)
           
           
              ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                                 dataset = datasetX, ncols = 1,
                                 Xnrow = Xnrow, ischaracter = TRUE,
                                 dif_name = c(names(period), names(country), names(H),
                                              names(PSU), names(ID_level1), "w_final",
                                              names(X), "w_design", "g", "q"))
           
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
  
  if (is.null(Y_thres)) Y_thres <- Y
  if (is.null(wght_thres)) wght_thres <- w_final
  namesDom <- names(Dom)  

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
  if (!is.null(Dom)) size <- data.table(size, Dom)
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
                         var_name = "lin_arpt", checking = FALSE)
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
                         order_quant = order_quant, var_name = "lin_arpr",
                         checking = FALSE)
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
                        var_name = "lin_gpg", checking = FALSE)
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
                             order_quant = order_quant, var_name = "lin_poormed",
                             checking = FALSE)
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
                          order_quant = order_quant, var_name = "lin_rmpg",
                          checking = FALSE)
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
                       dataset = NULL, alpha = alpha, var_name = "lin_qsr",
                       checking = FALSE) 
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
                         dataset = NULL, var_name = "lin_gini",
                         checking = FALSE)
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
                           dataset = NULL, var_name = "lin_gini2",
                           checking = FALSE)
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
                         order_quant = order_quant, var_name = "lin_rmir",
                         checking = FALSE) 
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
                      order_quant = order_quant, var_name = "lin_arr",
                      checking = FALSE) 

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
        X0 <- data.table(X_ID_level1, ind_gr, q, g, X)
        if (!is.null(countryX)) X0 <- data.table(countryX, X0)
        if (!is.null(periodX)) X0 <- data.table(periodX, X0)
        nos <- c(names(periodX), names(countryX), names(ID_level1))
        DT1 <- merge(DTc, X0, by = nos)
        DT1[, w_design := w_final / g ]

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

   DTY2[, (names_H) := as.factor(get(names_H))]
   DTY2[, paste0(names_H, "_", levels(get(names_H)))] -> DTY2H
   DTY2[, (DTY2H) := transpose(lapply(get(names_H), FUN = function(x){as.numeric(x == levels(get(names_H)))})) ]

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
