vardchangespoor <- function(Y, age = NULL,
                            pl085 = NULL,
                            month_at_work = NULL,
                            Y_den = NULL,
                            Y_thres = NULL,
                            wght_thres = NULL, 
                            H, PSU, w_final,
                            ID_level1, ID_level2, 
                            Dom = NULL, country = NULL,
                            period, sort = NULL,
                            period1, period2,
                            gender = NULL, dataset = NULL,
                            X = NULL, countryX = NULL,
                            periodX = NULL, X_ID_level1 = NULL,
                            ind_gr = NULL, g = NULL, q = NULL,
                            datasetX = NULL, percentage = 60,
                            order_quant = 50L, alpha = 20,
                            use.estVar = FALSE,
                            confidence = 0.95,
                            outp_lin = FALSE,
                            outp_res = FALSE,
                            type = "linrmpg",
                            change_type = "absolute") {
 
  ### Checking
  change_type <- check_var(vars = change_type, varn = "change_type", varntype = "change_type") 

  all_choices <- c("linarpr", "linarpt", "lingpg",
                   "linpoormed", "linrmpg", "lingini",
                   "lingini2", "linqsr", "linrmir", "linarr")
  type <- tolower(type)
  type <- match.arg(type, all_choices, length(type) > 1) 

  percentage <- check_var(vars = percentage, varn = "percentage", varntype = "numeric0100") 
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "integer0100") 
  alpha <- check_var(vars = alpha, varn = "alpha", varntype = "numeric0100") 
  use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical") 
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01") 

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
                 ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 ischaracter = TRUE, dif_name = "dataH_stratas")
  
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
                      dif_name = c("percoun", "period_country", names(country)))
  
  period1 <- check_var(vars = period1, varn = "period1", dataset = NULL,
                       ncols = 1, ischaracter = TRUE, periods = period)
  
  period2 <- check_var(vars = period2, varn = "period2", dataset = NULL,
                       ncols = 1, ischaracter = TRUE, periods = period)
  
  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Yncol = 0,
                         Ynrow = Ynrow, ischaracter = TRUE)
  
  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
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
                        dif_name = c(names(period), names(country), names(H),
                                     names(PSU), names(ID_level1), "w_final",
                                     "w_design", "g", "q"), dX = "X")
         Xnrow <- nrow(X)
    
         ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                             dataset = datasetX, ncols = 1,
                             Xnrow = Xnrow, ischaracter = TRUE, 
                             dif_name = c(names(period), names(country), names(H),
                                          names(PSU), names(ID_level1), "w_final",
                                          "w_design", "g", "q"), dX = "X")
    
         g <- check_var(vars = g, varn = "g", dataset = datasetX,
                        ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                        isvector = TRUE, dX = "X")
       
         q <- check_var(vars = q, varn = "q", dataset = datasetX,
                        ncols = 1, Xnrow = Xnrow, isnumeric = TRUE,
                        isvector = TRUE, dX = "X")
     
         countryX <- check_var(vars = countryX, varn = "countryX",
                               dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                               ischaracter = TRUE, mustbedefined = !is.null(country),
                               varnout = "country", varname = names(country),
                               country = country, dX = "X")
    
         periodX <- check_var(vars = periodX, varn = "periodX",
                              dataset = datasetX, ncols = 1, 
                              Xnrow = Xnrow, ischaracter = TRUE,
                              mustbedefined = !is.null(period),
                              duplicatednames = TRUE, varnout = "period",
                              varname = names(period), country = country,
                              countryX = countryX, periods = period, dX = "X")
    
         X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                  dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                  ischaracter = TRUE, varnout = "ID_level1",
                                  varname = names(ID_level1), country = country,
                                  countryX = countryX, periods = period,
                                  periodsX = periodX, ID_level1 = ID_level1, dX = "X")
     } 

  if (is.null(Y_thres)) Y_thres <- Y
  if (is.null(wght_thres)) wght_thres <- w_final

  data <- vardcrospoor(Y = Y, age = age, pl085 = pl085,
                       month_at_work = month_at_work,
                       Y_den = Y_den, Y_thres = Y_thres,
                       H = H, PSU = PSU, w_final = w_final,
                       ID_level1 = ID_level1, ID_level2 = ID_level2,
                       Dom = Dom, country = country,
                       period = period, sort = sort, 
                       gender = gender, dataset = NULL,
                       X = X, countryX = countryX,
                       periodX = periodX, X_ID_level1 = X_ID_level1,
                       ind_gr = ind_gr, g = g, q = q,
                       datasetX = NULL, percentage = percentage,
                       order_quant = order_quant,
                       alpha = alpha,
                       use.estVar = use.estVar,
                       withperiod = TRUE,
                       netchanges = TRUE,
                       confidence = confidence,
                       outp_lin = outp_lin,
                       outp_res = outp_res, 
                       type = type, checking = FALSE)

  cros_lin_out <- data$lin_out
  cros_res_out <- data$res_out
  data_res <- data$res_out

  crossectional_results <- data$results

  PSU <- names(PSU)
  Dom <- names(Dom)
  H <- names(H)
  Y <- type
  country <- names(country)
  per <- names(period)
  sar <- c(country, Dom, "type")
  sarp <- c(country, H, PSU)


  X <- countryX <- periodX <- X_ID_level1 <- ind_gr <- NULL
  ind_gr <- g <- q <- N <- w_final <- namesY <- ind <- NULL
  dataset <- rot <- rot_1 <- rot_2 <- stratasf <- name1 <- NULL
  num1 <- num1num1 <- num2num2 <- num1num2 <- num2 <- num1_1 <- NULL
  V12 <- grad1 <- grad2 <- num1_2 <- estim <- estim_1 <- nh <- NULL
  nhcor <- var_1 <- var_2 <- typs <- estim_2 <- se  <- NULL
  significant <- rho <- q_1 <- q_2 <- sum1 <- sum2 <- NULL

  var_grad <- copy(crossectional_results)
  var_grad <- var_grad[, c(per, sar, "estim", "var"), with = FALSE]
  period1[, ind := .I]
  period2[, ind := .I]
  per1 <- paste0(per, "_1")
  per2 <- paste0(per, "_2")  
  setnames(period1, per, per1)
  setnames(period2, per, per2)
  period1 <- merge(period1, period2, by = "ind")
  period2 <- NULL

  var_grad1 <- merge(period1, var_grad, all.x = TRUE,
                              by.x = per1, by.y = per,
                              allow.cartesian = TRUE)
  var_grad2 <- merge(period1, var_grad, all.x = TRUE,
                              by.x = per2, by.y = per,
                              allow.cartesian = TRUE)
  setnames(var_grad1, c("estim", "var"), paste0(c("estim", "var"), "_1"))
  setnames(var_grad2, c("estim", "var"), paste0(c("estim", "var"), "_2"))

  var_grad <- merge(var_grad1, var_grad2, all = TRUE, by = c("ind", per1, per2, sar))
  var_grad1 <- var_grad2 <- NULL

  data <- data.table(data$data_net_changes, check.names = TRUE)
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
  data1 <- data2 <- NULL

  recode.NA <- function(DT, cols = seq_len(ncol(DT))) {
     for (j in cols) if (is.numeric(DT[[j]]))
      set(DT, which(is.na(DT[[j]])), j, ifelse(is.integer(DT[[j]]), 0L, 0))
   }
  recode.NA(data, c(paste0(sard, "_1"), paste0(sard, "_2")))

  data[, (H) := as.factor(get(H))]
  data[, paste0(H, "_", levels(get(H)))] -> dataH
  data[, (dataH) := transpose(lapply(get(H), FUN = function(x){as.numeric(x == levels(get(H)))})) ]

  fit <- lapply(1 : (length(sard) - 1), function(i) {
         fitd <- lapply(split(data, data[["ind"]]), function(data1) {

                 fits <- lapply(split(data1, data1[[country]]), function(DT3c) {

                           y1 <- paste0(sard[i], "_1")
                           y2 <- paste0(sard[i], "_2")

                           funkc <- as.formula(paste0("cbind(", trim(toString(y1)), ", ", 
                                                                trim(toString(y2)), ") ~ -1 +",
                                                                paste(t(unlist(lapply(dataH, function(x) 
                                                                         paste0("rot_1 : ", toString(x), "+",
                                                                                "rot_2 : ", toString(x), "+",
                                                                                "rot_1 : rot_2 : ", toString(x))))),
                                                                                collapse= "+"))) 
                           res <- lm(funkc, data = DT3c)
                           ssumas <- DT3c[, .(sum1 = sum(get(y1)), sum2 = sum(get(y2)))]

                           if (use.estVar) { res <- data.table(estVar(res))
                                        } else res <- data.table(lm(funkc, data = DT3c)$res)
                           setnames(res, names(res), c("num1", "num2"))
                           res[, namesY := sard[i]]
                          
                           if (use.estVar) { 
                               res[, num1num1 := res[["num1"]][1]]
                               res[, num2num2 := res[["num2"]][2]]
                               res[, num1num2 := res[["num1"]][2]]
                               res <- data.table(res[1], DT3c[1])
                             } else {
                               res[, num1num1 := num1 * num1]
                               res[, num2num2 := num2 * num2]
                               res[, num1num2 := num1 * num2]
                               res <- data.table(res, DT3c)}

                           keynames <- c(country, "ind", paste0(per, "_1"), paste0(per, "_2"), "namesY")
                           fits <- res[, lapply(.SD, sum), keyby = keynames,
                                      .SDcols = c("num1num1", "num2num2", "num1num2")]
                           fits <- data.table(fits, ssumas)
                          return(fits)
                      })
               rbindlist(fits)
            })
            rbindlist(fitd)      
        })
   res <- rbindlist(fit)

   set(res, j = country, value = as.character(res[[country]]))

   var_grad[, namesY := paste0("lin_", tolower(type))]
   if (!is.null(Dom)) {
          var_grad[, paste0(Dom, "_ss") := lapply(Dom, function(x) make.names(paste0(x, ".", get(x))))]
          var_grad[, paste0(Dom[1], "_ss") := paste0(get(paste0(Dom[1], "_ss")))]
          var_grad[, typs:=paste0("lin_", tolower(type))]
          var_grad[, namesY:=Reduce(function(x, y)
                                      paste(x, y, sep = "__"), .SD),
                                     .SDcols = c("typs", paste0(Dom, "_ss"))]
         var_grad[, (c("typs", paste0(Dom, "_ss"))) := NULL]
       }

   data <- merge(res, var_grad, all = TRUE, by = c(country, "ind", per1, per2, "namesY"))
   res <- fit <- var_gr <- NULL
   data[, namesY := NULL]

   data[, rho := num1num2 / sqrt(num1num1 * num2num2)]
   data[, V12 := num1num2 * sqrt(var_1 * var_2 / (num1num1 * num2num2))]

   if (change_type == "relative") {
        data[, q_1 := - sum2 / sum1^2]
        data[, q_2 := 1 / sum1]
      } else {
         data[, q_1 := -1]
         data[, q_2 := 1]
       }

   if (change_type == "relative") {
        data[, estim := estim_2 / estim_1]
     } else data[, estim := estim_2 - estim_1]
   data[, var := q_1 * q_1 * var_1 + 2 * q_1 * q_2 * V12 + q_2 * q_2 * var_2]

   data[var >= 0, se := sqrt(var)]
  
   CI_lower <- CI_upper <- NULL
   tsad <- qnorm(0.5 * (1 + confidence))
   data[, CI_lower := estim - tsad * se]
   data[, CI_upper := estim + tsad * se]

   var_grad <- data[, c(country, per1, per2, Dom, 
                               "type", "q_1", "q_2",
                               "rho", "var_1", "var_2"), with = FALSE]

   changes_results <- data[, c(country, per1, per2, Dom, 
                               "type", "estim_1", "estim_2",
                               "estim", "var", "se",
                               "CI_lower", "CI_upper"), with = FALSE]
   data <- confidence_level <- NULL

   changes_results[, confidence_level := confidence]
   changes_results[, significant := TRUE]
   boundss <- as.numeric(change_type == "relative")
   changes_results[CI_lower <= boundss & CI_upper >= boundss, significant := FALSE]

   list(cros_lin_out = cros_lin_out,
        cros_res_out = cros_res_out,
        crossectional_results = crossectional_results,
        changes_results = changes_results)
}   


