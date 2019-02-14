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
                            order_quant = 50, alpha = 20,
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
  order_quant <- check_var(vars = order_quant, varn = "order_quant", varntype = "numeric0100") 
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
                 ischaracter = TRUE, 
                 dif_name = c("type", "nameYs", "dataH_stratas"))
  
  sort <- check_var(vars = sort, varn = "sort",
                    dataset = dataset, ncols = 1,
                    Ynrow = Ynrow, mustbedefined = FALSE,
                    isnumeric = TRUE, isvector = TRUE)
  
  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   Ynrow = Ynrow, ischaracter = TRUE,
                   mustbedefined = FALSE, duplicatednames = TRUE,
                   dif_name = c("type", "nameYs"),
                   grepls = "__")
  
  country <- check_var(vars = country, varn = "country",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       ischaracter = TRUE, mustbedefined = FALSE,
                       dif_name = c("percoun", "period_country",
                                    "type", "nameYs"))
  
  period <- check_var(vars = period, varn = "period",
                      dataset = dataset, Ynrow = Ynrow,
                      ischaracter = TRUE, duplicatednames = TRUE,
                      dif_name = c("percoun", "period_country", 
                                   "type", "nameYs", names(country)))
  
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
  
  if(!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) | !is.null(countryX) |
      !is.null(periodX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
         X <- check_var(vars = X, varn = "X", dataset = datasetX,
                        check.names = TRUE, isnumeric = TRUE,
                        dif_name = c(names(period), names(country), names(H),
                                     names(PSU), names(ID_level1), "w_final",
                                     "w_design", "g", "q", "type", "nameYs"),
                        dX = "X")
         Xnrow <- nrow(X)
    
         ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                             dataset = datasetX, ncols = 1,
                             Xnrow = Xnrow, ischaracter = TRUE, 
                             dif_name = c(names(period), names(country), names(H),
                                          names(PSU), names(ID_level1), "w_final",
                                          "w_design", "g", "q", "type", "nameYs"),
                             dX = "X")
    
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

  cros_calc <- vardcrospoor(Y = Y, age = age, pl085 = pl085,
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

  cros_lin_out <- cros_calc$lin_out
  cros_res_out <- cros_calc$res_out
  data_res <- cros_calc$res_out
  data <- cros_calc$data_net_changes
  crossectional_results <- copy(cros_calc$results)
  ID_level1 <- ID_level2 <- percoun <- cros_calc <- NULL
  
  sar <- c(names(period), names(country), names(Dom), "percoun", "type", "estim", "var")
  sar <- sar[sar %in% names(crossectional_results)]
  cros_var_grad <- crossectional_results[, sar, with = FALSE]
  setnames(cros_var_grad, "var", "num1")
  value <- nameYs <- NULL

  var_grad0 <- melt(data, id = c(names(period), names(country)), measure.vars = c(names(data)[grepl("lin", names(data))]))
  var_grad0 <- var_grad0[, .(valueY1 = sum(value)), keyby = c(names(period), names(country), "variable")]
  setnames(var_grad0, "variable", "nameYs")
        
  if (!is.null(Dom)) {
          cros_var_grad[, nameYs := namesD(cros_var_grad[, "type"], cros_var_grad[, names(Dom), with = FALSE], uniqueD = FALSE)]
          cros_var_grad[, nameYs := paste0("lin_", tolower(type), "__", substr(nameYs, 7, nchar(nameYs)))]
   } else  cros_var_grad[, nameYs := paste0("lin_", tolower(type))]
 
  cros_var_grad <- merge(cros_var_grad, var_grad0, all = TRUE, by = c(names(period), names(country), "nameYs"))
  cros_var_grad[, nameYs := NULL]
  var_grad0 <- NULL

  changes_calc <- vardchanges_calculation(Y1 = "type", Z1 = NULL, Dom = names(Dom),
                                          names_country = names(country), per = names(period),
                                          PSU = names(PSU), H = names(H), period1 = period1,
                                          period2 = period2, cros_var_grad = cros_var_grad,
                                          change_type = change_type, data = data, linratio = FALSE, 
                                          annual = FALSE, percentratio = 1,
                                          use.estVar = use.estVar, confidence = confidence,
                                          poor = TRUE)

  Y1 <- Z1 <- Dom <- period <- PSU <- H <- period1 <- period2 <- NULL
  
  crossectional_results <- cros_calc$results
  if (is.null(names(country))) crossectional_results[, percoun := NULL]

  list(lin_out <- cros_calc$lin_out,
       res_out = cros_calc$res_out,
       crossectional_results = crossectional_results,
       crossectional_var_grad = changes_calc$cros_var_grad,
       grad_var = changes_calc$grad_var,
       rho = changes_calc$rho_matrix,
       var_tau = changes_calc$var_tau,
       changes_results = changes_calc$changes_results)
 }   


