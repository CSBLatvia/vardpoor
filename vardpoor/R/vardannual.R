 
vardannual <- function(Y, H, PSU, w_final, ID_level1,
                       ID_level2, Dom = NULL, Z = NULL,
                       gender = NULL, country = NULL,
                       years, subperiods, dataset = NULL,
                       year1 = NULL, year2 = NULL, X = NULL,
                       countryX = NULL, yearsX = NULL,
                       subperiodsX = NULL, X_ID_level1 = NULL,
                       ind_gr = NULL, g = NULL, q = NULL,
                       datasetX = NULL, frate = 0, percentratio = 1,
                       use.estVar = FALSE, use.gender = FALSE,
                       confidence = 0.95, method = "cros") {

  ### Checking
  outp_res <- FALSE
  method <- check_var(vars = method, varn = "method", varntype = "method") 
  percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger")  
  use.estVar <- check_var(vars = use.estVar, varn = "use.estVar", varntype = "logical")
  use.gender <- check_var(vars = use.gender, varn = "use.gender", varntype = "logical")
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")
  frate <- check_var(vars = frate, varn = "frate", varntype = "numeric0100")

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
                       dif_name = c("percoun", "period_country", "Nrs"))
  
  years <- check_var(vars = years, varn = "years", dataset = dataset,
                     ncols = 1, Yncol = 0, Ynrow = Ynrow, ischaracter = TRUE,
                     dif_name = c("percoun", "period_country", names(country), "yearg", "Nrs"),
                     use.gender = use.gender)
  yearg <- NULL
  years[, yearg := substr(get(names(years)), 1, nchar(get(names(years))) - ifelse(use.gender, 2, 0))]
  yearm <- names(years)[1 + use.gender]
  
  if (method != "cros") {
            year1 <- check_var(vars = year1, varn = "year1", dataset = NULL, ncols = 1,
                               ischaracter = TRUE, years = years[, 1 + use.gender, with = FALSE])

            year2 <- check_var(vars = year2, varn = "year2", dataset = NULL,  ncols = 1, 
                               ischaracter = TRUE, years = years[, 1 + use.gender, with = FALSE])
         } else { if (!missing(year1)) if(!is.null(year1)) stop("'year1' must be NULL")
                  if (!missing(year2)) if (!is.null(year2)) stop("'year2' must be NULL")
                  year1 <- years[, .N, by = yearm][, N := NULL]
                  year2 <- years[, .N, by = yearm][, N := NULL] }

  subperiods <- check_var(vars = subperiods, varn = "subperiods",
                          dataset = dataset, ncols = 1, Ynrow = Ynrow,
                          ischaracter = TRUE, years = years, Domen = Dom,
                          dif_name = c("percoun", "period_country", names(country), "yearg", "Nrs"))
  subpm <- names(subperiods)
  subn <- data.table(years, subperiods, Dom)
  subn <- subn[, .N, by = c(names(subn))]

  subn <- max(subn[, .N, by = names(years)][["N"]])
 
  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE)
  
  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE, namesID1 = names(ID_level1),
                         country = country, years = years, periods = subperiods)
  
  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   namesID1 = names(ID_level1))

  if(!is.null(X) | !is.null(ind_gr) | !is.null(g) | !is.null(q) | !is.null(countryX) | 
      !is.null(yearsX) | !is.null(subperiodsX) | !is.null(X_ID_level1) | !is.null(datasetX)) {
         X <- check_var(vars = X, varn = "X", dataset = datasetX,
                        isnumeric = TRUE,
                        dif_name = c(names(years), names(subperiods),
                                     names(country), names(H), names(PSU),
                                     names(ID_level1), "w_final", names(Y),
                                     "w_design", "g", "q"), dX = "X")
         Xnrow <- nrow(X)

         ind_gr <- check_var(vars = ind_gr, varn = "ind_gr",
                             dataset = datasetX, ncols = 1,
                             Xnrow = Xnrow, ischaracter = TRUE,
                             dif_name = c(names(years), names(subperiods),
                                          names(country), names(H), names(PSU),
                                          names(ID_level1), "w_final", names(Y),
                                          names(X), "w_design", "g", "q"), dX = "X")
 
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
 
          yearsX <- check_var(vars = yearsX, varn = "yearsX", dataset = datasetX,
                              ncols = 1, Xnrow = Xnrow, ischaracter = TRUE,
                              mustbedefined = !is.null(years), varnout = "years",
                              varname = names(years)[1], country = country,
                              countryX = countryX, years = years[, 1, with = FALSE],
                              use.gender = use.gender, dX = "X")

          subperiodsX <- check_var(vars = subperiodsX, varn = "subperiodsX",
                                   dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                   ischaracter = TRUE, mustbedefined = !is.null(subperiods),
                                   varnout = "subperiods", varname = names(subperiods),
                                   country = country, countryX = countryX,
                                   years = years[, 1, with = FALSE], dX = "X",
                                   yearsX = yearsX, periods = subperiods)
 
          X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                    dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                                    ischaracter = TRUE, varnout = "ID_level1",
                                    varname = names(ID_level1), country = country,
                                    countryX = countryX, years = years[, 1, with = FALSE],
                                    yearsX = yearsX, periods = subperiods, dX = "X",
                                    periodsX = subperiodsX, ID_level1 = ID_level1)
                                    
         }
   dataset <- datasetX <- NULL



   ids <- nams <- cros_se <- num1 <- totalY <- totalZ <- NULL
   estim_1 <- estim_2 <- avar <- N <- estim <- NULL
   var_est2 <- se <- rse <- cv <- CI_lower <- CI_upper <- NULL
   Nr_sar <- cols <- Nrs <- percoun <- totalY_male <- NULL
   totalZ_male <- totalY_female <- totalZ_female <- confidence_level <- NULL  

   pers <- data.table(years, subperiods,
                      pers = paste0(years[[1]], "__", subperiods[[1]]))
   pers[, yearg := substr(get(names(years)), 1, nchar(get(names(years))) - ifelse(use.gender, 2, 0))]
 
   if (!is.null(X)) persX <- data.table(yearsX, subperiodsX,
                                  pers = paste0(yearsX[[names(yearsX)]], "__", subperiodsX[[names(subperiodsX)]]))

   sarak <- pers[,.N, keyby = names(pers)][, N := NULL][]

   cros_calc <- vardcros(Y = Y, H = H, PSU = PSU, w_final = w_final,
                         ID_level1 = ID_level1, ID_level2 = ID_level2,
                         Dom = Dom, Z = Z, gender = gender, 
                         country = country, period = pers[, "pers"],
                         dataset = NULL, X = X, countryX = countryX,
                         periodX = persX[, "pers"], X_ID_level1 = X_ID_level1,
                         ind_gr = ind_gr, g = g, q = q, datasetX = NULL,
                         linratio = !is.null(Z),
                         percentratio = percentratio,
                         use.estVar = use.estVar,
                         ID_level1_max = is.null(X),
                         outp_res = outp_res,
                         withperiod = TRUE,
                         netchanges = TRUE,
                         confidence = confidence,
                         checking = FALSE)
   countryX <- periodX <- yearX <- NULL
   X_ID_level1 <- ind_gr <- g <- q <- ID_level2 <- NULL
   subperiods <- w_final <- NULL   
   years <- years[, .N, by = c(names(years)[1])][, N := NULL]   

   pers12 <- paste("pers", 1:2, sep = "_")
   if (method == "cros") { period12 <- c(yearm, paste(subpm, 1:2, sep = "_"))
                           years12 <- yearm
                  } else { period12 <- paste(rep(c(yearm, subpm), 2), c(1, 1, 2, 2), sep = "_")
                           years12 <- paste(yearm, 1:2, sep = "_") }

   atsyear <- rbindlist(list(data.table(Nrs = 1 : nrow(year1), yrs = 1, year1),
                             data.table(Nrs = 1 : nrow(year2), yrs = 2, year2)))
   if (method == "cros") atsyear <- data.table(Nrs = 1 : nrow(year1), yrs = 1, year1)

   atsyear <- merge(atsyear, sarak, all.x = TRUE, by = yearm,
                    sort = FALSE, allow.cartesian = TRUE)

   atsyear[, ids := 1:.N, by = "Nrs" ] 

   nr1 <- max(atsyear[["ids"]])

   yrs <- rbindlist(lapply(1 : (nr1 - 1), function(j) {
                        atsy1 <- atsyear[ids == j]
                        atsy2 <- atsyear[ids %in% c((j + 1) : nr1)]
                        if (method == "cros") {
                                   atsy2[, (yearm) := NULL]
                                   setnames(atsy1, names(atsy1)[-c(1:2)], paste0(names(atsy1)[-c(1:2)], "_1"))
                                   setnames(atsy2, names(atsy2)[-1], paste0(names(atsy2)[-1], "_2"))
                            } else { setnames(atsy1, names(atsy1)[-2], paste0(names(atsy1)[-2], "_1"))
                                     setnames(atsy2, names(atsy2)[-2], paste0(names(atsy2)[-2], "_2")) }
                        merge(atsy1, atsy2, all = TRUE, by = "Nrs")
                     }))
   
   if (method != "cros") {  
              yr12 <- rbind(data.table(Nrs = 1 : nrow(year1), yearg = year1[[1]]),
                            data.table(Nrs = 1 : nrow(year1), yearg = year2[[1]]))
         } else yr12 <- data.table(Nrs = 1 : nrow(year1), yearg = year1[[1]])
 
   if (!is.null(Dom)) {
             Y1 <- namesD(Y, Dom, uniqueD = TRUE)
             Z1 <- NULL
             if (!is.null(Z)) Z1 <- namesD(Z, Dom, uniqueD = TRUE)
        } else { Y1 <- names(Y)
                 Z1 <- names(Z) }

   Y <- names(Y)
   Z <- names(Z)
   names_country <- names(country)
   PSU <- names(PSU)
   H <- names(H)
   Dom <- names(Dom)
   yrs_without <- yrs[, .N, by = c("pers_1", "pers_2")]

   data <- cros_calc$data_net_changes
   changes_calc <- vardchanges_calculation(Y1 = Y1, Z1 = Z1, Dom = Dom, 
                                           names_country = names_country,
                                           per = "pers", PSU = PSU, H = H,
                                           period1 = yrs_without[, .(pers = get("pers_1"))],
                                           period2 = yrs_without[, .(pers = get("pers_2"))],
                                           cros_var_grad = cros_calc$var_grad, change_type = "absolute",
                                           data = data, linratio = !is.null(Z), annual = TRUE,
                                           percentratio = percentratio, use.estVar = use.estVar,
                                           confidence = confidence, poor = FALSE)

    pers <- pers[, .N, keyby = names(pers)][, N := NULL]
    crossectional_results <- merge(pers, cros_calc$results, all = TRUE, by = "pers")
    crossectional_results[, (c("pers", "yearg")) := NULL]
    if (is.null(names(country))) crossectional_results[, percoun := NULL]
    gender <- data <- yrs_without <- cros_calc <- NULL
  
    cros_var_grad <- merge(sarak, changes_calc$cros_var_grad,
                                  all.y = TRUE, by = c("pers"))

    rho <- merge(yrs, changes_calc$rho_matrix,
                           all.y = TRUE,
                           by = c("pers_1", "pers_2"),
                           allow.cartesian = TRUE)

    sar <- c("Nrs", names_country, "namesY", "namesZ", Dom)
    sar <- sar[sar %in% names(rho)]
    rho[, Nr_sar := .GRP, by = sar]

    rho1 <- rho[nams == "num2"]
    rho1[, ids := 1 : .N, by = sar]

    rhoj <- rho[,.N, keyby = sar][, N := NULL]
    max_ids <- max(atsyear[["ids"]])

    yr12cros <- merge(yr12, cros_var_grad, by = "yearg",
                            allow.cartesian = TRUE, sort = FALSE)
 
    
    apstr <- lapply(1 : max(rho[["Nr_sar"]]), function(j){
                  rho2 <- rho1[Nr_sar == j]
                  A_matrix <- diag(1, max_ids, max_ids)
 
                  for (k in 1 : max(rho2[["ids"]])) {
                           at <- rho2[k == ids]
                           A_matrix[at[["ids_1"]], at[["ids_2"]]] <- at[["rho_num1"]]
                           A_matrix[at[["ids_2"]], at[["ids_1"]]] <- at[["rho_num1"]]
                           if (method != "cros") {
                                  if (at[["ids_2"]] > subn & at[["ids_1"]] < subn + 1) {
                                             A_matrix[at[["ids_1"]], at[["ids_2"]]] <- - at[["rho_num1"]]
                                             A_matrix[at[["ids_2"]], at[["ids_1"]]] <- - at[["rho_num1"]]
                                          }}
                        }
                  cros_rho <- merge(yr12cros, rho2[1, sar, with = FALSE], by = sar, sort = FALSE)
                  cros_rho[, cols := paste0("V", 1:.N)]
                  cros_rho[, cros_se := sqrt(num1)]
                  X <- cros_rho[["cros_se"]]

                  annual_var <- data.table(rho2[1, sar, with = FALSE],
                                             (1 - frate / 100) / (subn)^2 * (t(X) %*% A_matrix) %*% X)
                  setnames(annual_var, c("V1"), c("var"))
                  A_matrix <- data.table(rho2[1, sar, with = FALSE], cols = paste0("V", 1 : nrow(A_matrix)), A_matrix)
                  list(cros_rho, A_matrix, annual_var)})

    cros_rho <- rbindlist(lapply(apstr, function(x) x[[1]]))
    A_matrix <- rbindlist(lapply(apstr, function(x) x[[2]]))
    annual_var <- rbindlist(lapply(apstr, function(x) x[[3]]))

    sars <- c(names(country), yearm, Dom, "namesY", "namesZ")
    sars <- sars[sars %in% names(cros_var_grad)]
    sarsb <- sars[!(sars %in% yearm)]
    sarc <- c("totalY", "totalZ", "totalY_male", 
              "totalY_female", "totalZ_male", "totalZ_female")
    sarc <- sarc[sarc %in% names(cros_var_grad)]
   
    ysum <- cros_var_grad[, lapply(.SD, mean), by = sars, .SDcols = sarc]

    if (!is.null(ysum$totalZ_male)) {
             ysum[, estim := (totalY_male / totalZ_male - totalY_female / totalZ_female) * percentratio]
       } else if (!is.null(ysum$totalY_male)) {
             ysum[, estim := (totalY_male - totalY_female) * percentratio]
        } else if (!is.null(ysum$totalZ)) {
             ysum[, estim := totalY / totalZ * percentratio]
         } else ysum[, estim := totalY]
    
    year1m <- year1[[yearm]]
    ysum1 <- ysum[get(yearm) %in% year1m, c(sars, "estim"), with = FALSE]

    years1 <- copy(year1)[, Nrs := 1: .N]
    ysum1 <- merge(years1, ysum1, by = yearm, sort = FALSE, allow.cartesian = TRUE)

    if (method != "cros") {
            years2 <- copy(year2)[, Nrs := 1: .N]
            year2m <- year2[[yearm]]
            ysum2 <- ysum[get(yearm) %in% year2m, c(sars, "estim"), with = FALSE]
            ysum2 <- merge(years2, ysum2, by = yearm, sort = FALSE, allow.cartesian = TRUE)
            setnames(ysum1, c("estim", yearm), c("estim_1", paste0(yearm, "_1")))
            setnames(ysum2, c("estim", yearm), c("estim_2", paste0(yearm, "_2")))
            ysum <- merge(ysum1, ysum2, all.x = TRUE, by = c("Nrs", sarsb))   
            ysum[, estim := estim_2 - estim_1] 
        } else ysum <- ysum1
    ysum1 <- ysum2 <- NULL

    annual_results <- merge(ysum, annual_var, by = c("Nrs", sarsb), sort = FALSE)

    estim <- "estim"
    if(method != "cros") estim <- c("estim_1", "estim_2", "estim")
    annual_results <- annual_results[, c(years12, sarsb, estim, "var"), with = FALSE]
    ysum <- ysum[, c(years12, sarsb, estim), with = FALSE]

   grad_var <- merge(yrs[, c(pers12, period12), with = FALSE],
                     changes_calc$grad_var, all.y = TRUE,
                     by = pers12, allow.cartesian = TRUE)
   grad_var[, (pers12) := NULL]


   var_tau <- merge(yrs[, c(pers12, period12), with = FALSE],
                    changes_calc$var_tau, all.y = TRUE,
                    by = pers12, allow.cartesian = TRUE)
   var_tau[, (pers12) := NULL]

   vardchanges_results <- merge(yrs[, c(pers12, period12), with = FALSE],
                                changes_calc$changes_results, all.y = TRUE,
                                by = pers12, allow.cartesian = TRUE)
   vardchanges_results[, (pers12) := NULL]


   X_annual <- cros_rho
   if(method != "cros") {
                     atsyear <- data.table(Nrs = 1 : nrow(year1), year1, year2, check.names = TRUE)
                     setnames(atsyear, names(atsyear)[2:3], years12) 
                     X_annual <- merge(atsyear, cros_rho, all.y = TRUE, by = "Nrs", sort = FALSE)
             } else atsyear <- data.table(Nrs = 1 : nrow(years), years, check.names = TRUE)
   
   vars <- c(years12, subpm, sarsb, "cols", "cros_se")
   X_annual <- X_annual[, vars[vars %in% names(X_annual)], with = FALSE]

   A_matrix <- merge(atsyear, A_matrix, all.y = TRUE, by = "Nrs", sort = FALSE)
   A_matrix[, Nrs := NULL]

   annual_results[, var_est2 := var]
   annual_results[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
   annual_results[, se := sqrt(var_est2)]
   annual_results[, var_est2 := NULL]

   annual_results[, rse := se / estim]
   annual_results[, cv := rse * 100] 

   tsad <- qnorm(0.5 * (1 + confidence))
   absolute_margin_of_error <- relative_margin_of_error <- NULL
   annual_results[, absolute_margin_of_error := tsad * se]
   annual_results[, relative_margin_of_error := tsad * cv]
 
   annual_results[, CI_lower := estim - tsad * se]
   annual_results[, CI_upper := estim + tsad * se]
   annual_results[, confidence_level := confidence]

   if (method != "cros") {
              significant <- NULL
              annual_results[, significant := "YES"]
              annual_results[CI_lower <= 0 & CI_upper >= 0, significant := "NO"]
   }

   list(crossectional_results = crossectional_results,
        crossectional_var_grad = cros_var_grad,
        vardchanges_grad_var = grad_var,
        vardchanges_rho = rho,
        vardchanges_var_tau = var_tau,
        vardchanges_results = vardchanges_results,
        X_annual = X_annual, A_matrix = A_matrix,
        annual_sum = ysum,
        annual_results = annual_results)

 }
