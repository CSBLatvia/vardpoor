
vardbootstr <- function(boots_count = 500, Y, H, PSU,
                        w_final, ID_level1, Z = NULL,
                        Dom = NULL, dh = 1, fpc,
                        dataset = NULL, years,
                        subperiods = NULL, year1 = NULL,
                        year2 = NULL, percentratio = 100,
                        confidence = 0.95, method = "cros"){
   
  method <- check_var(vars = method, varn = "method", varntype = "method") 
  boots_count <- check_var(vars = boots_count, varn = "boots_count", varntype = "pinteger")
  if (boots_count < 2) stop("Iteration must be larger than 2!")
  dh <- check_var(vars = dh, varn = "dh", varntype = "pinteger")  
  percentratio <- check_var(vars = percentratio, varn = "percentratio", varntype = "pinteger")  
  confidence <- check_var(vars = confidence, varn = "confidence", varntype = "numeric01")
  
  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)
  
  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                 dif_name = c("n_sk", "COUNT", "sample_size"))
  
  w_final <- check_var(vars = w_final, varn = "w_final",
                       dataset = dataset, ncols = 1, Ynrow = Ynrow,
                       isnumeric = TRUE, isvector = TRUE)
  
  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                 isnumeric = TRUE, mustbedefined = FALSE)
  
  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                   ncols = 0, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, mustbedefined = FALSE,
                   duplicatednames = TRUE, grepls = "__")
  
  fpc <- check_var(vars = fpc, varn = "fpc",
                   dataset = dataset, ncols = 1, Ynrow = Ynrow,
                   isnumeric = TRUE, isvector = TRUE)
  
  years <- check_var(vars = years, varn = "years", dataset = dataset,
                     ncols = 1, Yncol = 0, Ynrow = Ynrow, ischaracter = TRUE,
                     dif_name = c(names(H), "n_sk", "COUNT", 
                                  "sample_size", "Nrs", "iteration_nr"))
  if (method != "cros") {
    year1 <- check_var(vars = year1, varn = "year1", dataset = NULL, ncols = 1,
                       ischaracter = TRUE, years = years)
    
    year2 <- check_var(vars = year2, varn = "year2", dataset = NULL,  ncols = 1, 
                       ischaracter = TRUE, years = years)
  } else {if (!missing(year1)) if (!is.null(year1)) stop("'year1' must be NULL")
    if (!missing(year2)) if (!is.null(year2)) stop("'year2' must be NULL")
    year1 <- years[, .N, by = yearm][, N := NULL]
    year2 <- years[, .N, by = yearm][, N := NULL] }
  
  
  subperiods <- check_var(vars = subperiods, varn = "subperiods",
                          dataset = dataset, ncols = 1, Ynrow = Ynrow,
                          ischaracter = TRUE, years = years,
                          dif_name = c(names(H), names(years), 
                                       "n_sk", "COUNT", "sample_size",
                                       "Nrs", "iteration_nr"))
  subpm <- names(subperiods)
  subn <- data.table(years, subperiods)
  subn <- subn[, .N, by = c(names(subn))]
  subn <- max(subn[, .N, by = names(years)][["N"]])
  
  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         ischaracter = TRUE)
  
  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                   namesID1 = names(ID_level1),
                   dif_name = c(names(H), names(years), 
                                "n_sk", "COUNT", "sample_size",
                                "Nrs", "iteration_nr"))
  
  dataset <- data.table(Y, H, PSU, ID_level1, w_final, years, fpc)
  if (!is.null(Z)) dataset <- data.table(dataset, Z)
  if (!is.null(Dom)) dataset <- data.table(dataset, Dom)
  if (!is.null(subperiods)) dataset <- data.table(dataset, subperiods)
  
  CI_lower <- CI_upper <- COUNT <- N <- Nrs <- NumberHits <- NULL
  WA <- WR <- absolute_margin_of_error <- confidence_level <- NULL
  cv <- estim <- estimR <- estimR_1 <- estimR_2 <- estim_1 <- NULL
  estim_2 <- iteration_nr <- n_sk <- nrs <- relative_margin_of_error <- NULL
  rse <- sample_size <- se <- sk <- value <- value_1 <- value_2 <- NULL
  var_est <-  var_est2 <- yearm <- NULL

  Y <- names(Y)
  Z <- names(Z)
  H <- names(H)
  PSU <- names(PSU)
  Dom <- names(Dom)
  fpc <- names(fpc)
  years <- names(years)
  ID_level1 <- names(ID_level1)
  subperiods <- names(subperiods)
  if (!is.null(subperiods)) period <- c(years, subperiods)
  dataset[, sk := .N, by = c(period, H, PSU)]
  dataset[, nrs := 1:.N, by = c(period, H, PSU)]

  frames <- dataset[nrs == sk, c(period, H, PSU), with = FALSE]
  
  frames[, COUNT := .N, by = c(H, period)]
  frames[, sample_size := COUNT - dh]
  frames[sample_size == 0, sample_size := 1]
  frames[, n_sk := 1:.N, by = c(H, period)]
  
  samples <- lapply(1:boots_count, function(i) { 
    strat <- copy(frames)
    strat[, iteration_nr := i]
    strat_agg <- strat[, .(n_sk = sample(.N, size = .SD[n_sk == 1, sample_size], replace = TRUE)), by = c(H, period)]
    setkeyv(strat, names(strat_agg))
    strat_agg <- strat_agg[, .(NumberHits = .N), keyby = names(strat_agg)]
    strat <- merge(strat, strat_agg, all.x = TRUE)
    strat <- strat[!is.na(NumberHits)]
    strat[]
  })
  samples <- rbindlist(samples)
  rm(frames)
  samples[, WA := NumberHits]
  setkeyv(samples, c(period, PSU, H, "iteration_nr"))
  setkeyv(dataset, c(period, PSU, H))
  samples <- merge(samples, dataset, by = c(period, PSU, H), all.x = TRUE)
  
  samples[, WR := w_final * ((1 - sqrt(sample_size / (COUNT - 1) * (1 - fpc))) +
                               sqrt(sample_size / (COUNT - 1) * (1 - fpc)) * COUNT / sample_size * WA)]
  samples[COUNT == 1, WR := 1]
  
  period2 <- c(period, Dom)
  
  sarYZ <- c(Y, Z)
  aggr <- samples[, lapply(sarYZ, function(x) { sum(WR * get(x)) / boots_count}),
                  keyby = c(period2, "iteration_nr")]
  setnames(aggr, paste0("V", 1:length(sarYZ)), sarYZ)
  rm(samples, dataset)
  
  aggr2 <- copy(aggr)[, (subperiods) := NULL]
  aggr <- rbindlist(list(aggr, aggr2), fill = TRUE)
  aggr <- aggr[, lapply(.SD, sum), keyby = c(period2, "iteration_nr"),
               .SDcols = sarYZ]
  rm(aggr2)
  sarnew <- Y
  cros_estim <- NULL
  if (!is.null(Z)) { aggr[, (paste0("Ratio_", Y, "_", Z)) := lapply(1:length(Y), function(i) {get(Y) / get(Z)})]
    sarnew <- paste0("Ratio_", Y, "_", Z)
    sarYZ <- c(sarYZ, sarnew)
    cros_estim <- aggr[, lapply(1:length(Y), function(i) {sum(get(Y)) / sum(get(Z)) * percentratio}),
                       keyby = c(period2)]
    setnames(cros_estim, names(cros_estim), c(period2, paste0("Ratio_", Y, "_", Z)))
    cros_estim <- melt(cros_estim, id.vars = c(period2))
    setnames(cros_estim, "value", "estimR")
  }
  
  aggr <- melt(aggr, id.vars = c(period2, "iteration_nr"))
  if (!is.null(Z)) aggr[, value := value * percentratio]
  
  cros_result <- aggr[, .(estim = mean(value),
                          var_est = var(value)), keyby = c(period2, "variable")]
  if (!is.null(Z)) {cros_result <- merge(cros_result, cros_estim, keyby = c(period2, "variable"), all.x = TRUE)
  cros_result[!is.na(estimR), estim := estimR]
  cros_result[, estimR := NULL]   }
  
  cros_result[, var_est2 := var_est]
  cros_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
  cros_result[, se := sqrt(var_est2)]
  
  cros_result[(estim != 0) & !is.nan(estim), rse := se / estim]
  cros_result[estim == 0 | is.nan(estim), rse := NA]
  cros_result[, cv := rse * 100]
  
  tsad <- qnorm(0.5 * (1 + confidence))
  cros_result[, absolute_margin_of_error := tsad * se]
  cros_result[, relative_margin_of_error := tsad * cv]
  cros_result[, CI_lower := estim - tsad * se]
  cros_result[, CI_upper := estim + tsad * se]
  cros_result[, confidence_level := confidence]
  setnames(cros_result, "var_est", "var")
  cros_result[, var_est2 := NULL]
              
  changes_result <- NULL
  if (method == "netchanges"){
    aggr_changes <- aggr[is.na(get(subperiods)), c(period2, "iteration_nr", "variable", "value"), with = FALSE]
    aggr_changes[, (subperiods) := NULL]
    
    year1[, Nrs := .I]
    year2[, Nrs := .I]
    cros_estim <- cros_estim[is.na(get(subperiods))]
    cros_estim[, (subperiods) := NULL]
    
    year01 <- merge(year1, aggr_changes, by = years, all.x = TRUE, allow.cartesian = TRUE)
    setnames(year01, c(years, "value"), paste0(c(years, "value"), "_1"))
    
    year02 <- merge(year2, aggr_changes, by = years, all.x = TRUE, allow.cartesian = TRUE)
    setnames(year02, c(years, "value"), paste0(c(years, "value"), "_2"))
    
    changes_result <- merge(year01, year02, by = c("iteration_nr", "Nrs", "variable"), all = TRUE)
    changes_result[, value := value_2 - value_1]
    
    yearsn <- c(paste0(years, "_", 1:2), Dom)
    changes_result <- changes_result[, .(estim_2 = mean(value_2),
                                         estim_1 = mean(value_1),
                                         estim = mean(value),
                                         var_est = var(value)), keyby = c(yearsn, "variable")]
    year1[, Nrs := .I]
    year2[, Nrs := .I]
    
    if (!is.null(Z)) {
      year1 <- merge(year1, cros_estim, by = years, all.x = TRUE, allow.cartesian = TRUE)
      setnames(year1, c(years, "estimR"), paste0(c(years, "estimR"), "_1"))
      
      year2 <- merge(year2, cros_estim, by = years, all.x = TRUE, allow.cartesian = TRUE)
      setnames(year2, c(years, "estimR"), paste0(c(years, "estimR"), "_2"))
      changes_estim <- merge(year1, year2, by = c("Nrs", "variable"), all = TRUE)
      changes_estim[, Nrs := NULL]
      
      changes_result  <- merge(changes_result,  changes_estim, keyby = c(yearsn, "variable"), all = TRUE)
      changes_result[!is.na(estimR_1), estim_1 := estimR_1]
      changes_result[!is.na(estimR_2), estim_2 := estimR_2]    
    }
    
    changes_result[, estim := estim_2 - estim_1]
    
    changes_result[, var_est2 := as.numeric(var_est)]
    changes_result[xor(is.na(var_est2), var_est2 < 0), var_est2 := NA]
    changes_result[, se := sqrt(var_est2)]
    
    changes_result[(estim != 0) & !is.nan(estim), rse := se / estim]
    changes_result[estim == 0 | is.nan(estim), rse := NA]
    changes_result[, cv := rse * 100]
    
    tsad <- qnorm(0.5 * (1 + confidence))
    changes_result[, absolute_margin_of_error := tsad * se]
    changes_result[, relative_margin_of_error:= tsad * cv]
    changes_result[, CI_lower := estim - tsad * se]
    changes_result[, CI_upper := estim + tsad * se]
    
    changes_result[, confidence_level := confidence]
    setnames(changes_result, "var_est", "var")
    changes_result[, var_est2 := NULL]
    
    significant <- NULL
    changes_result[, significant := "YES"]
    changes_result[CI_lower <= 0 & CI_upper >= 0, significant := "NO"]
  }
  
  
  list(cros_annual_results = cros_result,
       changes_annual_results = changes_result[])
  
}



