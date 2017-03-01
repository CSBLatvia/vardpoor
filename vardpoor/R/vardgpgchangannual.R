
vardgpgchangannual <- function(Y, H, PSU, w_final, ID_level1,
                               ID_level2, Dom = NULL, Z = NULL,
                               gender, country = NULL, years,
                               subperiods, dataset = NULL, year1,
                               year2, X = NULL, countryX = NULL, 
                               yearsX = NULL, subperiodsX = NULL, 
                               X_ID_level1 = NULL, ind_gr = NULL,
                               g = NULL, q = NULL, datasetX = NULL,
                               percentratio = 1, use.estVar = FALSE,
                               confidence = 0.95) {

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

  years <- check_var(vars = years, varn = "years", dataset = dataset,
                     ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                     dif_name = c("percoun", "period_country", names(country)))
  yearm <- names(years)
  
  year1 <- check_var(vars = year1, varn = "year1", dataset = NULL,
                     ncols = 1, ischaracter = TRUE, years = years)
  
  year2 <- check_var(vars = year2, varn = "year2", dataset = NULL,
                     ncols = 1, ischaracter = TRUE, years = years)

  subperiods <- check_var(vars = subperiods, varn = "subperiods",
                          dataset = dataset, ncols = 1, 
                          Ynrow = Ynrow, ischaracter = TRUE,
                          dif_name = c("percoun", names(country)))

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1",
                         dataset = dataset, ncols = 1, Yncol = 0,
                         Ynrow = Ynrow, ischaracter = TRUE)

  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2",
                         dataset = dataset, ncols = 1, Yncol = 0,
                         Ynrow = Ynrow, ischaracter = TRUE,
                         namesID1 = names(ID_level1), country = country,
                         years = years, periods = subperiods)

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   ncols = 1, Yncol = 0, Ynrow = Ynrow,
                   ischaracter = TRUE, namesID1 = names(ID_level1))

  gender <- check_var(vars = gender, varn = "gender",
                      dataset = dataset, ncols = 1,
                      Ynrow = Ynrow, isnumeric = TRUE,
                      isvector = TRUE)

  dataset <- data.table(Y, H, ID_level1, ID_level2,
                        PSU, w_final, Z, gender)

  Y <- names(Y)
  H <- names(H)
  ID_level1 <- names(ID_level1)
  ID_level2 <- names(ID_level2)
  PSU <- names(PSU)
  Z <- names(Z)
  w_final <- "w_final"
  if (!is.null(country)) dataset <- data.table(dataset, country)
  country <- names(country)
  dataset <- data.table(dataset, years, subperiods)
  years <- names(years)
  subperiods <- names(subperiods)
  if (!is.null(Dom)) dataset <- data.table(dataset, Dom)
  Dom <- names(Dom)

  yearsgender <- paste0(years, "gender")
  dataset <- rbindlist(lapply(1:2, function(i) { dats <- copy(dataset)
                                                 dats[, (yearsgender) := paste0(get(years), "_", i)]
                                                 dats[get("gender") != i, (c(Y, Z)):= 0]
                                                 return(dats)}))

  if(!is.null(X)) {
        X <- check_var(vars = X, varn = "X", dataset = dataset,
                       check.names = TRUE, isnumeric = TRUE,
                       grepls = "__",
                       dif_name = c(names(years), names(subperiods),
                                    names(country), names(H), names(PSU),
                                    names(ID_level1), "w_final", names(Y),
                                    "w_design", "g", "q"))
        Xnrow <- nrow(X)

        g <- check_var(vars = g, varn = "g", dataset = dataset,
                       check.names = TRUE, ncols = 1, Xnrow = Xnrow,
                       isnumeric = TRUE)

        q <- check_var(vars = q, varn = "q", dataset = dataset,
                       check.names = TRUE, ncols = 1, Xnrow = Xnrow,
                       isnumeric = TRUE)

        ind_gr <- check_var(vars = ind_gr, varn = "ind_gr", dataset = dataset,
                            check.names = TRUE, ncols = 1, Xnrow = Xnrow,
                            dif_name = c(names(years), names(subperiods),
                                         names(country), names(H), names(PSU),
                                         names(ID_level1), "w_final", names(Y),
                                         names(X), "w_design", "g", "q"))

        countryX <- check_var(vars = countryX, varn = "countryX",
                              dataset = datasetX, ncols = 1, Xnrow = Xnrow,
                              ischaracter = TRUE, mustbedefined = !is.null(country),
                              varnout = "country", varname = names(country),
                               country = country)

        yearsX <- check_var(vars = yearsX, varn = "yearsX", dataset = datasetX,
                            ncols = 1, Xnrow = Xnrow, ischaracter = TRUE,
                            mustbedefined = !is.null(years), varnout = "years",
                            varname = names(years), country = country,
                            countryX = countryX, years = years)

        subperiodsX <- check_var(vars = subperiodsX, varn = "subperiodsX",
                                 dataset = datasetX, ncols = 1, Yncol = 0,
                                 Xnrow = Xnrow, ischaracter = TRUE,
                                 mustbedefined = !is.null(subperiods),
                                 varnout = "subperiods", varname = names(subperiods),
                                 country = country, countryX = countryX,
                                 years = years, yearsX = yearsX,
                                 periods = subperiods)

        X_ID_level1 <- check_var(vars = X_ID_level1, varn = "X_ID_level1",
                                 dataset = datasetX, ncols = 1, Yncol = 0,
                                 Xnrow = Xnrow, ischaracter = TRUE,
                                 varnout = "ID_level1", varname = names(ID_level1),
                                 country = country, countryX = countryX,
                                 years = years, yearsX = yearsX,
                                 periods = subperiods, periodsX = subperiodsX,
                                 ID_level1 = ID_level1)

        datasetX <- data.table(X, yearsX, subperiodsX, X_ID_level1, ind_gr, g, q)
        if (!is.null(countryX)) datasetX <- data.table(datasetX, countryX)
        countryX <- names(countryX)
        X <- names(X)
        yearsX <-  names(yearsX)
        subperiodsX <- names(subperiodsX)
        X_ID_level1 <- names(X_ID_level1)
        ind_gr <- names(ind_gr)
        g <- "g"
        q <- "q"

        datasetX <- rbindlist(lapply(1:2, function(i) { dats <- copy(datasetX)
                                                        dats[, (yearsgender) := paste0(get(yearsX), "_", i)]
                                                        return(dats)}))
   }
   year1 <- paste0(unique(dataset[[years]]), "_2")
   year2 <- paste0(unique(dataset[[years]]), "_1")

   rez <- vardchangannual(Y = Y, H = H, PSU = PSU,
                          w_final = w_final, ID_level1 = ID_level1,
                          ID_level2 = ID_level2, Dom = Dom,
                          Z = Z, country = country, years = yearsgender,
                          subperiods = subperiods, dataset = dataset,
                          year1 = year1, year2 = year2, X = X,
                          countryX = countryX, yearsX = yearsgender,
                          subperiodsX = subperiodsX, X_ID_level1 = X_ID_level1,
                          ind_gr = ind_gr, g = g, q = q, datasetX = datasetX,
                          percentratio = percentratio, use.estVar = use.estVar,
                          use.gender = TRUE, confidence = confidence)

  list(crossectional_results = rez$crossectional_results,
       crossectional_var_grad = rez$crossectional_var_grad,
       vardchanges_grad_var = rez$vardchanges_grad_var,
       vardchanges_rho = rez$vardchanges_rho,
       vardchanges_var_tau = rez$vardchanges_var_tau,
       vardchanges_results = rez$vardchanges_results,
       X_annual = rez$X_annual, A_matrix = rez$A_matrix,
       annual_sum = rez$annual_sum,
       annual_cros = rez$annual_cros)

}
