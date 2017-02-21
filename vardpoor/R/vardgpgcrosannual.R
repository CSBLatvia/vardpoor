
vardgpgcrosannual <- function(Y, H, PSU, w_final, ID_level1,
                              ID_level2, Dom = NULL, Z = NULL,
                              gender, country = NULL, years,
                              subperiods, dataset = NULL, X = NULL,
                              countryX = NULL, yearsX = NULL,
                              subperiodsX = NULL, X_ID_level1 = NULL,
                              ind_gr = NULL, g = NULL, q = NULL,
                              datasetX = NULL, percentratio = 1,
                              use.estVar = FALSE, confidence = 0.95) {

  ### Checking
  vars = Y
  varn = "Y"
  ncols = 0
  Yncol = 0
  Ynrow = 0
  Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                 check.names = TRUE, ncols = 0, Yncol = 0,
                 Ynrow = 0, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)

  H <- check_var(vars = H, varn = "H", dataset = dataset,
                 check.names = TRUE, ncols = 1, Yncol = 0,
                 Ynrow = Ynrow, isnumeric = FALSE, ascharacter = TRUE)

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1", dataset = dataset,
                         check.names = TRUE, ncols = 1, Yncol = 0, Ynrow = Ynrow,
                         isnumeric = FALSE, ascharacter = TRUE)

  Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset, check.names = TRUE,
                   ncols = 0, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                   ascharacter = TRUE, dif_name = "percoun", grepls = "__")

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset, check.names = TRUE,
                   ncol = 1, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                   ascharacter = TRUE, namesID1 = names(ID_level1))

  w_final <- check_var(vars = w_final, varn = "w_final", dataset = dataset,
                       check.names = TRUE, ncols = 1, Yncol = 0, Ynrow = Ynrow,
                       isnumeric = TRUE, ascharacter = FALSE, asvector = TRUE)

  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2", dataset = dataset,
                         check.names = TRUE, ncols = 1, Yncol = 0, Ynrow = Ynrow,
                         isnumeric = FALSE, ascharacter = TRUE, asvector = FALSE,
                         namesID1 = names(ID_level1))

  country <- check_var(vars = country, varn = "country", dataset = dataset,
                       check.names = TRUE, ncols = 1, Yncol = 0, Ynrow = Ynrow,
                       isnumeric = FALSE, ascharacter = TRUE, dif_name = "percoun")

  Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                 data_type = "data.table", check.names = TRUE, ncols = 0,
                 Yncol = Yncol, Ynrow = Ynrow, isnumeric = FALSE,
                 ascharacter = TRUE, dif_name = "percoun")

  years <- check_var(vars = years, varn = "years", dataset = dataset,
                     data_type = "data.table", check.names = TRUE, ncols = 1,
                     Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                     ascharacter = TRUE, dif_name = "percoun")

  subperiods <- check_var(vars = subperiods, varn = "subperiods", dataset = dataset,
                     data_type = "data.table", check.names = TRUE, ncols = 1,
                     Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                     ascharacter = TRUE, dif_name = "percoun")

  gender <- check_var(vars = gender, varn = "gender",
                      dataset = dataset, data_type = "data.frame",
                      check.names = TRUE, ncols = 1, Yncol = 0,
                      Ynrow = Ynrow, isnumeric = TRUE,
                      ascharacter = TRUE, asvector = TRUE)

  dataset <- data.table(Y, H, ID_level1, ID_level2,
                        PSU, w_final, Z, gender)
  Y <- names(Y)
  H <- names(H)
  ID_level1 <- names(ID_level1)
  ID_level2 <- names(ID_level2)
  PSU <- names(PSU)
  Z <- names(Z)
  w_final <- "w_final"
  gender <- "gender"
  if (!is.null(country)) dataset <- data.table(dataset, country)
  country <- names(country)
  dataset <- data.table(dataset, years, subperiods)
  years <- names(years)
  subperiods <- names(subperiods)
  if (!is.null(Dom)) dataset <- data.table(dataset, Dom)
  Dom <- names(Dom)

 if(!is.null(X)) {
        X <- check_var(vars = X, varn = "X", dataset = dataset,
                       check.names = TRUE, ncols = 0, Yncol = 0,
                       Ynrow = 0, Xnrow = 0, isnumeric = TRUE,
                       grepls = "__")
        Xnrow <- nrow(X)

        g <- check_var(vars = g, varn = "g", dataset = dataset,
                       check.names = TRUE, ncols = 1, Yncol = 0,
                       Ynrow = 0, Xnrow = Xnrow, isnumeric = TRUE)

        q <- check_var(vars = q, varn = "q", dataset = dataset,
                       check.names = TRUE, ncols = 1, Yncol = 0,
                       Ynrow = 0, Xnrow = Xnrow, isnumeric = TRUE)

        ind_gr <- check_var(vars = ind_gr, varn = "ind_gr", dataset = dataset,
                            check.names = TRUE, ncols = 1, Yncol = 0, Ynrow = 0,
                            Xnrow = Xnrow, ascharacter = TRUE)

        if (is.null(yearsX)) stop("'yearsX' must be defined")
        if (is.null(subperiodsX)) stop("'subperiodsX' must be defined")
        if (is.null(X_ID_level1)) stop("'X_ID_level1' must be defined")

        datasetX <- data.table(X, yearsX, subperiodsX, X_ID_level1, ind_gr, g, q)
        X <- names(X)
        yearsX <-  ifelse(is.null(names(yearsX)), "yearsX", names(yearsX))
        subperiodsX <- ifelse(is.null(names(subperiodsX)), "subperiodsX", names(subperiodsX))
        X_ID_level1 <- names(X_ID_level1)

        ind_gr <- names(ind_gr)
        g <- "g"
        q <- "q"
        if (!is.null(countryX)) datasetX <- data.table(datasetX, countryX)
        countryX <- names(countryX)

        yearsgender <- paste0(years, "gender")
        dataset <- rbindlist(lapply(1:2, function(i) { dats <- copy(dataset)
                                                       dats[, (yearsgender) := paste0(get(years), "_", i)]
                                                       dats[get(gender) != i, (c(Y, Z)):= 0]
                                                       return(dats)}))

        datasetX <- rbindlist(lapply(1:2, function(i) { dats <- copy(datasetX)
                                                        dats[, (yearsgender) := paste0(get(yearsX), "_", i)]
                                                        return(dats)}))

        year1 <- paste0(unique(dataset[[years]]), "_2")
        year2 <- paste0(unique(dataset[[years]]), "_1")

        rez <- vardchangannual(Y = Y, H = H, PSU = PSU,
                               w_final = w_final, ID_level1 = ID_level1,
                               ID_level2 = ID_level2, Dom = Dom,
                               Z = Z, country = country, years = yearsnew,
                               subperiods = subperiods, dataset = dataset,
                               year1 = year1, year2 = year2, X = X,
                               countryX = NULL, yearsX = yearsnew,
                               subperiodsX = subperiodsX, X_ID_level1 = X_ID_level1,
                               ind_gr = ind_gr, g = g, q = q, datasetX = datasetX,
                               percentratio = percentratio, use.estVar = FALSE,
                               confidence = confidence)

#  list(crossectional_results = crossectional_results,
#       crossectional_var_grad = crossectional_var_grad,
#       vardchanges_grad_var = grad_var,
#       vardchanges_rho = rho,
#      vardchanges_var_tau = var_tau,
#       vardchanges_results = vardchanges_results,
#       X_annual = X_annual, A_matrix = A_matrix,
#       annual_sum = ysum,
#       annual_cros = annual_cros)

