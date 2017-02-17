
vardgpgcrosannual <- function(Y, H, PSU, w_final,
                            ID_level1, ID_level2,
                            Dom = NULL, Z = NULL, 
                            country = NULL, years,
                            subperiods, dataset = NULL,
                            X = NULL, countryX = NULL,
                            yearsX = NULL, subperiodsX = NULL,
                            X_ID_level1 = NULL, ind_gr = NULL,
                            g = NULL, q = NULL, datasetX = NULL,
                            percentratio = 1, use.estVar = FALSE,
                            confidence = 0.95) {
 
  ### Checking
  Y <- check_var(vars = Y, varn = "Y", dataset = dataset, data_type = "data.table", 
                 check.names = TRUE, ncols = 0, Yncol = 0, Ynrow = 0,
                 isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  Yncol <- ncol(Y)

  H <- check_var(vars = H, varn = "H", dataset = dataset, data_type = "data.table", 
                 check.names = TRUE, ncols = 1, Yncol = 0, Ynrow = Ynrow,
                 isnumeric = FALSE, ascharacter = TRUE)

  ID_level1 <- check_var(vars = ID_level1, varn = "ID_level1", dataset = dataset,
                         data_type = "data.table", check.names = TRUE, ncols = 1,
                         Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                         ascharacter = TRUE)

  PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                   data_type = "data.table", check.names = TRUE,
                   ncol = 1, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                   ascharacter = TRUE, namesID1 = names(ID_level1))

  w_final <- check_var(vars = w_final, varn = "w_final", dataset = dataset,
                       data_type = "data.table", check.names = TRUE, ncols = 1,
                       Yncol = 0, Ynrow = Ynrow, isnumeric = TRUE,
                       ascharacter = FALSE)

  ID_level2 <- check_var(vars = ID_level2, varn = "ID_level2", dataset = dataset,
                         data_type = "data.table", check.names = TRUE, ncols = 1,
                         Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                         ascharacter = TRUE, namesID1 = names(ID_level1))

  country <- check_var(vars = country, varn = "country", dataset = dataset,
                     data_type = "data.table", check.names = TRUE, ncols = 1,
                     Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                     ascharacter = TRUE, dif_name = "percoun")


 if (is.null(Z)) stop("'Z' must be defined")
 if (is.null(gender)) stop("'gender' must be defined")
 if (is.null(years)) stop("'years' must be defined")
 if (is.null(subperiods)) stop("'subperiods' must be defined")

 if(is.null(dataset)) {dataset <- data.table(Y, H, ID_level1,
                                             ID_level2, PSU,
                                             w_final, Z, gender)
                       Y <- names(Y)
                       H <- names(H)
                       ID_level1 <- names(ID_level1)
                       ID_level2 <- names(ID_level2)
                       PSU <- names(PSU)
                       w_final <- ifelse(is.null(names(w_final)), "w_final", names(w_final))
                       Z <- names(Z)
                       gender <- ifelse(is.null(names(gender)), "gender", names(gender))
                       if (!is.null(country)) dataset <- data.table(dataset, country)
                       country <- names(country) 
                       dataset <- data.table(dataset, years, subperiods)
                       years <- names(years)
                       subperiods <- names(subperiods)                       
                       if (!is.null(Dom)) dataset <- data.table(dataset, Dom)
                       Dom <- names(Dom)  }
 
 if(is.null(datasetX)&!is.null(X)) {
                       if (is.null(X)) stop("'X' must be defined")
                       if (is.null(yearsX)) stop("'yearsX' must be defined")
                       if (is.null(subperiodsX)) stop("'subperiodsX' must be defined")
                       if (is.null(X_ID_level1)) stop("'X_ID_level1' must be defined")
                       if (is.null(g)) stop("'g' must be defined")
                       datasetX <- data.table(X, yearsX, 
                                             subperiodsX, X_ID_level1)
                       X <- names(X)
                       yearsX <-  ifelse(is.null(names(yearsX)), "yearsX", names(yearsX))
                       subperiodsX <- ifelse(is.null(names(subperiodsX)), "subperiodsX", names(subperiodsX))
                       X_ID_level1 <- names(X_ID_level1)

                       ind_gr <- NULL
                       if (!is.null(ind_gr)) { datasetX <- data.table(datasetX, ind_gr)
                                               ind_gr <- ifelse(is.null(names(ind_gr)), NULL, names(ind_gr)) }
                       if (!is.null(countryX)) datasetX <- data.table(datasetX, countryX)
                       countryX <- names(countryX) 
                       datasetX <- data.table(datasetX, g)
                       g <- ifelse(is.null(names(g)), "g", names(g))
                       q <- NULL
                       if (!is.null(q)) { datasetX <- data.table(datasetX, q) 
                                          q <- ifelse(is.null(names(q)), "q", names(q))}}


 yearsgender <- paste0(years, "gender")
 dataset <- rbindlist(lapply(1:2, function(i) { dats <- copy(dataset)
                                                dats[, (yearsgender):= paste0(get(years), "_", i)]
                                                dats[get(gender) != i, (c(Y, Z)):= 0]
                                                return(dats)}))

 datasetX <- rbindlist(lapply(1:2, function(i) { dats <- copy(datasetX)
                                                 dats[, (yearsgender):= paste0(get(yearsX), "_", i)]
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

