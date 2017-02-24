# ************************************************************************
# ************************************************************************
# ************************************************************************
# ***                                                                  ***
# ***                                                                  ***
# ***            LINEARIZATION OF THE GINI COEFFICIENT II              ***
# ***                                                                  ***
# ***                                                                  ***
# ************************************************************************
# ************************************************************************
# ************************************************************************
# ************************************************************************


lingini2 <- function(Y, id = NULL, weight = NULL,
                     sort = NULL, Dom = NULL, period = NULL,
                     dataset = NULL, var_name = "lin_gini2",
                     checking = TRUE) {

   ## initializations
   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if (checking) {
          Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                         ncols = 1, isnumeric = TRUE,
                         isvector = TRUE, grepls = "__")
          Ynrow <- length(Y)

          weight <- check_var(vars = weight, varn = "weight",
                              dataset = dataset, ncols = 1,
                              Ynrow = Ynrow, isnumeric = TRUE,
                              isvector = TRUE)

          sort <- check_var(vars = sort, varn = "sort",
                            dataset = dataset, ncols = 1,
                            Ynrow = Ynrow, mustbedefined = FALSE,
                            isnumeric = TRUE, isvector = TRUE)

          period <- check_var(vars = period, varn = "period",
                              dataset = dataset, Ynrow = Ynrow,
                              ischaracter = TRUE, mustbedefined = FALSE,
                              duplicatednames = TRUE)

          Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                           Ynrow = Ynrow, ischaracter = TRUE,
                           mustbedefined = FALSE, duplicatednames = TRUE,
                           grepls = "__")

          id <- check_var(vars = id, varn = "id", dataset = dataset,
                          ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                          periods = period)
     }


   ## computations
   ind0 <- rep.int(1, length(Y))
   period_agg <- period1 <- NULL
   if (!is.null(period)) { period1 <- copy(period)
                           period_agg <- data.table(unique(period))
                       } else period1 <- data.table(ind = ind0)
   period1_agg <- data.table(unique(period1))


   # Gini by domain (if requested)
   gini_id <- id
   if (!is.null(period)) gini_id <- data.table(period, gini_id)

   if (!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))

        Gini <- c()
        gini_m <- copy(gini_id)
        for(i in 1 : nrow(Dom_agg)) {
            g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
            var_nams <- do.call(paste, as.list(c(g, sep = "__")))
            indi <- (rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

            gini_l <- lapply(1 : nrow(period1_agg), function(j) {
               indj <- ((rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))&(indi))
               if (!is.null(period)) { rown <- cbind(period_agg[j], Dom_agg[i])
                                     } else rown <- Dom_agg[i]

               ginil <- lingini2Calc(x = Y[indj],
                                     ids = gini_id[indj],
                                     weights = weight[indj],
                                     sort = sort[indj])
               list(data.table(rown, ginil$Gini), ginil$lin)
              })

            giniv <- rbindlist(lapply(gini_l, function(x) x[[1]]))
            ginilin <- rbindlist(lapply(gini_l, function(x) x[[2]]))

            setnames(ginilin, names(ginilin), c(names(gini_id), var_nams))
            gini_m <- merge(gini_m, ginilin, all = TRUE, by = names(gini_id))
            Gini <- rbind(Gini, giniv)
         }
     } else { gini_l <- lapply(1 : nrow(period1_agg), function(j) {
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))
                           ginil <- lingini2Calc(x = Y[indj],
                                                 ids = gini_id[indj],
                                                 weights = weight[indj],
                                                 sort = sort[indj])
                           if (!is.null(period)) {
                                  list(data.table(period_agg[j], ginil$Gini), ginil$lin)
                                }  else ginil
                         })
            Gini <- rbindlist(lapply(gini_l, function(x) x[[1]]))
            gini_m <- rbindlist(lapply(gini_l, function(x) x[[2]]))
            setnames(gini_m, names(gini_m), c(names(gini_id), var_name))
     }
    gini_m[is.na(gini_m)] <- 0
    setkeyv(gini_m, names(gini_id))
    return(list(value = Gini, lin = gini_m))
}


# workhorse
lingini2Calc <- function(x, ids, weights = NULL, sort = NULL) {

    # sort values and weights
    order <- if(is.null(sort)) order(x) else order(x, sort)
    x <- x[order]  # order values
    ids <- ids[order]  # order values
    if (is.null(weights)) { weights <- rep.int(1, length(x))  # equal weights
     } else weights <- weights[order]  # order weights

    ## calculations
    wx <- weights * x       # weighted values
    N <- sum(weights)     # Estimated population size
    cw <- cumsum(weights)   # cumulative sum of weights
    T <- sum(wx)             # Estimated total income

    dt <- data.table(x = x, weights = weights, wx = wx, key = "x")
    weights0 <- wx0 <- NULL
    dt1 <- dt[, list(weights0 = sum(weights),
                wx0 = sum(wx)), keyby = x][,
                Nk := cumsum(weights0)][,
                wx1 := cumsum(wx0)]
    dt <- merge(dt, dt1)

    # Nk - estimation of the cumulative distribution function
    Nk <- dt[["Nk"]]

    # wx - weighted partial sum
    wx1 <- dt[["wx1"]]
    dt <- dt1 <- NULL

    Num_eu <- 2 * sum(wx * cw) - sum(weights^2 * x)
    Num <- 2 * sum(wx * Nk) - sum(weights^2 * x)
    Den <- N * T

    Gini_eu <- 100 * (Num_eu / Den - 1)
    Gini <- Num / Den - 1

    Gini_pr <- 100 * Gini


    # LINEARIZED VARIABLE OF THE GINI COEFFICIENT (IN %)

    lin <- 100 * (2 * Nk * (x - wx1 / Nk) + T - N * x - Gini * (T + N * x)) / (N * T)

    if (is.nan(Gini))  Gini_pr <- lin <- 0

    Gini_pr <- data.table(Gini = Gini_pr, Gini_eu = Gini_eu)
    lin_id <- data.table(ids, lin = lin)

    return(list(Gini=Gini_pr, lin = lin_id))
}

