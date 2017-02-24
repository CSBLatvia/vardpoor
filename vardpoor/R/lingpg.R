#******************************************************************************************
#******************************************************************************************
#******************************************************************************************
#***                                                                                    ***
#***                                                                                    ***
#***                        LINEARIZATION OF THE GENDER PAY GAP                         ***
#***                                                                                    ***
#***                                                                                    ***
#******************************************************************************************
#******************************************************************************************
#******************************************************************************************

lingpg <- function(Y, gender = NULL, id = NULL,
                   weight = NULL, sort = NULL,
                   Dom = NULL, period = NULL,
                   dataset = NULL, var_name = "lin_gpg",
                   checking = TRUE) {

   ## initializations

   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
          stop("'var_name' must have defined name of the linearized variable")}

   if (is.null(gender)) stop("'gender' must be supplied")

   if (checking) {
          Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                         ncols = 1, isnumeric = TRUE,
                         isvector = TRUE, grepls = "__")
          Ynrow <- length(Y)

          gender <- check_var(vars = gender, varn = "gender",
                              dataset = dataset, ncols = 1,
                              Ynrow = Ynrow, isnumeric = TRUE,
                              isvector = TRUE)

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

  # GPG by domain (if requested)
  gpg_id <- id
  if (!is.null(period)) gpg_id <- data.table(gpg_id, period)

  if(!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom_agg))

       gpg_v <- c()
       gpg_m <- copy(gpg_id)

       for(i in 1 : nrow(Dom_agg)) {
           g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
           var_nams <- do.call(paste, as.list(c(g, sep = "__")))
           indi <- (rowSums(Dom  ==  Dom_agg[i,][ind0,]) == ncol(Dom))

           gpg_l <- lapply(1 : nrow(period1_agg), function(j) {
                indj <- ((rowSums(period1 == period1_agg[j,][ind0,])  ==  ncol(period1))&(indi))
                if (!is.null(period)) { rown <- cbind(period_agg[j], Dom_agg[i])
                                    } else rown <- Dom_agg[i]
                gpgl <- linGapCalc(x = Y[indj], gend = gender[indj],
                                   ids = gpg_id[indj], weights = weight[indj],
                                   sort = sort[indj])
                list(data.table(rown, gpg = gpgl$gpg_pr), gpgl$lin)
             })

           gpgs <- rbindlist(lapply(gpg_l, function(x) x[[1]]))
           gpglin <- rbindlist(lapply(gpg_l, function(x) x[[2]]))

           setnames(gpglin, names(gpglin), c(names(gpg_id), var_nams))
           gpg_m <- merge(gpg_m, gpglin, all.x = TRUE, by = names(gpg_id))
           gpg_v <- rbind(gpg_v, gpgs)
         }
     } else { gpg_l <- lapply(1 : nrow(period1_agg), function(j) {
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           gpg_l <- linGapCalc(x = Y[indj], gend = gender[indj],
                                               ids = gpg_id[indj], weights = weight[indj],
                                               sort = sort[indj])

                           if (!is.null(period)) {
                                    gpgs <- data.table(period_agg[j], gpg = gpg_l$gpg_pr)
                              } else gpgs <- data.table(gpg = gpg_l$gpg_pr)
                           list(gpg = gpgs, lin = gpg_l$lin)
                       })
               gpg_v <- rbindlist(lapply(gpg_l, function(x) x[[1]]))
               gpg_m <- rbindlist(lapply(gpg_l, function(x) x[[2]]))
               setnames(gpg_m, names(gpg_m), c(names(gpg_id), var_name))
            }
    gpg_m[is.na(gpg_m)] <- 0
    setkeyv(gpg_m, names(gpg_id))
    return(list(value = gpg_v, lin = gpg_m))
 }


  ## workhorse
 linGapCalc <- function(x, gend, ids, weights = NULL, sort = NULL) {
    if(is.null(gend)) stop("'gender' must be supplied")
    if (length(gend) != length(x)) stop("'x' is not the same as 'gend'")
    if (length(gend) != length(weights)) stop("'weights' is not the same as 'gend'")

    if (is.null(weights)) weights <- rep.int(1, length(x))  # equal weights

    indic_men <- ifelse(gend == 1, 1, 0)
    indic_women <- ifelse(gend == 2, 1, 0)

    x[is.na(x)] <- 0

    Nmen <- sum(weights * indic_men)
    Nwomen <- sum(weights * indic_women)
    SINCmen <- sum(weights * x * indic_men)
    SINCwomen <- sum(weights * x * indic_women)

    Num <- SINCmen / Nmen - SINCwomen / Nwomen
    Den <- SINCmen / Nmen
    gpg <- Num / Den # Estimated gender pay gap
    gpg_pr <- gpg * 100

 #-------------------------- Linearized variable (in %) -----------------------
    lin <- 100 * (1 - gpg) * ((indic_women / Nwomen) - (indic_men / Nmen) + ((x * indic_men) / SINCmen) - ((x * indic_women) / SINCwomen))
 #-----------------------------------------------------------------------------

    if (length(unique(gend)) != 2 | is.nan(gpg)) gpg_pr <- lin <- 0

    lin_id <- data.table(ids, lin)

    gpg <- data.table(gpg_pr = gpg_pr)
    return(list(gpg_pr = gpg_pr, lin = lin_id))
 }

