#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************
#***                                                                                         ***
#***                                                                                         ***
#***                    LINEARIZATION OF THE AT-RISK-OF-POVERTY THRESHOLD                    ***
#***                                                                                         ***
#***                                                                                         ***
#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************

linarpt <- function(Y, id = NULL, weight = NULL, sort = NULL,
                    Dom = NULL, period=NULL, dataset = NULL,
                    percentage = 60, order_quant = 50L,
                    var_name="lin_arpt", checking = TRUE) {

   ## initializations
   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if (checking) {
       percentage <- check_var(vars = percentage, varn = "percentage",
                               varntype = "numeric0100") 

       order_quant <- check_var(vars = order_quant, varn = "order_quant",
                                varntype = "integer0100")

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
   dataset <- NULL

   ## computations
   ind0 <- rep.int(1, length(Y))
   period_agg <- period1 <- NULL
   if (!is.null(period)) { period1 <- copy(period)
                           period_agg <- data.table(unique(period))
                       } else period1 <- data.table(ind = ind0)
   period1_agg <- data.table(unique(period1))

   # ARPT by domain (if requested)

   quantile <- incPercentile(Y = Y,
                             weights = weight,
                             sort = sort,
                             Dom = Dom,
                             period = period,
                             k = order_quant,
                             dataset = NULL,
                             checking = FALSE)

   quantile <- data.table(quantile)
   setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
   if (ncol(quantile) > 1) setkeyv(quantile, head(names(quantile), -1))
   threshold <- copy(quantile)
   threshold[, threshold := percentage / 100 * quantile]
   threshold[, quantile := NULL]

   arpt_id <- id
   if (!is.null(period)) arpt_id <- data.table(arpt_id, period)

   if(!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom_agg))

       arpt_m <- copy(arpt_id)
       for(i in 1 : nrow(Dom_agg)) {
             g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
             var_nams <- do.call(paste, as.list(c(g, sep = "__")))
             ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))
             arpt_l <- lapply(1:nrow(period1_agg), function(j) {
                              if (!is.null(period)) {
                                      rown <- cbind(period_agg[j], Dom_agg[i])
                                      } else rown <- Dom_agg[i]

                              setkeyv(rown, names(rown))
                              rown2 <- copy(rown)
                              rown <- merge(rown, quantile, all.x = TRUE)
                              ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                              arptl <- arptlinCalc(inco = Y[ind2],
                                                   ids = arpt_id[ind2],
                                                   wght = weight[ind2],
                                                   indicator = ind[ind2],
                                                   order_quan = order_quant,
                                                   quant_val = rown[["quantile"]],
                                                   percentag = percentage)
                               })
           arptl <- rbindlist(arpt_l)
           setnames(arptl, names(arptl), c(names(arpt_id), var_nams))
           arpt_m <- merge(arpt_m, arptl, all.x = TRUE, by = names(arpt_id))
        }
    } else { arptl <- lapply(1 : nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x = TRUE)
                                       } else rown <- quantile
                           ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           arptl <- arptlinCalc(inco = Y[ind2],
                                                ids = arpt_id[ind2],
                                                wght = weight[ind2],
                                                indicator = ind0[ind2],
                                                order_quan = order_quant,
                                                quant_val = rown[["quantile"]],
                                                percentag = percentage)
                       })
               arpt_m <- rbindlist(arptl)
               setnames(arpt_m, names(arpt_m), c(names(arpt_id), var_name))
            }
    arpt_m[is.na(arpt_m)] <- 0
    setkeyv(arpt_m, names(arpt_id))
    return(list(quantile = quantile, value = threshold, lin = arpt_m))
 }

    ## workhorse
arptlinCalc <- function(inco, ids, wght, indicator, order_quan, quant_val, percentag) {
    wt <- wght * indicator
    N <- sum(wt); # Estimated (sub)population size

    # h=S/N^(1/5)
    h <- sqrt((sum(wght * inco * inco) - sum(wght * inco) * sum(wght * inco) / sum(wght)) / sum(wght)) / exp(0.2 * log(sum(wght)))

    u <- (quant_val - inco) / h
    vect_f <- exp(-(u^2) / 2) / sqrt(2 * pi)
    f_quant <- sum(vect_f * wt) / (N * h) # Estimate of F'(quantile)

 #****************************************************************************************
 #*                    LINEARIZED VARIABLE OF THE POVERTY THRESHOLD                      *
 #****************************************************************************************
    lin <- - (percentag / 100) * (1 / N) * indicator * ((inco <= quant_val) - order_quan / 100) / f_quant
    lin_id <- data.table(ids, lin)
    return(lin_id)
}

