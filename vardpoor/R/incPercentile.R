
incPercentile <- function(Y, weights = NULL, sort = NULL,
                          Dom = NULL, period = NULL,
                          k = c(20, 80), dataset = NULL,
                          checking = TRUE) {

   ## initializations
   if (checking) {

         Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                        ncols = 1, isnumeric = TRUE,
                        isvector = TRUE, grepls = "__")
         Ynrow <- length(Y)

         weights <- check_var(vars = weights, varn = "weights",
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
     }
   dataset <- NULL
   namesDom <- names(Dom)

   if (!is.null(period)) {
       if (!is.null(Dom)) { Dom <- data.table(period, Dom)
                 } else Dom <- period }

   # Percentiles by domain (if requested)

   N <- NULL
   if(!is.null(Dom)) {
        Dom_app <- do.call("paste", c(as.list(Dom), sep = "__"))
        q1 <- lapply(split(Dom[, .I], Dom_app), function(i) {
               Yind <- Y[i]
               weightsind <- weights[i]
               sortind <- sort[i]
               order <- if(is.null(sortind)) order(Yind) else order(Yind, sortind)
               Yind <- Yind[order]
               weightsind <- weightsind[order]  # also works if 'weights' is NULL
               percentile <- weightedQuantile(Yind, weightsind, probs = k / 100,
                                              sorted = FALSE, na.rm = FALSE)
               q <- data.table(Dom[i][1], t(percentile))})
        q <- rbindlist(q1)
        setnames(q, names(q)[ncol(Dom) + 1 : length(k)], paste0("x", k))
        if (!is.null(period) & !is.null(namesDom)) {
              q1 <-  q[, .N, keyby = namesDom][, N := NULL]
              q2 <- q[, .N, by = names(period)][, N := NULL]
              qrs <- rbindlist(lapply(1:nrow(q2), function(i) {
                                   data.table(q2[i], q1) }))
              qrs[, (c(paste0("x", k))) := 0]
              qrs <- rbind(q, qrs)
              q <- qrs[, lapply(.SD, sum), keyby = names(Dom), .SDcols = paste0("x", k)]
            }
         setkeyv(q, names(Dom))
     } else {  order <- if(is.null(sort)) order(Y) else order(Y, sort)
               Y <- Y[order]
               weights <- weights[order]  # also works if 'weights' is NULL
               percentile <- weightedQuantile(Y, weights, probs = k / 100,
                                              sorted = TRUE, na.rm = FALSE)
               q <- data.table(t(percentile))
               setnames(q, names(q)[1 : length(k)], paste0("x", k))
     }

   ## return results
   return(q[])
}

