#*********************************************************************************************************
#*********************************************************************************************************
#*********************************************************************************************************
#***                                                                                                   ***
#***                                                                                                   ***
#***                    LINEARIZATION OF THE RELATIVE MEDIAN AT-RISK-OF-POVERTY GAP                    ***
#***                                                                                                   ***
#***                                                                                                   ***
#*********************************************************************************************************
#*********************************************************************************************************
#*********************************************************************************************************

linrmpg <- function(inc, id, weight=NULL, sort=NULL, Dom=NULL,
                    period=NULL, dataset = NULL, percentage=60, 
                    order_quant=50, var_name="lin_rmpg") {
 
   ## initializations
   
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   # check 'p'
   p <- percentage
   if (!is.numeric(p) || length(p) != 1 || p[1] < 0 || p[1] > 100) {
          stop("'percentage' must be a numeric value in [0,100]")
      } else p <- percentage[1]

   # check 'order_quant'

   oq <- order_quant
   if (!is.numeric(oq) || length(oq) != 1 || oq[1] < 0 || oq[1] > 100) {
          stop("'order_quant' must be a numeric value in [0,100]")
      } else order_quant <- order_quant[1]

   if (!is.null(dataset)) {
        dataset <- data.frame(dataset)
        if (checker(inc, dataset, "inc")) inc <- dataset[, inc] 

        if (!is.null(id)) {
             id2 <- id
             if (checker(id, dataset, "id")) id <- data.frame(dataset[, id], stringsAsFactors=FALSE)
             names(id) <- id2 }

        if(!is.null(weight)) {
            if (checker(weight, dataset, "weight")) weight <- dataset[, weight] }

        if(!is.null(sort)) {
            if (checker(sort, dataset, "sort")) sort <- dataset[, sort] }

       if (!is.null(period)) {
            aperiod <- period  
            if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
            if (min(period %in% names(dataset))==1) {
                                period <- data.frame(dataset[, period], stringsAsFactors=FALSE)
                                names(period) <- aperiod }}
        if (!is.null(Dom)) {
             Dom2<-Dom
             if (checker(Dom, dataset, "Dom")) {
                     Dom <- as.data.frame(dataset[, Dom], stringsAsFactors=FALSE) 
                     names(Dom) <- Dom2 }    }
      }

   # check vectors
   # inc
   inc <- data.frame(inc)
   n <- nrow(inc)                     
   if (ncol(inc) != 1) stop("'inc' must be vector or 1 column data.frame, matrix, data.table")
   inc <- inc[,1]
   if (!is.numeric(inc)) stop("'inc' must be numerical")
   if (any(is.na(inc))) stop("'inc' has unknown values")
 
   # weight
   weight <- data.frame(weight)
   if (is.null(weight)) weight <- data.frame(rep.int(1, n))
   if (nrow(weight) != n) stop("'weight' must be the same length as 'inc'")
   if (ncol(weight) != 1) stop("'weight' must be vector or 1 column data.frame, matrix, data.table")
   weight <- weight[,1]
   if (!is.numeric(weight)) stop("'weight' must be numerical")
   if (any(is.na(weight))) stop("'weight' has unknown values")

   # sort
   if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
         stop("'sort' must be a vector or ordered factor") }
   if (!is.null(sort) && length(sort) != n) stop("'sort' must be the same length as 'inc'")     
   
   # period     
   if (!is.null(period)) {
       period <- data.table(period)
       if (any(duplicated(names(period)))) 
                 stop("'period' are duplicate column names: ", 
                      paste(names(period)[duplicated(names(period))], collapse = ","))
       if (nrow(period) != n) stop("'period' must be the same length as 'inc'")
       if(any(is.na(period))) stop("'period' has unknown values")  
   }   

   # id
   if (is.null(id)) id <- 1:n
   id <- data.table(id)
   if (any(is.na(id))) stop("'id' has unknown values")
   if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
   if (nrow(id) != n) stop("'id' must be the same length as 'inc'")
   if (is.null(names(id))||(names(id)=="id")) setnames(id,names(id),"ID")
   if (is.null(period)){ if (any(duplicated(id))) stop("'id' are duplicate values") 
                       } else {
                          id1 <- data.table(period, id)
                          if (any(duplicated(id1))) stop("'id' by period are duplicate values")
                         }

   # Dom     
   if (!is.null(Dom)) {
             Dom <- data.table(Dom)
             if (any(duplicated(names(Dom)))) 
                 stop("'Dom' are duplicate column names: ", 
                      paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
             if (is.null(names(Dom))) stop("'Dom' must be colnames")
             if (nrow(Dom) != n) stop("'Dom' must be the same length as 'inc'")
       }

    ## computations
    ind0 <- rep.int(1, n)
    period_agg <- period1 <- NULL
    if (!is.null(period)) { period1 <- copy(period)
                            period_agg <- data.table(unique(period))
                        } else period1 <- data.table(ind=ind0)
    period1_agg <- data.table(unique(period1))

    # Relative median at-risk-of-poverty gap by domain (if requested)

    quantile <- incPercentile(inc = inc, weights = weight,
                              sort = sort, Dom = NULL,
                              period = period,
                              k = order_quant,
                              dataset = NULL)
    quantile <- data.table(quantile)
    setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
    if (ncol(quantile)>1) setkeyv(quantile, head(names(quantile), -1))
    threshold <- copy(quantile)
    threshold[, threshold:=p/100 * quantile]
    threshold[, quantile:=NULL]
   
    rmpgap_id <- id
    if (!is.null(period)) rmpgap_id <- data.table(period, rmpgap_id)

    if (!is.null(Dom)) {
         Dom_agg <- data.table(unique(Dom))
         setkeyv(Dom_agg, names(Dom)) 

         rmpgap_v <- c()
         rmpgap_m <- copy(rmpgap_id)
 
         for(i in 1:nrow(Dom_agg)) {
                 g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
                 var_nams <- do.call(paste, as.list(c(g, sep="__")))
                 ind <- (rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))
                 

                 rmpgapl <- lapply(1:nrow(period1_agg), function(j) {
                      if (!is.null(period)) { 
                               rown <- cbind(period_agg[j], Dom_agg[i])
                               setkeyv(rown, names(rown))
                               rown2 <- copy(rown)
                               rown <- merge(rown, quantile, all.x=TRUE)
                          } else { rown <- quantile
                                   rown2 <- Dom_agg[i] }

                       indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                       rmpgap_l <- linrmpgCalc(inco=inc[indj],
                                               ids=rmpgap_id[indj],
                                               wght=weight[indj],
                                               sort=sort[indj],
                                               ind=ind[indj],
                                               percentag=p,
                                               order_quants=order_quant,
                                               quant_val=rown[["quantile"]]) 

                      list(rmpgap=data.table(rown2, rmpgap=rmpgap_l$rmpgap),
                           lin=rmpgap_l$lin)
                 })

              rmpgaps <- rbindlist(lapply(rmpgapl, function(x) x[[1]]))
              rmpgaplin <- rbindlist(lapply(rmpgapl, function(x) x[[2]]))

              setnames(rmpgaplin, names(rmpgaplin), c(names(rmpgap_id), var_nams))
              setkeyv(rmpgap_m, names(rmpgap_id))
              setkeyv(rmpgaplin, names(rmpgap_id))
              rmpgap_m <- merge(rmpgap_m, rmpgaplin, all.x=T)
              rmpgap_v <- rbind(rmpgap_v, rmpgaps) 
           }
 } else {rmpgap_l <- lapply(1:nrow(period1_agg), function(j) {
                           if (!is.null(period)) { 
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x=TRUE)
                                       } else rown <- quantile
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))
      
                           rmpgapl <- linrmpgCalc(inco=inc[indj], ids=rmpgap_id[indj],
                                                  wght=weight[indj], sort=sort[indj],
                                                  ind=ind0[indj], percentag=p,
                                                  order_quants=order_quant,
                                                  quant_val=rown[["quantile"]])  
                           if (!is.null(period)) {
                               rmpgap_v <- data.table(period_agg[j], rmpgap=rmpgapl$rmpgap)
                             } else rmpgap_v <- data.table(rmpgap=rmpgapl$rmpgap)
                          list(rmpgap_v=rmpgap_v, lin=rmpgapl$lin)
                       })
               rmpgap_v <- rbindlist(lapply(rmpgap_l, function(x) x[[1]]))
               rmpgap_m <- rbindlist(lapply(rmpgap_l, function(x) x[[2]]))
               setnames(rmpgap_m, names(rmpgap_m), c(names(rmpgap_id), var_name))
         }
   rmpgap_m[is.na(rmpgap_m)] <- 0
   setkeyv(rmpgap_m, names(rmpgap_id))
   return(list(quantile=quantile, threshold=threshold, value=rmpgap_v, lin=rmpgap_m)) 
}

## workhorse
linrmpgCalc <- function(inco, ids, wght, sort, ind, percentag, order_quants, quant_val) {
    wt <- ind*wght   
    thres_val <- percentag/100 * quant_val
    N0 <- sum(wght)                # Estimated whole population size
    N <- sum(wt)                # Estimated (sub)population size
    poor <- (inco<=thres_val)*ind

    inc1 <- inco[poor==1]
    wght1 <- wght[poor==1]
    sort1 <- sort[poor==1]

    rate_val <- sum(wt*poor)/N  # Estimated poverty rate
    rate_val_pr <- 100*rate_val  # Estimated poverty rate
 
    poor_people_median <- incPercentile(inc=inc1, weights=wght1,
                                        sort=sort1, Dom=NULL,
                                        period=NULL, k=order_quants,
                                        dataset=NULL)
    poor_people_median <- poor_people_median[[paste0("x", order_quants)]]
#*************************************************************************************
 #**          LINEARIZATION OF THE MEDIAN INCOME BELOW THE POVERTY THRESHOLD         **
 #*************************************************************************************
    h <- sqrt((sum(wght*inco*inco)-sum(wght*inco)*sum(wght*inco)/sum(wght))/sum(wght))/exp(0.2*log(sum(wght))) 
   # h=S/N^(1/5)
 
 #--------------------------------------------------
 #----- LINEARIZATION OF THE POVERTY THRESHOLD -----
 #--------------------------------------------------
    u1 <- (quant_val-inco)/h
    vect_f1 <- exp(-(u1^2)/2)/sqrt(2*pi)
    f_quant1 <- sum(vect_f1*wght)/(N0*h) 

    lin_thres <- -(percentag/100)*(1/N0)*((inco<=quant_val)-order_quants/100)/f_quant1
 # ---------------------------------------------
 # ----- LINEARIZATION OF THE POVERTY RATE -----
 # ---------------------------------------------
    u2 <- (thres_val-inco)/h
    vect_f2 <- exp(-(u2^2)/2)/sqrt(2*pi)
    f_quant2 <- sum(vect_f2*wt)/(N*h) 

    lin_rate <- (1/N)*ind*((inco<=thres_val)-rate_val)+f_quant2*lin_thres
 # --------------------------------------------------------
 # ----- LINEARIZATION OF POOR PEOPLE'S MEDIAN INCOME -----
 # --------------------------------------------------------
     u3 <- (poor_people_median-inco)/h
     vect_f3 <- exp(-(u3^2)/2)/sqrt(2*pi)
     f_quant3 <- sum(vect_f3*wt)/(N*h) 

     lin_median <- (0.5*lin_rate-(1/N)*ind*((inco<=poor_people_median)-0.5*rate_val))/f_quant3

 #*****************************************************************************************
 #                   LINEARIZED VARIABLE OF THE RELATIVE MEDIAN GAP (IN %)                *
 #*****************************************************************************************
     lin_gap <- 100*(poor_people_median*lin_thres/(thres_val*thres_val)-lin_median/thres_val)
     
     rmpgap <- 100 - 100*poor_people_median/thres_val

     lin_id <- data.table(ids, lin_gap)

     return(list(rmpgap=rmpgap, lin=lin_id))
}

