#******************************************************************************************
#******************************************************************************************
#******************************************************************************************
#***                                                                                    ***
#***                                                                                    ***
#***                    LINEARIZATION OF THE AT-RISK-OF-POVERTY RATE                    ***
#***                                                                                    ***
#***                                                                                    ***
#******************************************************************************************
#******************************************************************************************
#******************************************************************************************

linarpr <- function(inc, id, weight=NULL, income_thres=NULL, wght_thres=NULL,
                 sort=NULL, Dom=NULL, period=NULL, dataset = NULL, percentage=60, 
                 order_quant=50, na.rm=FALSE, var_name="lin_arpr") {
 
   ## initializations
   if (min(dim(data.table(var_name))==1)!=1) {
       stop("'var_name' must have defined one name of the linearized variable")}

   # check 'p'
   p <- percentage
   if(!is.numeric(p) || length(p) != 1 || p[1] < 0 || p[1] > 100) {
          stop("'percentage' must be a numeric value in [0,100]")
      } else p <- percentage[1]

   # check 'order_quant'

   oq <- order_quant
   if(!is.numeric(oq) || length(oq) != 1 || oq[1] < 0 || oq[1] > 100) {
          stop("'order_quant' must be a numeric value in [0,100]")
      } else order_quant <- order_quant[1]

   if(!is.null(dataset)) {
       dataset <- data.frame(dataset)
       if (checker(inc, dataset, "inc")) inc <- dataset[, inc] 

       if(!is.null(id)) {
          id2 <- id
          if (checker(id, dataset, "id")) id <- data.frame(dataset[, id], stringsAsFactors=FALSE)
          names(id) <- id2 }

       if(!is.null(weight)) {
           if (checker(weight, dataset, "weight")) weight <- dataset[, weight] }

       if(!is.null(income_thres)) {
           if (checker(income_thres, dataset, "income_thres")) income_thres <- dataset[, income_thres] }
     
       if(!is.null(wght_thres)) {
           if (checker(wght_thres, dataset, "wght_thres")) wght_thres <- dataset[, wght_thres] }

       if(!is.null(sort)) {
           if (checker(sort, dataset, "sort")) sort <- dataset[, sort] }

       if (!is.null(period)) {
            aperiod <- period  
            if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
            if (min(period %in% names(dataset))==1) {
                                period <- data.frame(dataset[, period], stringsAsFactors=FALSE)
                                names(period) <- aperiod }}

       if (!is.null(Dom)) {
            Dom2 <- Dom
            if (checker(Dom, dataset, "Dom")) {
                    Dom <- data.table(dataset[, Dom]) 
                    setnames(Dom, names(Dom), Dom2) }    }
      }

   # check vectors
   # inc
   inc <- data.frame(inc)
   n <- nrow(inc)
   if (ncol(inc) != 1) stop("'inc' must be vector or 1 column data.frame, matrix, data.table")
   inc <- inc[,1]
   if(!is.numeric(inc)) stop("'inc' must be numerical")
   if (any(is.na(inc))) warning("'inc' has unknown values")

   # id
   if (is.null(id)) id <- 1:n
   id <- data.table(id)
   if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
   if (nrow(id) != n) stop("'id' must be the same length as 'inc'")
   if (is.null(names(id))||(names(id)=="id")) setnames(id,names(id),"ID")
   if (any(duplicated(id))) stop("'id' are duplicate values: ",
                                  paste(id[duplicated(id)], collapse = ",")) 

   # weight
   weight <- data.frame(weight)
   if (is.null(weight)) weight <- data.frame(rep.int(1, n))
   if (nrow(weight) != n) stop("'weight' must be the same length as 'inc'")
   if (ncol(weight) != 1) stop("'weight' must be vector or 1 column data.frame, matrix, data.table")
   weight <- weight[,1]
   if (!is.numeric(weight)) stop("'weight' must be numerical")

   # income_thres
   if (is.null(income_thres)) income_thres <- inc
   income_thres <- data.frame(income_thres)
   if (!is.null(income_thres)) {
        if(nrow(income_thres) != n) stop("'income_thres' must be the same length as 'inc'")
        if (ncol(income_thres) != 1) stop("'income_thres' must be vector or 1 column data.frame, matrix, data.table")
        income_thres <- income_thres[,1]
        if(!is.numeric(income_thres)) stop("'income_thres' must be numerical")
     } 

   # wght_thres
   if (is.null(wght_thres)) wght_thres <- weight
   wght_thres <- data.frame(wght_thres)
   if (nrow(wght_thres) != n) stop("'wght_thres' must be the same length as 'inc'")
   if (ncol(wght_thres) != 1) stop("'wght_thres' must be vector or 1 column data.frame, matrix, data.table")
   wght_thres <- wght_thres[,1]
   if (!is.numeric(wght_thres)) stop("'wght_thres' must be numerical")
    
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
    ind <- rep.int(1,n)
    Dom1 <- Dom
    if (!is.null(period)) {
       if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 

    # ARPR by domain (if requested)

    quantiles <- incPercentile(income_thres, wght_thres, sort=sort,Dom=period, order_quant, dataset = NULL, na.rm=na.rm)
    threshold <- data.table(quantiles)
    threshold[,names(threshold)[ncol(threshold)]:=p/100 * threshold[,ncol(threshold),with=FALSE]]
    setnames(threshold,names(threshold)[ncol(threshold)],"threshold")
   
    if (!is.null(Dom1)) {
        Dom_agg <- data.table(unique(Dom1))
        setkeyv(Dom_agg, names(Dom1))
        if (!is.null(period)) quantil <- merge(Dom_agg, quantiles, by=names(period), sort=F) 
        arpr_v <- c()
        arpr_id <- id
        if (!is.null(period)) arpr_id <- data.table(period, arpr_id)
        if (is.null(period)) ind2 <- rep.int(1, n)
        arpr_m <- arpr_id
      
        for(i in 1:nrow(Dom_agg)) {          
              g <- paste(names(Dom1), as.matrix(Dom_agg[i,]), sep = ".")
              breakdown2 <- do.call(paste, as.list(c(g, sep="__")))
              D <- Dom_agg[i,][rep(1,nrow(Dom1)),]
              ind <- (rowSums(Dom1 == D) == ncol(Dom1))
              quantile <- quantiles
              if (!is.null(period)) {
                   quantile <- data.frame(quantil)[i,ncol(quantil)]
                   ind2 <- (rowSums(period == D[,1:ncol(period),with=F]) == ncol(period)) }

              arpr_l <- arprlinCalc(inc1=inc, ids=arpr_id, wght1=weight, indicator=ind,
                                    indic2=ind2, income_thresh=income_thres,
                                    wght_thresh=wght_thres, percent=p,
                                    order_quants=order_quant, quant_val=quantile, na.rm=na.rm) 
              arprl <- arpr_l$lin
              setnames(arprl,names(arprl), c(names(arpr_id), paste(var_name, breakdown2, sep="__")))
              arpr_m <- merge(arpr_m, arprl, by=names(arpr_id), all.x=T, sort=FALSE)
              arpr_v <- rbind(arpr_v, arpr_l$rate_val_pr) 
           }
       colnames(arpr_v) <- "arpr"
       arpr_v <- data.table(Dom_agg, arpr_v)
     } else { arpr_l <- arprlinCalc(inc1=inc, ids=id, wght1=weight, indicator=ind,
                                    indic2=ind, income_thresh=income_thres,
                                    wght_thresh=wght_thres, percent=p,
                                    order_quants=order_quant, quant_val=quantiles, na.rm=na.rm) 
              arpr_m <- arpr_l$lin
              setnames(arpr_m, names(arpr_m), c(names(id), var_name))
              arpr_v <- data.table(arpr_l$rate_val_pr)
              setnames(arpr_v, names(arpr_v), "arpr") } 
     arpr_m[is.na(arpr_m)] <- 0
     quantiles <- data.table(quantiles)
     setnames(quantiles, names(quantiles)[ncol(quantiles)], "quantiles")
     return(list(quantile=quantiles, threshold=threshold, value=arpr_v, lin=arpr_m))
}



## workhorse
arprlinCalc <- function(inc1, ids, wght1, indicator, indic2, income_thresh, wght_thresh, percent, order_quants=NULL, quant_val, na.rm) {
    indices<-NULL
    if(isTRUE(na.rm)){
        indices <- !is.na(inc1)
        ids <- ids[indices] 
        inc1 <- inc1[indices]
        wght1 <- wght1[indices]
        income_thresh <- income_thresh[indices]
        wght_thresh <- wght_thresh[indices]
        indicator <- indicator[indices]
        indic2 <- indic2[indices]
    } else if(any(is.na(inc1))) return(NA)

    inc2 <- income_thresh * indic2
    wght2 <- wght_thresh * indic2
  
    wt <- indicator * wght1
    thres_val <- percent/100 * quant_val
    N0 <- sum(wght2)   # Estimated whole population size 
    N <- sum(wt)       # Estimated (sub)population size

    poor <- (inc1<=thres_val)
    rate_val <- sum(wt*poor)/N  # Estimated poverty rate */
    rate_val_pr <- 100*rate_val

    h <- sqrt((sum(wght2*inc2*inc2)-sum(wght2*inc2)*sum(wght2*inc2)/sum(wght2))/sum(wght2))/exp(0.2*log(sum(wght2))) 
    # h=S/N^(1/5)

    #---- 1. Linearization of the poverty threshold ----

    u1 <- (quant_val-inc2)/h
    vect_f1 <- exp(-(u1^2)/2)/sqrt(2*pi)
    f_quant1 <- sum(vect_f1*wght2)/(N0*h)   # Estimate of F'(quantile)

    lin_thres <- -(percent/100)*indic2*(1/N0)*((inc2<=quant_val)-order_quants/100)/f_quant1  # Linearized variable

    #---- 2. Linearization of the poverty rate -----

    u2 <-   (thres_val-inc1)/h
    vect_f2 <- exp(-(u2^2)/2)/sqrt(2*pi)
    f_quant2 <- sum(vect_f2*wt)/(N*h)      # Estimate of F'(beta*quantile)

   #****************************************************************************************
   #                       LINEARIZED VARIABLE OF THE POVERTY RATE (IN %)                  *
   #****************************************************************************************
      lin <- 100*((1/N)*indicator*((inc1<=thres_val)-rate_val)+f_quant2*lin_thres)

      lin_id <- data.table(ids,lin)
    return(list(rate_val=rate_val, rate_val_pr=rate_val_pr, lin=lin_id))
}

