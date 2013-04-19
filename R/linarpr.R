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
                 sort=NULL, Dom=NULL, dataset = NULL, percentage=60, 
                 order_quant=50, na.rm=FALSE, var_name="lin_arpr") {
 
   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

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
       if (checker(inc,dataset,"inc")) inc <- dataset[, inc] 

       if(!is.null(id)) {
          id2 <- id
          if (checker(id,dataset,"id")) id <- data.frame(dataset[, id])
          names(id) <- id2 }

       if(!is.null(weight)) {
           if (checker(weight,dataset,"weight")) weight <- dataset[, weight] }

       if(!is.null(income_thres)) {
           if (checker(income_thres,dataset,"income_thres")) income_thres <- dataset[, income_thres] }
     
       if(!is.null(wght_thres)) {
           if (checker(wght_thres,dataset,"wght_thres")) wght_thres <- dataset[, wght_thres] }

       if(!is.null(sort)) {
           if (checker(sort,dataset,"sort")) sort <- dataset[, sort] }

       if (!is.null(Dom)) {
            Dom2<-Dom
            if (checker(Dom,dataset,"Dom")) {
                    Dom <- as.data.frame(dataset[, Dom]) 
                    names(Dom)<-Dom2 }    }
      }

   # check vectors
   # inc
   inc <- as.data.frame(inc)
   if(!is.numeric(as.vector(inc[,1]))) stop("'inc' must be a numeric vector")
   if (ncol(inc) != 1) stop("'inc' must have vector or 1 column data.frame")
   n <- nrow(inc)                     

   # id
   if(is.null(id)) id <- 1:n 
   id <- as.data.frame(id)
   if (is.null(names(id))||(names(id)=="1:n")) names(id) <- "ID"
 
   # weight
   if(is.null(weight)) { weight <- rep.int(1, n)
       } else if(!is.numeric(weight)) stop("'weight' must be a numeric vector")
   if(length(weight) != n) stop("'weight' must have the same length as 'x'")
   
   # income_thres
   if(!is.null(income_thres)) {
       if(!is.numeric(income_thres)) stop("'income_thres' must be a numeric vector")
       if(nrow(income_thres) != n) stop("'income_thres' must have the same length as 'x'")
     } else income_thres <- inc[,1]

   # wght_thres
   if(is.null(wght_thres)) { wght_thres <- weight
       } else if(!is.numeric(wght_thres)) stop("'wght_thres' must be a numeric vector")
   if(length(wght_thres) != n) stop("'wght_thres' must have the same length as 'x'")

   # sort
   if(!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
         stop("'sort' must be a vector or ordered factor") }
   if(!is.null(sort) && length(sort) != n) stop("'sort' must have the same length as 'x'")     
   
   # Dom     
   if(!is.null(Dom)) {
             if (is.null(colnames(Dom))) stop("'Dom' must be colnames")
             if (nrow(Dom) != n) stop("'Dom' must have the same length as 'inc'")
       }

    ## computations

    # ARPR by domain (if requested)

    quantiles <- incPercentile(income_thres, wght_thres, sort=sort,Dom=NULL, order_quant, na.rm=na.rm)
    names(quantiles) <- NULL
    threshold <- p/100 * quantiles

    if(!is.null(Dom)) {
       D <- as.matrix(Dom) 
       Dom0 <- as.matrix(unique(D))
       Dom_agg <- Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
       Dom_agg <- as.matrix(Dom_agg)
       colnames(Dom_agg) <- colnames(Dom)

       arpr_v <- c()
       arpr_m <- id
       for(i in 1:nrow(Dom_agg)) {
              c <- paste(colnames(Dom), Dom_agg[i,], sep=".")
              breakdown2 <- do.call(paste, as.list(c(c, sep="__")))
              ind <- (rowSums(Dom == matrix(Dom_agg[i,], n, ncol(D),1))== ncol(D))       
              arpr_l <- arprlinCalc(inc[,1], id[,1], wght1=weight, indicator=ind, income_thresh=income_thres,
                               wght_thresh=wght_thres, percent=p, order_quan=order_quant, 
                               quant_val=quantiles, na.rm=na.rm) 
              arprl <- arpr_l$lin
              colnames(arprl) <- c(names(id),paste(var_name,breakdown2,sep="__"))
              arpr_m <- merge(arpr_m,arprl,by=colnames(id),all.x=T)
              arpr_m[is.na(arpr_m[,ncol(arpr_m)]),ncol(arpr_m)]=0
              arpr_v <- rbind(arpr_v,arpr_l$rate_val_pr) 
           }
       rownames(threshold) <- NULL
       colnames(arpr_v) <- "arpr"
       arpr_v <- data.frame(Dom_agg,arpr_v)
     } else { ind=rep(1,n)
              names(quantile) <- NULL
              names(threshold) <- NULL
              arpr_l <- arprlinCalc(inc[,1], id[,1], wght1=weight, indicator=ind, income_thresh=income_thres,
                                wght_thresh=wght_thres, percent=p, order_quan=order_quant, 
                                quant_val=quantiles, na.rm=na.rm) 
              arpr_m <- arpr_l$lin
              colnames(arpr_m) <- c(colnames(id),var_name)
              arpr_v <- arpr_l$rate_val_pr   }

 return(list(quantile=quantile, threshold=threshold, value=arpr_v, lin=arpr_m))
}


## workhorse
arprlinCalc <- function(inc1, ids, wght1, indicator, income_thresh, wght_thresh, percent, order_quan, quant_val, na.rm) {
    if(isTRUE(na.rm)){
        indices <- !is.na(inc1)
        ids <- ids[indices] 
        inc1 <- inc1[indices]
        wght1 <- wght1[indices]
        income_thresh <- income_thresh[indices]
        wght_thresh <- wght_thresh[indices]
        indicator <- indicator[indeces]
    } else if(any(is.na(inc1))) return(NA)

    inc2 <- income_thresh
    wght2 <- wght_thresh

    wt <- indicator*wght1
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
 
    lin_thres <- -(percent/100)*(1/N0)*((inc2<=quant_val)-order_quant/100)/f_quant1  # Linearized variable

    #---- 2. Linearization of the poverty rate -----

    u2 <- (thres_val-inc1)/h
    vect_f2 <- exp(-(u2^2)/2)/sqrt(2*pi)
    f_quant2 <- sum(vect_f2*wt)/(N*h)      # Estimate of F'(beta*quantile)

   #****************************************************************************************
   #                       LINEARIZED VARIABLE OF THE POVERTY RATE (IN %)                  *
   #****************************************************************************************
      lin <- 100*((1/N)*indicator*((inc1<=thres_val)-rate_val)+f_quant2*lin_thres)

      lin_id <- cbind(ids,lin)
    return(list(rate_val=rate_val, rate_val_pr=rate_val_pr, lin=lin_id))
}

