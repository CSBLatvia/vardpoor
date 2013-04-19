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


linrmpg<- function(inc, id, weight=NULL, sort=NULL, Dom=NULL, dataset = NULL,
                     percentage=60, order_quant=50, na.rm=FALSE, var_name="lin_rmpg") {
 
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

    # Relative median at-risk-of-poverty gap by domain (if requested)

    quantiles <- incPercentile(inc, weight, sort=sort,Dom=NULL, order_quant, na.rm=na.rm)
    names(quantiles) <- NULL
    threshold <- p/100 * quantiles

    if(!is.null(Dom)) {
       D <- as.matrix(Dom) 
       Dom0 <- as.matrix(unique(D))
       Dom_agg <- Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
       Dom_agg <- as.matrix(Dom_agg)
       colnames(Dom_agg) <- colnames(Dom)

       rmpg_v <- c()
       rmpg_m <- id
       for(i in 1:nrow(Dom_agg)) {
              c <- paste(colnames(Dom), Dom_agg[i,], sep=".")
              breakdown2 <- do.call(paste, as.list(c(c, sep="__")))
              indicator <- (rowSums(Dom == matrix(Dom_agg[i,], n, ncol(D),1))== ncol(D))       
              rmpg_l <- linrmpgCalc(inc[,1], id, weight, ind=indicator, percentag=p,
                                order_quan=order_quant, quant_val=quantiles, na.rm=na.rm) 
              rmpgl <- rmpg_l$lin
              colnames(rmpgl) <- c(names(id),paste(var_name,breakdown2,sep="__"))
              rmpg_m <- merge(rmpg_m,rmpgl,by=colnames(id),all.x=T)
              rmpg_m[is.na(rmpg_m[,ncol(rmpg_m)]),ncol(rmpg_m)] <- 0
              rmpg_v <- rbind(rmpg_v,rmpg_l$rmgap)
           }
       rownames(threshold) <- NULL
       colnames(rmpg_v) <- "rmpg"
       rmpg_v <- data.frame(Dom_agg,rmpg_v)
     } else { indicator=rep(1,n)
              names(quantile) <- NULL
              names(threshold) <- NULL
              rmpg_l <- linrmpgCalc(inc[,1], id, weight, ind=indicator, percentag=p,
                                      order_quan=order_quant, quant_val=quantiles, na.rm=na.rm)  
              rmpg_m <- rmpg_l$lin
              colnames(rmpg_m) <- c(colnames(id),var_name)
              rmpg_v <- rmpg_l$rmgap   }

  return(list(quantile=quantile, threshold=threshold, value=rmpg_v, lin=rmpg_m)) 
}


## workhorse
linrmpgCalc <- function(inco, ids, wght, ind, percentag, order_quan, quant_val, na.rm) {
    if(isTRUE(na.rm)){
        indices <- !is.na(inco)
        ids <- ids[indices]
        inco <- inc1[indices]
        wght <- wght[indices]
        indicator <- indicator[indeces]
    } else if(any(is.na(inco))) return(NA)

    wt <- ind*wght   
    thres_val <- percentag/100 * quant_val
    N0=sum(wght)                # Estimated whole population size
    N <- sum(wt)                # Estimated (sub)population size
    poor <- (inco<=thres_val)*ind

    inc1 <- inco[poor==1]
    wght1 <- wght[poor==1]
    sort1 <- sort[poor==1]

    rate_val <- sum(wt*poor)/N  # Estimated poverty rate
    rate_val_pr <- 100*rate_val  # Estimated poverty rate
 
    poor_people_median <- incPercentile(inc1, wght1, sort=sort1, Dom=NULL, 50, na.rm=na.rm)
    names(poor_people_median) <-NULL
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

    lin_thres <- -(percentag/100)*(1/N0)*((inco<=quant_val)-order_quant/100)/f_quant1
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
     lin_gap=100*(poor_people_median*lin_thres/(thres_val*thres_val)-lin_median/thres_val)
     
     rmgap<-100-100*poor_people_median/thres_val

     lin_id <- cbind(ids,lin_median)

     return(list(rmgap=rmgap, lin=lin_id))
}

