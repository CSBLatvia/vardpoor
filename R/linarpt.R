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


linarpt <- function(inc, id = NULL, weight = NULL, sort = NULL, 
        Dom = NULL, dataset = NULL, percentage = 60, order_quant=50,
        na.rm = FALSE, var_name="lin_arpt") {

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

       if(!is.null(Dom)) {
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

    # ARPT by domain (if requested)

    quantile <- incPercentile(inc, weight, sort, Dom, order_quant, na.rm=na.rm)
    threshold <- quantile
    if (!is.null(Dom)) {threshold[,ncol(threshold)] <- p/100 * quantile[,ncol(quantile)]
           } else threshold <- p/100 * quantile

    if(!is.null(Dom)) {
       D <- as.matrix(Dom) 
       Dom0 <- as.matrix(unique(D))
       Dom_agg <- Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
       Dom_agg <- as.matrix(Dom_agg)
       colnames(Dom_agg) <- colnames(Dom)
       arpt_m <- id
       for(i in 1:nrow(Dom_agg)) {
              c <- paste(colnames(Dom), Dom_agg[i,], sep=".")
              breakdown2 <- do.call(paste, as.list(c(c, sep="__")))
              ind <- (rowSums(Dom == matrix(Dom_agg[i,], n, ncol(D),1))== ncol(D))            
              arptl <- arptlinCalc(inc[,1],id,weight,ind, order_quant, quantile[i,ncol(quantile)])             
              colnames(arptl) <- c(names(id),paste(var_name,breakdown2,sep="__"))
              arpt_m <- merge(arpt_m,arptl,by=colnames(id),all.x=T)
              arpt_m[is.na(arpt_m[,ncol(arpt_m)]),ncol(arpt_m)] <- 0
           }
       rownames(threshold) <- NULL
     } else { names(quantile) <- NULL
              names(threshold) <- NULL
              ind <- rep(1,n)
              arpt_m <- arptlinCalc(inc[,1],id,weight,ind, order_quant, quantile)
              colnames(arpt_m) <- c(colnames(id),var_name)
             }
 return(list(quantile=quantile, value=threshold, lin=arpt_m)) 
}
   
    ## workhorse
arptlinCalc <- function(inco, ids, wght, indicator, order_quan, quant_val) {
  
    wt <- wght*indicator
    N <- sum(wt); # Estimated (sub)population size
 
    # h=S/N^(1/5) 
    h <- sqrt((sum(wght*inco*inco)-sum(wght*inco)*sum(wght*inco)/sum(wght))/sum(wght))/exp(0.2*log(sum(wght))) 

    u <-(quant_val-inco)/h
    vect_f <- exp(-(u^2)/2)/sqrt(2*pi)
    f_quant <- sum(vect_f*wt)/(N*h) # Estimate of F'(quantile)

 #****************************************************************************************
 #*                    LINEARIZED VARIABLE OF THE POVERTY THRESHOLD                      *
 #****************************************************************************************
    lin <- -(percentage/100)*(1/N)*indicator*((inc<=quant_val)-order_quan/100)/f_quant
    lin_id <- cbind(ids,lin)
    return(lin_id)
}
