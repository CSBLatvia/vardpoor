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
        Dom = NULL, period=NULL, dataset = NULL, percentage = 60,
        order_quant=50, na.rm = FALSE, var_name="lin_arpt") {

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
       dataset <- data.frame(dataset)
       if (checker(inc, dataset, "inc")) inc <- dataset[, inc] 

       if(!is.null(id)) {
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

       if(!is.null(Dom)) {
            Dom2 <- Dom
            if (checker(Dom,dataset,"Dom")) {
                    Dom <- as.data.frame(dataset[, Dom], stringsAsFactors=FALSE) 
                    names(Dom) <- Dom2 }    }

      }

   # check vectors
   # inc
   inc <- data.frame(inc)
   n <- nrow(inc)
   if (ncol(inc) != 1) stop("'inc' must be a vector or 1 column data.frame, matrix, data.table")
   inc <- inc[,1]
   if(!is.numeric(inc)) stop("'inc' must be numerical")
   if (any(is.na(inc))) warning("'inc' has unknown values")

   # id
   if (is.null(id)) id <- 1:n 
   id <- data.table(id)
   if (nrow(id) != n) stop("'id' must be the same length as 'inc'")
   if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
   if (is.null(names(id))||(names(id)=="id")) setnames(id, names(id),"ID")
   if (any(duplicated(id))) stop("'id' are duplicate values: ",
                                  paste(id[duplicated(id)], collapse = ",")) 

   # weight
   weight <- data.frame(weight)
   if (is.null(weight)) weight <- data.frame(rep.int(1, n))
   if (nrow(weight) != n) stop("'weight' must be the same length as 'inc'")
   if (ncol(weight) != 1) stop("'weight' must be vector or 1 column data.frame, matrix, data.table")
   weight <- weight[,1]
   if (!is.numeric(weight)) stop("'weight' must be numerical")

   # sort
   if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
         stop("'sort' must be a vector or ordered factor") }
   if (!is.null(sort) && length(sort) != n) stop("'sort' must have the same length as 'inc'")
   
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
    Dom1 <- Dom
    if (!is.null(period)) {
       if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 


    # ARPT by domain (if requested)  

    quantile <- incPercentile(inc, weights = weight, sort = sort, Dom = Dom1,
                              k = order_quant, dataset = NULL, na.rm=na.rm)
    threshold <- data.table(quantile)
    threshold[,names(threshold)[ncol(threshold)]:=p/100 * threshold[, ncol(threshold), with=FALSE]]
    setnames(threshold, names(threshold)[ncol(threshold)], "threshold")
   
              
    if(!is.null(Dom1)) {
        Dom_agg <- data.table(unique(Dom1))
        setkeyv(Dom_agg, names(Dom1))
        if (is.null(period)) ind2 <- rep.int(1, n)
        arpt_id <- id
        if (!is.null(period)) arpt_id <- data.table(period, arpt_id)
        arpt_m <- arpt_id
              
        for(i in 1:nrow(Dom_agg)) {
              g <- paste(names(Dom1),as.matrix(Dom_agg[i,]), sep = ".")
              breakdown2 <- do.call(paste, as.list(c(g, sep="__")))
              D <- Dom_agg[i,][rep(1,nrow(Dom1)),]
              ind <- (rowSums(Dom1 == D) == ncol(Dom1))
              if (!is.null(period)) {
                   ind2 <- (rowSums(period == D[,1:ncol(period),with=F]) == ncol(period)) }

              arptl <- arptlinCalc(inco=inc, ids=arpt_id, wghts=weight, 
                                   indicator=ind, ind2=ind2,
                                   order_quan=order_quant,
                                   quant_val=quantile[i,ncol(quantile)],
                                   percentag=p)
              setnames(arptl, names(arptl), c(names(arpt_id), paste(var_name,breakdown2,sep="__")))
              arpt_m <- merge(arpt_m, arptl, by=names(arpt_id),all.x=T, sort=FALSE)
           }
    } else {  ind <- rep.int(1, n)
              arpt_m <- arptlinCalc(inco=inc, ids=id, wghts=weight,
                                    indicator=ind, ind2=ind,
                                    order_quan=order_quant,
                                    quant_val=quantile,
                                    percentag=p)
              setnames(arpt_m, names(arpt_m), c(names(id),var_name))
             }
   arpt_m[is.na(arpt_m)] <- 0
   quantile <- data.table(quantile)
   setnames(quantile,names(quantile)[ncol(quantile)],"quantile")
   return(list(quantile=quantile, value=threshold, lin=arpt_m)) 
}


    ## workhorse
arptlinCalc <- function(inco, ids, wghts, indicator, ind2, order_quan, quant_val, percentag) {
    wght <- wghts * ind2
    wt <- wght * indicator
    N <- sum(wt); # Estimated (sub)population size

    # h=S/N^(1/5) 
    h <- sqrt((sum(wght*inco*inco)-sum(wght*inco)*sum(wght*inco)/sum(wght))/sum(wght))/exp(0.2*log(sum(wght))) 
    u <-(quant_val-inco)/h

    vect_f <- exp(-(u^2)/2)/sqrt(2*pi)
    f_quant <- sum(vect_f*wt)/(N*h) # Estimate of F'(quantile)

 #****************************************************************************************
 #*                    LINEARIZED VARIABLE OF THE POVERTY THRESHOLD                      *
 #****************************************************************************************
    lin <- -(percentag/100)*(1/N)*indicator*((inco<=quant_val)-order_quan/100)/f_quant
    lin_id <- data.table(ids, lin)
    return(lin_id)
}


