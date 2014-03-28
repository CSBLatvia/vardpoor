#***********************************************************************************************************************
#***********************************************************************************************************************
#***********************************************************************************************************************
#***                                                                                                                 ***
#***                                                                                                                 ***
#***                    LINEARIZATION OF THE MEDIAN INCOME BELOW THE AT-RISK-OF-POVERTY THRESHOLD                    ***
#***                                                                                                                 ***
#***                                                                                                                 ***
#***********************************************************************************************************************
#***********************************************************************************************************************
#***********************************************************************************************************************

linpoormed <- function(inc, id, weight=NULL, sort=NULL, Dom=NULL, period=NULL, 
                       dataset = NULL, percentage=60, order_quant=50, na.rm=FALSE,
                       var_name="lin_poormed") {
 
   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must be defined name of the linearized variable")}

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

       if (!is.null(Dom)) {
            Dom2 <- Dom
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
   if (!is.numeric(inc)) stop("'inc' must be a numeric vector")
   if (any(is.na(inc))) warning("'inc' has unknown values")

   # id
   if(is.null(id)) id <- 1:n 
   id <- data.table(id)
   if (nrow(id) != n) stop("'id' must be the same length as 'inc'")
   if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
   if (is.null(names(id))||(names(id)=="id")) names(id) <- "ID"
 
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
   Dom1 <- Dom
   if (!is.null(period)) {
       if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 
 
    # Poor median people by domain (if requested)

    quantiles <- incPercentile(inc, weight, sort, Dom=period, k=order_quant, dataset = NULL, na.rm=na.rm)
    threshold <- data.table(quantiles)
    threshold[,names(threshold)[ncol(threshold)]:=p/100 * threshold[,ncol(threshold),with=FALSE]]
    setnames(threshold,names(threshold)[ncol(threshold)],"threshold")
  
    if(!is.null(Dom1)) {
        Dom_agg <- data.table(unique(Dom1))
        setkeyv(Dom_agg, names(Dom1)) 
        if (!is.null(period)) quantil <- merge(Dom_agg, quantiles, by=names(period), sort=F) 
        poor_people_median <- c()
        poor_med_id <- id
        if (!is.null(period))  poor_med_id <- data.table(period,  poor_med_id)
        if (is.null(period)) { ind2 <- rep.int(1, n)
                               quantile <- quantiles }
        poor_med_m <- poor_med_id
        for(i in 1:nrow(Dom_agg)) {
              g <- paste(names(Dom1), as.matrix(Dom_agg[i,]), sep = ".")
              breakdown2 <- do.call(paste, as.list(c(g, sep="__")))
              D <- Dom_agg[i,][rep(1, nrow(Dom1)),]
              ind <- (rowSums(Dom1 == D) == ncol(Dom1))

              if (!is.null(period)) {
                   quantile <- data.frame(quantil)[i,ncol(quantil)]
                   ind2 <- (rowSums(period == D[,1:ncol(period),with=F]) == ncol(period)) }

              poormed_l <- linpoormedCalc(incom=inc, ids=id, wghts=weight, sort=sort,
                                          ind=ind, ind_year=ind2, percentag=p,
                                          order_quants=order_quant,
                                          quant_val=quantile, na.rm=na.rm) 
              poormedl <- poormed_l$lin
              setnames(poormedl, names(poormedl), c(names(id), paste(var_name, breakdown2, sep="__")))
              poor_med_m <- merge(poor_med_m, poormedl, by=names(id), all.x=T)
              poor_people_median <- rbind(poor_people_median, poormed_l$poor_people_median)
           }
        colnames(poor_people_median) <- "poor_people_median"
        poor_people_median <- data.table(Dom_agg, poor_people_median)
      } else { ind <- rep.int(1,n)
               poormed_l <- linpoormedCalc(incom=inc, ids=id, wghts=weight, sort=sort,
                                           ind=ind, ind_year=ind, percentag=p,
                                           order_quants=order_quant,
                                           quant_val=quantiles, na.rm=na.rm)  
               poor_med_m <- poormed_l$lin
               setnames(poor_med_m, names(poor_med_m), c(names(id),var_name))
               poor_people_median <- data.table(poormed_l$poor_people_median)   
               setnames(poor_people_median, names(poor_people_median), "poor_people_median")    }           
  poor_med_m[is.na(poor_med_m)] <- 0
  quantile <- data.table(quantiles)
  setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
  return(list(quantile=quantile, threshold=threshold, value=poor_people_median, lin=poor_med_m)) 
}


## workhorse
linpoormedCalc <- function(incom, ids, wghts, sort, ind, ind_year, percentag, order_quants, quant_val, na.rm) {
    inco <- incom * ind_year
    wght <- wghts * ind_year

    if (isTRUE(na.rm)){
          indices <- !is.na(inco)
          ids <- ids[indices]
          inco <- inco[indices]
          wght <- wght[indices]
          ind <- ind[indices]
          ind_year <- ind_year[indices]
     } else if(any(is.na(inco))) return(NA)

    wt <- ind * wght   
    thres_val <- percentag/100 * quant_val
    N0 <- sum(wght)             # Estimated whole population size
    N <- sum(wt)                # Estimated (sub)population size
    poor <- (inco<=thres_val)*ind
    inc1 <- inco[poor==1]
    wght1 <- wght[poor==1]
    sort1 <- sort[poor==1]

    rate_val <- sum(wt*poor)/N  # Estimated poverty rate
    rate_val_pr <- 100*rate_val  # Estimated poverty rate
 
    poor_people_median <- incPercentile(inc1, wght1, sort=sort1, Dom=NULL, k=order_quants, dataset = NULL, na.rm=na.rm)
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

    lin_thres <- -(percentag/100)*ind_year*(1/N0)*((inco<=quant_val)-order_quants/100)/f_quant1
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

     lin_id <- data.table(ids,lin_median)

     return(list(poor_people_median=poor_people_median, lin=lin_id))
}

