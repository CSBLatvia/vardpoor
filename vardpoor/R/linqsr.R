#**********************************************************************************************
#**********************************************************************************************
#**********************************************************************************************
#***                                                                                        ***
#***                                                                                        ***
#***                    LINEARIZATION OF THE INCOME QUANTILE SHARE RATIO                    ***
#***                                                                                        ***
#***                                                                                        ***
#**********************************************************************************************
#**********************************************************************************************
#**********************************************************************************************

linqsr <- function(inc, id=NULL, weight=NULL, sort = NULL,
                   Dom = NULL, period=NULL, dataset = NULL,
                   alpha = 20, var_name="lin_qsr") {

   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if(!is.numeric(alpha) || length(alpha) != 1 || alpha[1] < 0 || alpha[1] > 100) {
          stop("'alpha' must be a numeric value in [0,100]")  }

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
   if (ncol(inc) != 1) stop("'inc' must be vector or 1 column data.frame, data matrix, data table")
   inc <- inc[,1]
   if (!is.numeric(inc)) stop("'inc' must be a numeric vector")
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

   # QSR by domain (if requested)  

   QSR_id <- id
   if (!is.null(period)) QSR_id <- data.table(QSR_id, period)
    
   if (!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))

        QSR_v <- c()
        QSR_m <- copy(QSR_id)

        for(i in 1:nrow(Dom_agg)) {
              g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
              var_nams <- do.call(paste, as.list(c(g, sep="__")))

              ind <- (rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

              QSR_l <- lapply(1:nrow(period1_agg), function(j) {
                               indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                               QSR_l <- linQSRCalc(income=inc[indj],
                                                   ids=QSR_id[indj],
                                                   weights=weight[indj],
                                                   sort=sort[indj],
                                                   ind=ind[indj],
                                                   alpha=alpha) 
                                if (!is.null(period)) { 
                                       list(QSR=data.table(period_agg[j], Dom_agg[i], QSR_l$QSR), lin=QSR_l$lin)
                                  } else list(QSR=data.table(Dom_agg[i], QSR_l$QSR), lin=QSR_l$lin)                               
                         })
                 QSRs <- rbindlist(lapply(QSR_l, function(x) x[[1]]))
                 QSRlin <- rbindlist(lapply(QSR_l, function(x) x[[2]]))

                 setnames(QSRlin, names(QSRlin), c(names(QSR_id), var_nams))
                 setkeyv(QSR_m, names(QSR_id))
                 setkeyv(QSRlin, names(QSR_id))
                 QSR_m <- merge(QSR_m, QSRlin, all.x=T)
                 QSR_v <- rbind(QSR_v, QSRs)
           }
    } else { QSRl <- lapply(1:nrow(period1_agg), function(j) {
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))
      
                           QSR_l <- linQSRCalc(income=inc[indj], ids=QSR_id[indj],
                                               weights=weight[indj], sort=sort[indj],
                                               ind=ind0[indj], alpha=alpha)
                           if (!is.null(period)) { 
                                       list(QSR=data.table(period_agg[j], QSR_l$QSR), lin=QSR_l$lin)
                                  } else list(QSR=data.table(QSR_l$QSR), lin=QSR_l$lin)
                       })
             QSR_v <- rbindlist(lapply(QSRl, function(x) x[[1]]))
             QSR_m <- rbindlist(lapply(QSRl, function(x) x[[2]]))

             setnames(QSR_m, names(QSR_m), c(names(QSR_id), var_name))
           }
  QSR_m[is.na(QSR_m)] <- 0               
  setkeyv(QSR_m, names(QSR_id))
  return(list(value=QSR_v, lin=QSR_m))
}


linQSRCalc<-function(income, ids, weights=NULL, sort=NULL, ind=NULL, alpha) {
#--------------------------------------------------------------------------------
#----- COMPUTATION OF ESTIMATED VALUES OF THE NUMERATOR AND THE DENOMINATOR -----
#--------------------------------------------------------------------------------
   if (is.null(ind)) ind <- data.frame(ind=rep.int(1,length(ids)))

   alpha2 <- 100 - alpha
   quantile <- incPercentile(inc=income, weights=weights,
                             sort=sort, Dom=data.table(ind),
                             period=NULL, k=c(alpha,alpha2),
                             dataset=NULL) 
   quant_inf <- quantile[ind==TRUE][[paste0("x", alpha)]] 
   quant_sup <- quantile[ind==TRUE][[paste0("x", alpha2)]] 

   wt <- weights * ind
   v <- weights * income * ind

   indinf <- (income <= quant_inf)
   indsup <- (income > quant_sup)

   num_eu <- sum(v*indsup)/sum(wt[indsup]) # Numerator 
   den_eu <- sum(v*indinf)/sum(wt[indinf]) # Denominator 

   num <- sum(v*indsup) # Numerator 
   den <- sum(v*indinf) # Denominator 
    
   QSR <- num/den
   QSR_eu <- num_eu/den_eu   

#**********************************************************************
#*          LINEARIZATION OF THE INCOME QUANTILE SHARE RATIO          *
#**********************************************************************

#----------------------------------------------
#----- LINEARIZATION OF THE TWO QUANTILES -----
#----------------------------------------------

   N <- sum(wt) # Estimated (sub)population size  
   h <- sqrt((sum(weights*income*income)-sum(weights*income)*sum(weights*income)/sum(weights))/sum(weights))/exp(0.2*log(sum(weights))) 
   # h=S/N^(1/5) 

   # 1. Linearization of the bottom quantile 
 
   u1 <- (quant_inf-income)/h;
   vect_f1 <- exp(-(u1^2)/2)/sqrt(2*pi)
   f_quant1 <- sum(vect_f1*wt)/(N*h) 

   lin_inf <- -(1/N)*((income<=quant_inf)-alpha/100)/f_quant1

   # 2. Linearization of the top quantile 
 
   u2 <- (quant_sup-income)/h
   vect_f2 <- exp(-(u2^2)/2)/sqrt(2*pi)
   f_quant2 <- sum(vect_f2*wt)/(N*h)

   lin_sup <- -(1/N)*((income<=quant_sup)-alpha2/100)/f_quant2 

#------------------------------------------------------------
#----- LINEARIZATION OF THE INCOME QUANTILE SHARE RATIO -----
#------------------------------------------------------------
 
   # 1. Linearization of the numerator 
  
   u3 <- (quant_sup-income)/h
   vect_f3 <- exp(-(u3^2)/2)/sqrt(2*pi)
   f_quant3 <- sum(vect_f3*v)/h
 
   lin_num <- income-income*(income<=quant_sup)-f_quant3*lin_sup
 
   # 2. Linearization of the denominator 
  
   u4 <- (quant_inf-income)/h
   vect_f4 <- exp(-(u4^2)/2)/sqrt(2*pi)
   f_quant4 <- sum(vect_f4*v)/h
 
   lin_den <- income*(income<=quant_inf)+f_quant4*lin_inf

 #****************************************************************************
 #                 LINEARIZED VARIABLE OF THE IQ SHARE RATIO                  
 #****************************************************************************

   lin <- (den*lin_num-num*lin_den)/(den*den)

   lin_id <- data.table(ids, lin)
   QSR <- data.table(QSR=QSR, QSR_eu=QSR_eu)
  return(list(QSR=QSR, lin=lin_id))
}

