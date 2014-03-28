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
                   alpha = 20, na.rm = FALSE, var_name="lin_qsr") {

   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if(!is.numeric(alpha) || length(alpha) != 1 || alpha[1] < 0 || alpha[1] > 100) {
          stop("'alpha' must be a numeric value in [0,100]")  }

   if(!is.null(dataset)) {
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
   if (any(is.na(inc))) warning("'inc' has unknown values")

   # id
   if (is.null(id)) id <- 1:n 
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
    ind <- data.frame(ind=rep.int(1, n))
    Dom1 <- Dom
    if (!is.null(period)) {
       if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 

    # QSR by domain (if requested)  
    
   if (!is.null(Dom1)) {
        Dom_agg <- data.table(unique(Dom1))
        setkeyv(Dom_agg,colnames(Dom1))
        QSR_v <- c()
        QSR_id <- id
        if (!is.null(period)) QSR_id <- data.table(period, QSR_id)
        if (is.null(period)) ind2 <- ind
        QSR_m <- QSR_id
        for(i in 1:nrow(Dom_agg)) {
              g <- paste(names(Dom1), as.matrix(Dom_agg[i,]), sep = ".")
              breakdown2 <- do.call(paste, as.list(c(g, sep="__")))
              D <- Dom_agg[i,][rep(1, nrow(Dom1)),]
              ind <- data.frame(ind=(rowSums(Dom1 == D) == ncol(Dom1)))
              if (!is.null(period)) {
                   ind2 <- (rowSums(period == D[,1:ncol(period),with=F]) == ncol(period)) 
                   ind2 <- data.frame(ind2)
                  }
              QSR_l <- linQSRCalc(incom=inc, ids=id, weightss=weight, sort=sort,
                                  ind=ind, ind2=ind2, alpha=alpha, na.rm=na.rm) 
              QSRl <- QSR_l$lin
              setnames(QSRl,names(QSRl), c(names(id), paste(var_name, breakdown2, sep="__")))
              QSR_m <- merge(QSR_m, QSRl, by=names(id), all.x=T)
              QSR_v <- rbind(QSR_v, data.table(QSR_l$QSR, QSR_l$QSR_eu))   }
      setnames(QSR_v,names(QSR_v), c("QSR", "QSR_eu"))
      QSR_v <- data.table(Dom_agg, QSR_v)
    } else { ind <- data.frame(ind=rep.int(1, n))
             QSR_l <- linQSRCalc(incom=inc, ids=id, weightss=weight, sort=sort,
                                 ind=ind, ind2=ind, alpha=alpha, na.rm=na.rm)
             QSR_m <- QSR_l$lin
             setnames(QSR_m,names(QSR_m),c(names(id),var_name))
             QSR_v <- data.table(QSR_l$QSR, QSR_l$QSR_eu)
             setnames(QSR_v,names(QSR_v),c("QSR","QSR_eu"))       }
     QSR_m[is.na(QSR_m)] <- 0               
  return(list(value=QSR_v,lin=QSR_m))
}


linQSRCalc<-function(incom, ids, weightss=NULL, sort=NULL, ind=NULL, ind2=NULL, alpha, na.rm=FALSE) {
#--------------------------------------------------------------------------------
#----- COMPUTATION OF ESTIMATED VALUES OF THE NUMERATOR AND THE DENOMINATOR -----
#--------------------------------------------------------------------------------
   if (is.null(ind)) ind <- data.frame(ind=rep.int(1,length(ids)))
   if (is.null(ind2)) ind2 <- data.frame(ind2=rep.int(1,length(ids)))
   if (isTRUE(na.rm)){
         indices <- !is.na(incom)
         incom <- incom[indices]
         ids <- ids[indices]
         ind  <- data.frame(ind[indices,1])
         colnames(ind)[1]<-"ind"
         ind2  <- data.frame(ind2[indices,1])
         colnames(ind2)[1]<-"ind2"
         if(!is.null(weightss)) weightss <- weightss[indices]
         if(!is.null(sort)) sort <- sort[indices]
    } else if(any(is.na(incom))) return(NA)

   alpha2 <- 100 - alpha
   income <- incom * ind2
   weights <- weightss * ind2
   quantile <- incPercentile(income, weightss, sort, Dom=ind, k=c(alpha,alpha2), dataset=NULL, na.rm=na.rm) 
   quant_inf <- quantile[quantile$ind==1,ncol(ind)+1] 
   quant_sup <- quantile[quantile$ind==1,ncol(ind)+2] 

   if (is.data.frame(ind)) ind <- ind[,1]

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

   lin_inf <- -(1/N)*ind2*((income<=quant_inf)-alpha/100)/f_quant1

   # 2. Linearization of the top quantile 
 
   u2 <- (quant_sup-income)/h
   vect_f2 <- exp(-(u2^2)/2)/sqrt(2*pi)
   f_quant2 <- sum(vect_f2*wt)/(N*h)

   lin_sup <- -(1/N)*ind2*((income<=quant_sup)-alpha2/100)/f_quant2 

#------------------------------------------------------------
#----- LINEARIZATION OF THE INCOME QUANTILE SHARE RATIO -----
#------------------------------------------------------------
 
   # 1. Linearization of the numerator 
  
   u3 <- (quant_sup-income)/h
   vect_f3 <- exp(-(u3^2)/2)/sqrt(2*3.1415926536)
   f_quant3 <- sum(vect_f3*v)/h
 
   lin_num <- income-income*(income<=quant_sup)-f_quant3*lin_sup
 
   # 2. Linearization of the denominator 
  
   u4 <- (quant_inf-income)/h
   vect_f4 <- exp(-(u4^2)/2)/sqrt(2*3.1415926536)
   f_quant4 <- sum(vect_f4*v)/h
 
   lin_den <- income*(income<=quant_inf)+f_quant4*lin_inf

 #****************************************************************************
 #                 LINEARIZED VARIABLE OF THE IQ SHARE RATIO                  
 #****************************************************************************

   lin <- (den*lin_num-num*lin_den)/(den*den)

   lin_id <- data.table(ids, lin)
  return(list(QSR=QSR, QSR_eu=QSR_eu, lin=lin_id))
}

