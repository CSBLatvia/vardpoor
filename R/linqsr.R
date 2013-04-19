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


linqsr <- function(inc, id=NULL, weight=NULL, sort = NULL, Dom = NULL,
                dataset = NULL, alpha = 20, na.rm = FALSE, var_name="lin_qsr") {
   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if(!is.numeric(alpha) || length(alpha) != 1 || alpha[1] < 0 || alpha[1] > 100) {
          stop("'alpha' must be a numeric value in [0,100]")  }

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

    # QSR by domain (if requested)  
    
   if(!is.null(Dom)) {
       D <- as.matrix(Dom) 
       Dom0 <- as.matrix(unique(D))
       Dom_agg <- Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
       Dom_agg <- as.matrix(Dom_agg)
       colnames(Dom_agg) <- colnames(Dom) 
   
       QSR_v <- c()
       QSR_m <- id
       for(i in 1:nrow(Dom_agg)) {
             c <- paste(colnames(Dom), Dom_agg[i,], sep=".")
             breakdown2 <- do.call(paste, as.list(c(c, sep="__")))
             ind <- (rowSums(Dom == matrix(Dom_agg[i,], n, ncol(D),1))== ncol(D))                           
             QSR_l <- linQSRCalc(inc[,1], ids=id[,1], weight, sort, ind, alpha, na.rm) 
             QSRl <- QSR_l$lin
             colnames(QSRl) <- c(names(id),paste(var_name,breakdown2,sep="__"))
             QSR_m <- merge(QSR_m,QSRl,by=colnames(id),all.x=T)
             QSR_m[is.na(QSR_m[,ncol(QSR_m)]),ncol(QSR_m)] <- 0
             QSR_v <- rbind(QSR_v, data.frame(QSR_l$QSR,QSR_l$QSR_eu))   }
      colnames(QSR_v)<-c("QSR", "QSR_eu")
      QSR_v <- data.frame(Dom_agg,QSR_v)

    } else { ind <- rep(1,n)
             QSR_l <- linQSRCalc(inc[,1], id[,1], weight, sort, ind, alpha, na.rm)
             QSR_m <- QSR_l$lin
             colnames(QSR_m) <- c(colnames(id),var_name)   
             QSR_m[is.na(QSR_m[,ncol(QSR_m)]),ncol(QSR_m)] <- 0
             QSR_v <- data.frame(QSR_l$QSR,QSR_l$QSR_eu)
             colnames(QSR_v) <- c("QSR","QSR_eu")       }
    rownames(QSR_v) <- NULL
    return(list(value=QSR_v,lin=QSR_m))
}


linQSRCalc<-function(income, ids, weights=NULL, sort=NULL, ind=rep(1,length(ids)), alpha, na.rm=FALSE) {
#--------------------------------------------------------------------------------
#----- COMPUTATION OF ESTIMATED VALUES OF THE NUMERATOR AND THE DENOMINATOR -----
#--------------------------------------------------------------------------------
   if(isTRUE(na.rm)){
        indices <- !is.na(income)
        income <- income[indices]
        ids <- ids[indices]
        ind <- ind[indices]
        if(!is.null(weights)) weights <- weights[indices]
        if(!is.null(sort)) sort <- sort[indices]
   } else if(any(is.na(income))) return(NA)

   alpha2<-100-alpha
   ind <- as.data.frame(ind)
   if (is.null(colnames(ind))) colnames(ind) <- "ind"

   quantile <- incPercentile(income, weights, sort, ind, k=c(alpha,alpha2), na.rm=na.rm)

   quant_inf <- quantile[quantile$ind==1,ncol(ind)+1]
   quant_sup <- quantile[quantile$ind==1,ncol(ind)+2]

   if (is.data.frame(ind)) ind<-ind[,1]

   wt <- weights*ind
   v <- weights*income*ind

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

   lin_id <- cbind(ids,lin)
  return(list(QSR=QSR, QSR_eu=QSR_eu, lin=lin_id))
}

