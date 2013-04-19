# ************************************************************************
# ************************************************************************
# ************************************************************************
# ***                                                                  ***
# ***                                                                  ***
# ***            LINEARIZATION OF THE GINI COEFFICIENT                 ***
# ***                                                                  ***
# ***                                                                  ***
# ************************************************************************
# ************************************************************************
# ************************************************************************


lingini <- function(inc, id = NULL, weight = NULL, sort = NULL, 
        Dom = NULL, dataset = NULL, na.rm = FALSE, var_name="lin_gini") {

   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

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
           
  # Gini by domain (if requested)

   if(!is.null(Dom)) {
      D <- as.matrix(Dom) 
      Dom0 <- as.matrix(unique(D))
      Dom_agg <- Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
      Dom_agg <- as.matrix(Dom_agg)
      colnames(Dom_agg) <- colnames(Dom)
   
      Gini <- c()
      gini_m <- id
 
      for(k in 1:nrow(Dom_agg)) {
              c <- paste(colnames(Dom), Dom_agg[k,], sep=".")
              breakdown2 <- do.call(paste, as.list(c(c, sep="__")))
              ind <- (rowSums(Dom == matrix(Dom_agg[k,], n, ncol(D),1))== ncol(D))
              gini_l <- linginiCalc(inc[ind,1],id[ind,1], weight[ind], sort[ind], na.rm=na.rm)
              ginilin <- gini_l$lin
              colnames(ginilin) <- c(colnames(id),paste(var_name,breakdown2,sep="__"))
              gini_m <- merge(gini_m,ginilin,by=colnames(id),all.x=T)
              gini_m[is.na(gini_m[,ncol(gini_m)]),ncol(gini_m)] <- 0
              Gini <- rbind(Gini, data.frame(gini_l$Gini,gini_l$Gini_eu))           
             }
      colnames(Gini) <- c("Gini","Gini_eu")
      Gini <- data.frame(Dom_agg,Gini)
     } else { gini_l <- linginiCalc(inc[,1],id[,1], weight, sort, na.rm=na.rm)
              gini_m <- gini_l$lin
              colnames(gini_m) <- c(colnames(id),var_name)   
              gini_m[is.na(gini_m[,ncol(gini_m)]),ncol(gini_m)] <- 0
              Gini <- data.frame(gini_l$Gini,gini_l$Gini_eu)
              colnames(Gini) <- c("Gini","Gini_eu")}
    rownames(Gini) <- NULL
    return(list(value=Gini,lin=gini_m))
}


## workhorse
linginiCalc<- function(x, ids, weights = NULL, sort = NULL, na.rm = FALSE) {
     # initializations
    if(isTRUE(na.rm)){
        indices <- !is.na(x)
        x <- x[indices]
        if(!is.null(weights)) weights <- weights[indices]
        if(!is.null(ids)) ids <- ids[indices]
        if(!is.null(sort)) sort <- sort[indices]
     } else if(any(is.na(x))) return(NA)
     # sort values and weights
    order <- if(is.null(sort)) order(x) else order(x, sort)
    x <- x[order]  # order values
    ids <- ids[order]  # order values
    if(is.null(weights)) { weights <- rep.int(1, length(x))  # equal weights
     } else weights <- weights[order]  # order weights

    ## calculations
    taille <- length(weights)   # Sample size
    wx <- weights * x       # weighted values
    N <- sum(weights)     # Estimated population size
    cw <- cumsum(weights)   # cumulative sum of weights
    T<- sum(wx)             # Estimated total income

    Num_eu <- 2 * sum(wx*cw) - sum(weights^2 * x)
    Num <- sum((2*cw-1)*wx)
    Den <- N*T;


    Gini_eu <- 100*(Num_eu/Den-1)
    Gini <- Num/Den-1
    Gini_pr <- 100*Gini

    # COMPUTATION OF A LINEARIZED VARIABLE

    F <- cumsum(weights/N)   #  Estimation of the cumulative distribution function
    G <- cumsum(wx)            #  Weighted partial sum

    # LINEARIZED VARIABLE OF THE GINI COEFFICIENT (IN %)

    lin <- 100*(2*(T-G+wx+N*(x*F))-x-(Gini+1)*(T+N*x))/(N*T)
    
    lin_id <- cbind(ids,lin)
    return(list(Gini_eu=Gini_eu,Gini=Gini_pr,lin=lin_id))
}

