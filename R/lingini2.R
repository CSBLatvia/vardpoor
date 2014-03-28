# ************************************************************************
# ************************************************************************
# ************************************************************************
# ***                                                                  ***
# ***                                                                  ***
# ***            LINEARIZATION OF THE GINI COEFFICIENT II              ***
# ***                                                                  ***
# ***                                                                  ***
# ************************************************************************
# ************************************************************************
# ************************************************************************
# ************************************************************************


lingini2 <- function(inc, id = NULL, weight = NULL, sort = NULL, 
                     Dom = NULL, period=NULL, dataset = NULL,
                     na.rm = FALSE, var_name="lin_gini2") {

   ## initializations
   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if (!is.null(dataset)) {
        if (checker(inc,dataset,"inc")) inc <- dataset[, inc] 

        if (!is.null(id)) {
             id2 <- id
             if (checker(id,dataset,"id")) id <- data.frame(dataset[, id], stringsAsFactors=FALSE)
             names(id) <- id2 }

        if (!is.null(weight)) {
             if (checker(weight,dataset,"weight")) weight <- dataset[, weight] }

        if (!is.null(sort)) {
             if (checker(sort,dataset,"sort")) sort <- dataset[, sort] }

        if (!is.null(period)) {
            aperiod <- period  
            if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
            if (min(period %in% names(dataset))==1) {
                                period <- data.frame(dataset[, period], stringsAsFactors=FALSE)
                                names(period) <- aperiod }}

        if (!is.null(Dom)) {
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
   if(!is.numeric(inc)) stop("'inc' must be a numerical")
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
   Dom1 <- Dom
   if (!is.null(period)) {
       if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 
 

  # Gini by domain (if requested)

   if (!is.null(Dom1)) {
        Dom_agg <- data.table(unique(Dom1))
        setkeyv(Dom_agg, names(Dom1))
  
        Gini <- c()
        gini_id <- id
        if (!is.null(period)) gini_id <- data.table(period, gini_id)
        gini_m <- gini_id
        for(i in 1:nrow(Dom_agg)) {
                g <- paste(names(Dom1), as.matrix(Dom_agg[i,]), sep = ".")
                breakdown2 <- do.call(paste, as.list(c(g, sep="__")))
                D <- Dom_agg[i,][rep(1, nrow(Dom1)),]
                ind <- (rowSums(Dom1 == D) == ncol(Dom1))
                gini_l <- lingini2Calc(inc[ind], gini_id[ind], weight[ind], sort[ind], na.rm=na.rm)
                ginilin <- gini_l$lin
                setnames(ginilin, names(ginilin), c(names(gini_id), paste(var_name, breakdown2, sep="__")))
                gini_m <- merge(gini_m, ginilin, by=names(gini_id), all.x=T, sort=FALSE)
                Gini <- rbind(Gini, data.frame(gini_l$Gini, gini_l$Gini_eu))           
             }
      colnames(Gini) <- c("Gini", "Gini_eu")
      Gini <- data.table(Dom_agg, Gini)
     } else { gini_l <- lingini2Calc(inc, id, weight, sort, na.rm=na.rm)
              gini_m <- gini_l$lin
              setnames(gini_m, names(gini_m), c(names(id),var_name))   
              Gini <- data.frame(gini_l$Gini, gini_l$Gini_eu)
              colnames(Gini) <- c("Gini", "Gini_eu")}
    rownames(Gini) <- NULL
    gini_m[is.na(gini_m)] <- 0
    return(list(value=Gini, lin=gini_m))
}


## workhorse
lingini2Calc <- function(x, ids, weights = NULL, sort = NULL, na.rm = FALSE) {
     # initializations
    if (isTRUE(na.rm)){
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
    if (is.null(weights)) { weights <- rep.int(1, length(x))  # equal weights
     } else weights <- weights[order]  # order weights

    ## calculations
    taille <- length(weights)   # Sample size
    wx <- weights * x       # weighted values
    N <- sum(weights)     # Estimated population size
    cw <- cumsum(weights)   # cumulative sum of weights
    T <- sum(wx)             # Estimated total income

    Nk <- c()
    wx1 <- c()

    for(i in 1:length(x)) {
        Nk[i] <- sum(weights*(x<=x[i]))       #  Estimation of the cumulative distribution function
        wx1[i]<- sum(wx*(x<=x[i]))           #  Weighted partial sum
    }

    Num_eu <- 2 * sum(wx*cw) - sum(weights^2 * x)
    Num <- 2 * sum(wx*Nk) - sum(weights^2 * x)

    Den <- N*T;

    Gini_eu <- 100*(Num_eu/Den-1)
    Gini <- Num/Den-1

    Gini_pr <- 100*Gini
        

    # LINEARIZED VARIABLE OF THE GINI COEFFICIENT (IN %)

    lin <- 100*(2*Nk*(x-wx1/Nk)+T-N*x-Gini*(T+N*x))/(N*T)
    
    lin_id <- data.table(ids,lin)

    return(list(Gini_eu=Gini_eu, Gini=Gini_pr, lin=lin_id))

}

