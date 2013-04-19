
checker <- function(variables, datasets, varname) {
      vars <- 0
      if (sum(variables %in% names(datasets)) > 0) { vars <- vars + 100 
         } else vars <- vars + 1
      if (is.numeric(variables)) {
         if ((variables <= ncol(datasets))*(0 < variables)) { vars <- vars + 100
            } else vars <- vars + 10 }
      if (is.logical(variables)) {
         if (nrow(as.data.frame(variables)) == ncol(datasets)) { vars <- vars + 100
            } else vars <- vars + 20 }
       
      if (vars == 1)  stop(paste(variables,"does not exist in 'dataset'!", sep = " "))
      if (vars == 11) stop(paste("Column",variables,"does not exist in 'dataset'!", sep = " "))
      if (vars == 21) stop(paste("'",varname,"' logical vector must be the same length as 'dataset' column count!", sep = ""))

      return(ifelse(vars >= 100, TRUE, FALSE))
 }

incPercentile <- function(inc, weights = NULL, sort = NULL, 
        Dom = NULL, k = c(20, 80), dataset = NULL, na.rm = FALSE) {
   
   ## initializations
   if(!is.numeric(k) || length(k) == 0 || any(k < 0 | k > 100)) {
        stop("'k' must be a vector of integers between 0 and 100")
    } else k <- round(k)
   
   if(!is.null(dataset)) {
       if (checker(inc,dataset,"inc")) inc <- dataset[, inc] 

       if(!is.null(weights)) {
           if (checker(weights,dataset,"weights")) weights <- dataset[, weights] }
     
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
 
   # weights
   if(is.null(weights)) { weights <- rep.int(1, n)
       } else if(!is.numeric(weights)) stop("'weights' must be a numeric vector")
   if(length(weights) != n) stop("'weights' must have the same length as 'x'")
   
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
 
    # Percentiles by domain (if requested)
    if(!is.null(Dom)) {
        D<-as.matrix(Dom) 
        Dom0 <- as.matrix(unique(D))
        Dom_agg<-Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
        Dom_agg<-as.matrix(Dom_agg)
        colnames(Dom_agg)<-colnames(Dom) 
        
        q=c()               
        for(i in 1:nrow(Dom_agg)) {
              index <- (rowSums(Dom == matrix(Dom_agg[i,], n, ncol(D),TRUE))== ncol(D))
              incind<-inc[index,1]
              weightsind<-weights[index]
              sortind<-sort[index] 
              order <- if(is.null(sortind)) order(incind) else order(incind, sortind)
              incind <- incind[order]
              weightsind <- weightsind[order]  # also works if 'weights' is NULL                               
              percentile<-weightedQuantile(incind, weightsind, probs=k/100, sorted=FALSE, na.rm=na.rm)
              q<-rbind(q,percentile)
              rownames(q)<-NULL
              colnames(q)<-k }
       q<-data.frame(Dom_agg,q)
    } else { order <- if(is.null(sort)) order(inc[,1]) else order(inc[,1], sort)
             inc <- inc[order,1]
             weights <- weights[order]  # also works if 'weights' is NULL
             q<-weightedQuantile(inc, weights, probs=k/100, sorted=TRUE, na.rm=na.rm)
             names(q)<-k  # use percentile numbers as names
     }
    ## return results
    return(q)
}

