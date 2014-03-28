
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
      if (vars == 21) stop(paste0("'",varname,"' logical vector must be the same length as 'dataset' column count!"))

      return(ifelse(vars >= 100, TRUE, FALSE))
 }

incPercentile <- function(inc, weights = NULL, sort = NULL, 
        Dom = NULL, k = c(20, 80), dataset = NULL, na.rm = FALSE) {
   
   ## initializations
   if(!is.numeric(k) || length(k) == 0 || any(k < 0 | k > 100)) {
        stop("'k' must be a vector of integers between 0 and 100")
    } else k <- round(k)
   
   if(!is.null(dataset)) {
       dataset <- data.frame(dataset)
       if (checker(inc, dataset, "inc")) inc <- dataset[, inc] 

       if(!is.null(weights)) {
           if (checker(weights, dataset, "weights")) weights <- dataset[, weights] }
     
       if(!is.null(sort)) {
           if (checker(sort, dataset, "sort")) sort <- dataset[, sort] }

       if (!is.null(Dom)) {
            Dom2 <- Dom
            if (checker(Dom,dataset,"Dom")) {
                    Dom <- data.frame(dataset[, Dom]) 
                    colnames(Dom) <- Dom2     }}
      }

   # check vectors
   # inc
   inc <- data.frame(inc)
   n <- nrow(inc)
   if (ncol(inc) != 1) stop("'inc' must be a vector or 1 column data.frame, matrix, data.table")
   inc <- inc[,1]
   if(!is.numeric(inc)) stop("'inc' must be numerical")
   if (any(is.na(inc))) warning("'inc' has unknown values")
   if (all(is.na(inc))) warning("'inc' has unknown all values")

   # weights
   weights <- data.frame(weights)
   if(is.null(weights)) weights <- data.frame(rep.int(1, n))
   if (nrow(weights) != n) stop("'weights' must be the same length as 'x'")
   if (ncol(weights) != 1) stop("'weights' must be vector or 1 column data.frame, matrix, data.table")
   weights <- weights[,1]
   if(!is.numeric(weights)) stop("'weights' must be numerical")

   # sort  
   if(!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
         stop("'sort' must be a vector or ordered factor") }
   if(!is.null(sort) && length(sort) != n) stop("'sort' must be the same length as 'x'")

   # Dom
   if(!is.null(Dom)) {
             Dom <- data.table(Dom)
             if (is.null(names(Dom))) stop("'Dom' must be colnames")
             if (nrow(Dom) != n) stop("'Dom' must be the same length as 'inc'")
       }
  
    ## computations
 
    # Percentiles by domain (if requested)
    if(!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg,colnames(Dom))
        q <- c()
        for(i in 1:nrow(Dom_agg)) {
               D <- Dom_agg[i,][rep(1, nrow(Dom)),]
               index <- rowSums(Dom == D) == ncol(Dom)
               incind <- inc[index]
               weightsind <- weights[index]
               sortind <- sort[index]
               order <- if(is.null(sortind)) order(incind) else order(incind, sortind)
               incind <- incind[order]
               weightsind <- weightsind[order]  # also works if 'weights' is NULL                               
               percentile <- weightedQuantile(incind, weightsind, probs=k/100, sorted=FALSE, na.rm=na.rm)               
               q <- rbind(q,percentile)   }
       colnames(q) <- k
       rownames(q) <- NULL
       q <- data.frame(Dom_agg, q)
     } else {  order <- if(is.null(sort)) order(inc) else order(inc, sort)
               inc <- inc[order]
               weights <- weights[order]  # also works if 'weights' is NULL
               q <- weightedQuantile(inc, weights, probs=k/100, sorted=TRUE, na.rm=na.rm)
               names(q) <- k  # use percentile numbers as names
     }
     ## return results
    return(q)
}

