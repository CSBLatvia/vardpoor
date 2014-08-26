#******************************************************************************************
#******************************************************************************************
#******************************************************************************************
#***                                                                                    ***
#***                                                                                    ***
#***                        LINEARIZATION OF THE GENDER PAY GAP                         ***
#***                                                                                    ***
#***                                                                                    ***
#******************************************************************************************
#******************************************************************************************
#******************************************************************************************

lingpg <- function(inc, gender = NULL, id, weight=NULL, sort = NULL,
                   Dom = NULL, period=NULL, dataset = NULL,
                   var_name="lin_gpg") {

   ## initializations

   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if (is.null(gender)) stop("'gender' must be supplied")

   if (!is.null(dataset)) {
        dataset <- data.frame(dataset)
        if (checker(inc, dataset, "inc")) inc <- dataset[, inc] 
        if (checker(gender, dataset, "gender")) gender <- dataset[, gender]  

        if (!is.null(id)) {
             id2 <- id
             if (checker(id, dataset, "id")) id <- data.frame(dataset[, id], stringsAsFactors=FALSE)
             names(id) <- id2 }

        if (!is.null(weight)) {
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
   if(!is.numeric(inc)) stop("'inc' must be a numeric vector")                   
   if (any(is.na(inc))) stop("'inc' has unknown values")

   # gender
   if (!is.numeric(gender)) stop("'gender' must be numerical")
   if (length(gender) != n) stop("'gender' must be the same length as 'inc'")
   if (length(unique(gender)) != 2) stop("'gender' must be exactly two values")
   if (!all.equal(unique(gender),c(1, 2))) stop("'gender' must be value 1 for male, 2 for females")
 
   # weight
   weight <- data.frame(weight)
   if (is.null(weight)) weight <- data.frame(rep.int(1, n))
   if (nrow(weight) != n) stop("'weight' must be the same length as 'inc'")
   if (ncol(weight) != 1) stop("'weight' must be vector or 1 column data.frame, matrix, data.table")
   weight <- weight[,1]
   if (!is.numeric(weight)) stop("'weight' must be a numerical")
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

   # GPG by domain (if requested)
   gpg_id <- id
   if (!is.null(period)) gpg_id <- data.table(gpg_id, period)

   if(!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))

        gpg_v <- c()
        gpg_m <- copy(gpg_id)

        for(i in 1:nrow(Dom_agg)) {
            g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
            var_nams <- do.call(paste, as.list(c(g, sep="__")))
            indi <- (rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))
            
            gpg_l <- lapply(1:nrow(period1_agg), function(j) {
                 indj <- ((rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))&(indi))
                 if (!is.null(period)) { rown <- cbind(period_agg[j], Dom_agg[i])
                                     } else rown <- Dom_agg[i] 
                 if (!all(indj)) {
                      gpgl <- linGapCalc(x=inc[indj], gend=gender[indj],
                                         ids=gpg_id[indj], weights=weight[indj],
                                         sort=sort[indj])
                      list(data.table(rown, gpg=gpgl$gpg_pr), gpgl$lin)
                     }  else list(data.table(rown, gpg=0, Gini_eu=0),                                            
                                lin=data.table(lin=gpg_id[indi], lin=0))
              })

            gpgs <- rbindlist(lapply(gpg_l, function(x) x[[1]]))
            gpglin <- rbindlist(lapply(gpg_l, function(x) x[[2]]))

            setnames(gpglin, names(gpglin), c(names(gpg_id), var_nams))
            setkeyv(gpg_m, names(gpg_id))
            setkeyv(gpglin, names(gpg_id))
            gpg_m <- merge(gpg_m, gpglin, all.x=T)
            gpg_v <- rbind(gpg_v, gpgs) 
          }
      } else { gpg_l <- lapply(1:nrow(period1_agg), function(j) {
                           if (!is.null(period)) { 
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x=TRUE)
                                       } else rown <- quantile
                           indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))
      
                           gpg_l <- linGapCalc(x=inc[indj], gend=gender[indj],
                                               ids=gpg_id[indj], weights=weight[indj],
                                               sort=sort[indj])

                          if (!is.null(period)) { 
                                   gpgs <- data.table(period_agg[j], gpg=gpg_l$gpg)
                             } else gpgs <- data.table(gpg=gpg_l$gpg)
                          list(gpg=gpgs, lin=gpg_l$lin)
                       })
               gpg_v <- rbindlist(lapply(gpg_l, function(x) x[[1]]))
               gpg_m <- rbindlist(lapply(gpg_l, function(x) x[[2]]))
               setnames(gpg_m, names(gpg_m), c(names(gpg_id), var_name))
            } 
    gpg_m[is.na(gpg_m)] <- 0  
    setkeyv(gpg_m, names(gpg_id))
    return(list(value=gpg_v, lin=gpg_m))
}


  ## workhorse
linGapCalc <- function(x, gend, ids, weights = NULL, sort = NULL) {
    if(is.null(gend)) stop("'gender' must be supplied")
    if (length(gend)!=length(x)) stop("'x' is not the same as 'gend'")
    if (length(gend)!=length(weights)) stop("'weights' is not the same as 'gend'")
   
    if (is.null(weights)) weights <- rep.int(1, length(x))  # equal weights

    indic_men <- ifelse(gend==1, 1, 0)
    indic_women <- ifelse(gend==2, 1, 0)
 
    x[is.na(x)] <- 0
   
    Nmen <- sum(weights*indic_men)
    Nwomen <- sum(weights*indic_women)
    SINCmen <- sum(weights*x*indic_men)
    SINCwomen <- sum(weights*x*indic_women)
  
    Num <- (SINCmen/Nmen)-(SINCwomen/Nwomen)
    Den <- (SINCmen/Nmen)
    gpg <- Num/Den # Estimated gender pay gap 
    gpg_pr <- gpg*100 
 
 #-------------------------- Linearized variable (in %) -----------------------
   lin <- 100*(1-gpg)*((indic_men/Nwomen)-(indic_men/Nmen)+((x*indic_men)/SINCmen)-((x*indic_women)/SINCwomen))
 #-----------------------------------------------------------------------------

   lin_id <- cbind(ids, lin)

   gpg <- data.table(gpg=gpg)
   return(list(gpg_pr=gpg_pr, lin=lin_id))
}

