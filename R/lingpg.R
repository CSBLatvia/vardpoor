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
                   na.rm = FALSE, var_name="lin_gpg") {

   ## initializations

   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if (is.null(gender)) stop("'gender' must be supplied")

   if (!is.null(dataset)) {
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
   if (any(is.na(inc))) warning("'inc' has unknown values")

   # gender
   if (!is.factor(gender)) stop("'gender' must be a factor.")
   if (length(levels(gender)) != 2) stop("'gender' must be exactly two levels")
   if (!all(levels(gender) == c(1, 2))) {
        gender <- factor(gender, labels=c(1,2))
        warning("The levels of gender were internally recoded - your first level has to correspond to males")
       } 
   if (length(gender) != n) stop("'gender' must be the same length as 'inc'")

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
   if (!is.numeric(weight)) stop("'weight' must be a numerical")
 
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


   # GPG by domain (if requested)

   if(!is.null(Dom1)) {
        Dom_agg <- data.table(unique(Dom1))
        setkeyv(Dom_agg, names(Dom1))
        gpg_pr <- c()
        gpg_id <- id
        if (!is.null(period)) gpg_id <- data.table(period, gpg_id)
        gpg_m <- gpg_id
        for(i in 1:nrow(Dom_agg)) {
               g <- paste(names(Dom), as.matrix(Dom_agg[i,]), sep = ".")
               breakdown2 <- do.call(paste, as.list(c(g, sep="__")))
               D <- Dom_agg[i,][rep(1,nrow(Dom1)),]
               ind <- (rowSums(Dom1 == D) == ncol(Dom1))
               gpg_l <- linGapCalc(x=inc[ind], gend=gender[ind],
                                   ids=gpg_id[ind], weights=weight[ind],
                                   sort=sort[ind], na.rm=na.rm)
               gpglin <- gpg_l$lin
               setnames(gpglin, names(gpglin), c(names(gpg_id), paste(var_name, breakdown2, sep="__")))
               gpg_m <- merge(gpg_m, gpglin, by=names(gpg_id), all.x=T, sort=FALSE)
               gpg_pr <- rbind(gpg_pr,gpg_l$gpg_pr)                 }
        colnames(gpg_pr)[ncol(gpg_pr)] <- "gender_pay_gap"
        gpg_pr <- data.table(Dom_agg, gpg_pr)
      } else { gpg_l <- linGapCalc(x=inc, gend=gender, ids=id,
                                   weights=weight, sort=sort,
                                   na.rm=na.rm)
               gpg_m <- gpg_l$lin
               setnames(gpg_m, names(gpg_m), c(names(id), var_name))
               gpg_pr <- gpg_l$gpg_pr 
               setnames(gpg_pr, names(gpg_pr)[ncol(gpg_pr)], "gender_pay_gap")
       }
    gpg_m[is.na(gpg_m)] <- 0  
    return(list(value=gpg_pr, lin=gpg_m))
}

  ## workhorse
linGapCalc <- function(x, gend, ids, weights = NULL, sort = NULL, na.rm = FALSE) {
    if(is.null(gend)) stop("'gender' must be supplied")
    if (length(gend)!=length(x)) stop("'x' is not the same as 'gend'")
    if (length(gend)!=length(weights)) stop("'weights' is not the same as 'gend'")
    # initializations
    if (isTRUE(na.rm)){
          indices <- !is.na(x)
          x <- x[indices]
          ids <- ids[indices]
          gend <- gend[indices]
          if (!is.null(weights)) weights <- weights[indices]
          if (!is.null(id)) id <- id[indices]
          if (!is.null(sort)) sort <- sort[indices]
      } else if(any(is.na(x))) return(NA)
    
    if (is.null(weights)) weights <- rep.int(1, length(x))  # equal weights

    indic_men <- ifelse((gend==1)&(!is.na(x)),1,0)
    indic_women <- ifelse((gend==2)&(!is.na(x)),1,0)
 
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

   lin_id <- cbind(ids,lin)

   return(list(gpg_pr=gpg_pr,lin=lin_id))
}

