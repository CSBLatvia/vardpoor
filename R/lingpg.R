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
        Dom = NULL, dataset = NULL, na.rm = FALSE, var_name="lin_gpg") {

   ## initializations

   if (min(dim(as.data.frame(var_name))==1)!=1) {
       stop("'var_name' must have defined name of the linearized variable")}

   if(is.null(gender)) stop("'gender' must be supplied")

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

   # gender
   if(!is.factor(gender)) stop("'gender' must be a factor.")
   if(length(levels(gender)) != 2) stop("'gender' must have exactly two levels")
   if(!all(levels(gender) == c(1, 2))) {
        gender <- factor(gender, labels=c(1,2))
        warning("The levels of gender were internally recoded - your first level has to correspond to males")
       } 
   if(length(gender) != n) stop("'gender' must have the same length as 'x'")

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

   # GPG by domain (if requested)
   if(!is.null(Dom)) {
      D <- as.matrix(Dom) 
      Dom0 <- as.matrix(unique(D))
      Dom_agg <- Dom0[do.call("order", lapply(1:NCOL(Dom0), function(i) Dom0[, i])), ]
      Dom_agg <- as.matrix(Dom_agg)
      colnames(Dom_agg) <- colnames(Dom)

      gpg_pr <- c()
      gpg_m <- id
 
      for(i in 1:nrow(Dom_agg)) {
              c <- paste(colnames(Dom), Dom_agg[i,], sep=".")
              breakdown2 <- do.call(paste, as.list(c(c, sep="__")))
              ind <- (rowSums(Dom == matrix(Dom_agg[i,], n, ncol(D),1))== ncol(D))            
              gpg_l <- linGapCalc(inc[ind,1],gender[ind],id[ind,1], weight[ind], sort[ind], na.rm=na.rm)
              gpglin <- gpg_l$lin
              colnames(gpglin) <- c(colnames(id),paste(var_name,breakdown2,sep="__"))
              gpg_pr <- rbind(gpg_pr,gpg_l$gpg_pr)           
              gpg_m <- merge(gpg_m,gpglin,by=colnames(id),all.x=T)
              gpg_m[is.na(gpg_m[,ncol(gpg_m)]),ncol(gpg_m)] <- 0
       }
       colnames(gpg_pr)[ncol(gpg_pr)] <- c("gender_pay_gap")
       gpg_pr <- data.frame(Dom_agg,gpg_pr)
     } else { Dom_agg<-NULL
              gpg_l <- linGapCalc(inc[,1],gender,id[,1], weight, sort, na.rm=na.rm)
              gpg_m <- gpg_l$lin
              colnames(gpg_m) <- c(colnames(id),var_name)
              gpg_m[is.na(gpg_m[,ncol(gpg_m)]),ncol(gpg_m)] <- 0
              gpg_pr <- data.frame(gpg_l$gpg_pr)  
              colnames(gpg_pr)[ncol(gpg_pr)] <- c("gender_pay_gap")
       }
    rownames(gpg_pr) <- NULL
   
    return(list(value=gpg_pr, lin=gpg_m))
}


  ## workhorse
linGapCalc<-function(x, gend, ids, weights = NULL, sort = NULL, na.rm = FALSE) {
    if(is.null(gend)) stop("'gender' must be supplied")
    if (length(gend)!=length(x)) stop("'x' is not the same as 'gend'")
    if (length(gend)!=length(weights)) stop("'weights' is not the same as 'gend'")
    # initializations
    if(isTRUE(na.rm)){
        indices <- !is.na(x)
        x <- x[indices]
        ids <- ids[indices]
        gend <- gend[indices]
        if(!is.null(weights)) weights <- weights[indices]
        if(!is.null(id)) id <- id[indices]
        if(!is.null(sort)) sort <- sort[indices]
     } else if(any(is.na(x))) return(NA)
    
    if(is.null(weights)) weights <- rep.int(1, length(x))  # equal weights

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

