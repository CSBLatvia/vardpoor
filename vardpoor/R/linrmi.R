#******************************************************************************************
#******************************************************************************************
#******************************************************************************************
#                                                                                
#                                                                                
#                       LINEARIZATION OF THE RELATIVE MEDIAN INCOME RATIO     
#
#
#******************************************************************************************
#******************************************************************************************
#******************************************************************************************

linrmi <- function(Y, id, age, weight=NULL,  sort=NULL, 
                             Dom=NULL, period=NULL, dataset = NULL, 
                             order_quant=50, var_name="lin_rmi") {
 
   ## initializations
   if (min(dim(data.table(var_name))==1)!=1) {
       stop("'var_name' must have defined one name of the linearized variable")}

   # check 'order_quant'

   oq <- order_quant
   if(!is.numeric(oq) || length(oq) != 1 || oq[1] < 0 || oq[1] > 100) {
          stop("'order_quant' must be a numeric value in [0,100]")
      } else order_quant <- order_quant[1]

   if(!is.null(dataset)) {
       dataset <- data.frame(dataset)
       if (checker(Y, dataset, "Y"))  Y <- dataset[, Y] 

       if(!is.null(id)) {
          id2 <- id
          if (checker(id, dataset, "id")) id <- data.frame(dataset[, id], stringsAsFactors=FALSE)
          names(id) <- id2 }

       if(!is.null(age)) {
           if (checker(age, dataset, "age")) age <- dataset[, age] }

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
                    Dom <- data.table(dataset[, Dom]) 
                    setnames(Dom, names(Dom), Dom2) }    }
      }

   # check vectors
   # Y
   Y <- data.frame(Y)
   n <- nrow(Y)
   if (ncol(Y) != 1) stop("'Y' must be vector or 1 column data.frame, matrix, data.table")
   Y <- Y[,1]
   if(!is.numeric(Y)) stop("'Y' must be numerical")
   if (any(is.na(Y))) stop("'Y' has unknown values")

   # weight
   weight <- data.frame(weight)
   if (nrow(weight) != n) stop("'weight' must be the same length as 'Y'")
   if (ncol(weight) != 1) stop("'weight' must be vector or 1 column data.frame, matrix, data.table")
   weight <- weight[, 1]
   if (!is.numeric(weight)) stop("'weight' must be numerical")
   if (any(is.na(weight))) stop("'weight' has unknown values")
   
   # age
   age <- data.frame(age)
   if (nrow(age) != n) stop("'age' must be the same length as 'Y'")
   if (ncol(age) != 1) stop("'age' must be vector or 1 column data.frame, matrix, data.table")
   age <- age[, 1]
   if (!is.numeric(age)) stop("'age' must be numerical")
   if (any(is.na(age))) stop("'age' has unknown values")
   
   # sort
   if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
         stop("'sort' must be a vector or ordered factor") }
   if (!is.null(sort) && length(sort) != n) stop("'sort' must be the same length as 'Y'")  

   # period     
   if (!is.null(period)) {
       period <- data.table(period)
       if (any(duplicated(names(period)))) 
                 stop("'period' are duplicate column names: ", 
                      paste(names(period)[duplicated(names(period))], collapse = ","))
       if (nrow(period) != n) stop("'period' must be the same length as 'Y'")
       if(any(is.na(period))) stop("'period' has unknown values")  
   }   
   
   # id
   if (is.null(id)) id <- 1:n
   id <- data.table(id)
   if (any(is.na(id))) stop("'id' has unknown values")
   if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
   if (nrow(id) != n) stop("'id' must be the same length as 'Y'")
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
             if (nrow(Dom) != n) stop("'Dom' must be the same length as 'Y'")
             Dom <- Dom[, lapply(.SD, as.character), .SDcols = names(Dom)]
       }

    ## computations
    ind0 <- rep.int(1, n)
    period_agg <- period1 <- NULL
    if (!is.null(period)) { period1 <- copy(period)
                            period_agg <- data.table(unique(period))
                        } else period1 <- data.table(ind=ind0)
    period1_agg <- data.table(unique(period1))

    # RMI by domain (if requested)
    age_under_65s <- data.table(age_under_65s=as.integer(age < 65))
    if (!is.null(Dom)) age_under_65s <- data.table(age_under_65s, Dom)

    quantile <- incPercentile(Y = Y, weights = weight,
                                sort = sort, Dom = age_under_65s,
                                period = period, k = order_quant,
                                dataset = NULL)
    quantile <- data.table(quantile)
    quantile_under_65 <- quantile[age_under_65s==1][, age_under_65s:=NULL]
    quantile_over_65 <- quantile[age_under_65s==0][, age_under_65s:=NULL]
    setnames(quantile_under_65, names(quantile_under_65)[ncol(quantile_under_65)], "quantile_under_65")
    setnames(quantile_over_65, names(quantile_over_65)[ncol(quantile_over_65)], "quantile_over_65")
    sk <- length(names(quantile_under_65))-1
    if (sk > 0) {
               setkeyv(quantile_under_65, names(quantile_under_65)[1:sk])
               setkeyv(quantile_over_65, names(quantile_over_65)[1:sk])
               quantile <- merge(quantile_under_65, quantile_over_65, all=TRUE)
        } else quantile <- data.table(quantile_under_65, quantile_over_65)
  
    rmi_id <- id
    age_under_65s <- age_under_65s[["age_under_65s"]]
    if (!is.null(period)) rmi_id <- data.table(rmi_id, period)

  if (!is.null(Dom)) {
        Dom_agg <- data.table(unique(Dom))
        setkeyv(Dom_agg, names(Dom_agg))
          
        rmi_v <- c()
        rmi_m <- copy(rmi_id)
        for(i in 1:nrow(Dom_agg)) {
              g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
              var_nams <- do.call(paste, as.list(c(g, sep="__")))
              ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))

              rmil <- lapply(1:nrow(period1_agg), function(j) {
                               if (!is.null(period)) { 
                                       rown <- cbind(period_agg[j], Dom_agg[i])
                                       setkeyv(rown, names(rown))
                                       rown2 <- copy(rown)
                                       rown <- merge(rown, quantile, all.x=TRUE)
                                     } else {rown <- quantile
                                             rown2 <- Dom_agg[i] }

                               indj <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                               rmi_l <- rmilinCalc(Y1=Y[indj],
                                                   ids=rmi_id[indj],
                                                   wght=weight[indj],
                                                   indicator=ind[indj],
                                                   order_quants=order_quant,
                                                   age_under_65=age_under_65s[indj],
                                                   quant_under_65=rown[["quantile_under_65"]],
                                                   quant_over_65=rown[["quantile_over_65"]])

                      list(rmi=data.table(rown2, rmi=rmi_l$rmi_val), lin=rmi_l$lin)
                      })
                 rmis <- rbindlist(lapply(rmil, function(x) x[[1]]))
                 rmilin <- rbindlist(lapply(rmil, function(x) x[[2]]))

                 setnames(rmilin, names(rmilin), c(names(rmi_id), var_nams))
                 setkeyv(rmi_m, names(rmi_id))
                 setkeyv(rmilin, names(rmi_id))
                 rmi_m <- merge(rmi_m, rmilin, all.x=TRUE)
                 rmi_v <- rbind(rmi_v, rmis) 
           }
     } else { rmil <- lapply(1:nrow(period1_agg), function(j) {
                           if (!is.null(period)) { 
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x=TRUE)
                                       } else rown <- quantile
                           ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))
      
                           rmi_l <- rmilinCalc(Y1=Y[ind2],
                                                 ids=rmi_id[ind2],
                                                 wght=weight[ind2],
                                                 indicator=ind0[ind2],
                                                 order_quants=order_quant,
                                                 age_under_65=age_under_65s[ind2],
                                                 quant_under_65=rown[["quantile_under_65"]],
                                                 quant_over_65=rown[["quantile_over_65"]])
                          if (!is.null(period)) { 
                                   rmis <- data.table(period_agg[j], rmi=rmi_l$rmi_val)
                             } else rmis <- data.table(rmi=rmi_l$rmi_val)
                          list(rmi=rmis, lin=rmi_l$lin)
                       })
               rmi_v <- rbindlist(lapply(rmil, function(x) x[[1]]))
               rmi_m <- rbindlist(lapply(rmil, function(x) x[[2]]))
               setnames(rmi_m, names(rmi_m), c(names(rmi_id), var_name))
            } 
     rmi_m[is.na(rmi_m)] <- 0
     setkeyv(rmi_m, names(rmi_id))
     return(list(value=rmi_v, lin=rmi_m))
}





## workhorse
rmilinCalc <- function(Y1, ids, wght, indicator, order_quants, age_under_65, quant_under_65, quant_over_65) {

    dom1 <- (age_under_65==1) * indicator
    dom2 <- (age_under_65==0) * indicator
 
   # Size of the domains
    N1 <- sum(wght * dom1)   
    N2 <- sum(wght * dom2) 
				
    rmi_val <- quant_over_65/quant_under_65  # Estimated relative median income ratio

    # Bandwith parameter - h=S/N^(1/5) (calculated over the whole population) 

    h <- sqrt((sum(wght*Y1*Y1)-sum(wght*Y1)*sum(wght*Y1)/sum(wght))/sum(wght))/exp(0.2*log(sum(wght))) 


    #---- 1. Linearization of the median income of people aged below 65 ----

    u1 <- (quant_under_65-Y1) * dom1/h
    vect_f1 <- exp(-(u1^2)/2)/sqrt(2*pi)
    f_quant1 <- sum(vect_f1*wght)/(N1*h)   # Estimate of F'(quantile)

    lin_quant_under_65 <- -(1/N1)*dom1*((Y1<=quant_under_65)-order_quants/100)/f_quant1  # Linearized variable

    #---- 2. Linearization of the median income of people aged above 65 -----

    u2 <- (quant_over_65-Y1) * dom2/h
    vect_f2 <- exp(-(u2^2)/2)/sqrt(2*pi)
    f_quant2 <- sum(vect_f2*wght)/(N2*h)   # Estimate of F'(quantile)

    lin_quant_over_65 <- -(1/N2)*dom2*((Y1<=quant_over_65)-order_quants/100)/f_quant2  # Linearized variable

   #********************************************************************************
   #         3. Linearization of the relative median income ratio                  *
   #********************************************************************************
    lin <- (quant_under_65 * lin_quant_over_65 - quant_over_65 * lin_quant_under_65)/(quant_under_65*quant_under_65)

    lin_id <- data.table(ids, lin)
    return(list(rmi_val=rmi_val, lin=lin_id))
}

