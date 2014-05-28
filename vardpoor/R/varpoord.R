
varpoord <- function(inc, w_final, 
                     income_thres = NULL,
                     wght_thres = NULL,
                     ID_household,
                     id = NULL, 
                     H, PSU, N_h,
                     fh_zero = FALSE,
                     PSU_level=TRUE,
                     sort = NULL,
                     Dom = NULL,
                     period = NULL,
                     gender = NULL,
                     dataset = NULL,
                     X = NULL,
                     periodX = NULL,
                     X_ID_household = NULL,
                     ind_gr = NULL,
                     g = NULL,
                     datasetX = NULL,
                     q = rep(1, if (is.null(datasetX)) 
                            nrow(data.frame(X)) else nrow(datasetX)),
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     confidence = .95,
                     outp_lin = FALSE,
                     outp_res = FALSE,
                     na.rm=FALSE,
                     several.ok=FALSE,
                     type="lin_rmpg") {

  ### Checking

  if (!is.logical(fh_zero)) stop("'fh_zero' must be the logical value")
  if (!is.logical(PSU_level)) stop("'PSU_level' must be the logical value")

  all_choices <- c("linarpr","linarpt","lingpg","linpoormed",
                   "linrmpg","lingini","lingini2","linqsr")
  choices <- c("all_choises",all_choices)
  type <- tolower(type)
  type <- match.arg(type,choices, several.ok)
  if (any(type == "all_choises")) type <- all_choices

  # check 'p'
  p <- percentage
  if(!is.numeric(p) || length(p) != 1 || p[1] < 0 || p[1] > 100) {
         stop("'percentage' must be a numeric value in [0,100]")
     } else p <- percentage[1]

  # check 'order_quant'

  oq <- order_quant
  if(!is.numeric(oq) || length(oq) != 1 || oq[1] < 0 || oq[1] > 100) {
         stop("'order_quant' must be a numeric value in [0,100]")
     } else order_quant <- order_quant[1]

  if(!is.numeric(alpha) || length(alpha) != 1 || alpha[1] < 0 || alpha[1] > 100) {
         stop("'alpha' must be a numeric value in [0,100]")  }

  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
         stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {

      dataset <- data.frame(dataset)

      if (min(inc %in% names(dataset))!=1) stop("'inc' does not exist in 'dataset'!")
      if (min(inc %in% names(dataset))==1) inc <- dataset[, inc]
      if(!is.null(w_final)) {
          if (min(w_final %in% names(dataset))!=1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset))==1) w_final <- dataset[, w_final] }
      if(!is.null(income_thres)) {
          if (min(income_thres %in% names(dataset))!=1) stop("'income_thres' does not exist in 'dataset'!")
          if (min(income_thres %in% names(dataset))==1) income_thres <- dataset[, income_thres] }    
      if(!is.null(wght_thres)) {
          if (min(wght_thres %in% names(dataset))!=1) stop("'wght_thres' does not exist in 'dataset'!")
          if (min(wght_thres %in% names(dataset))==1) wght_thres <- dataset[, wght_thres] }
      if(!is.null(id)) {
          id2 <- id
          if (min(id %in% names(dataset))!=1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset))==1) id <- data.frame(dataset[, id], stringsAsFactors=FALSE)
          names(id) <- id2  }
      if(!is.null(ID_household)) {
          ID_household2 <- ID_household
          if (min(ID_household %in% names(dataset))!=1) stop("'ID_household' does not exist in 'dataset'!")
          if (min(ID_household %in% names(dataset))==1) ID_household <- as.data.frame(dataset[, ID_household], stringsAsFactors=FALSE)
          names(ID_household) <- ID_household2  }
      if(!is.null(H)) {
          aH <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- as.data.frame(dataset[, H], stringsAsFactors=FALSE)
                                names(H) <- aH }}
      if(!is.null(PSU)) {
          aPSU <- PSU  
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) {
                                PSU <- as.data.frame(dataset[, PSU], stringsAsFactors=FALSE)
                                names(PSU) <- aPSU }}
      if(!is.null(gender)) {
          if (min(gender %in% names(dataset))!=1) stop("'gender' does not exist in 'dataset'!")
          if (min(gender %in% names(dataset))==1) gender <- dataset[, gender] }
      if(!is.null(sort)) {
          if (min(sort %in% names(dataset))!=1) stop("'sort' does not exist in 'dataset'!")
          if (min(sort %in% names(dataset))==1) sort <- dataset[, sort] }
      if (!is.null(period)) {
            aperiod <- period  
            if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
            if (min(period %in% names(dataset))==1) {
                                period <- as.data.frame(dataset[, period], stringsAsFactors=FALSE)
                                names(period) <- aperiod  }}
      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'dataset'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom], stringsAsFactors=FALSE) 
                  names(Dom) <- Dom2 }    }
      }

  if(!is.null(datasetX)) {
       if (!is.null(periodX)) {
            aperiodX <- periodX  
            if (min(periodX %in% names(datasetX))!=1) stop("'periodX' does not exist in 'datasetX'!")
            if (min(periodX %in% names(datasetX))==1) {
                                periodX <- as.data.frame(dataset[, periodX])
                                names(periodX) <- aperiodX }}     
      if(!is.null(X_ID_household)) {
          X_ID_household2 <- X_ID_household
          if (min(X_ID_household %in% names(datasetX))!=1) stop("'X_ID_household' does not exist in 'datasetX'!")
          if (min(X_ID_household %in% names(datasetX))==1) X_ID_household <- data.frame(datasetX[, X_ID_household])
          names(X_ID_household) <- X_ID_household2  }
      if(!is.null(X)) {
          if (min(X %in% names(datasetX))!=1) stop("'X' does not exist in 'datasetX'!")
          if (min(X %in% names(datasetX))==1) X <- datasetX[, X] }
      if(!is.null(ind_gr)) {
          if (min(ind_gr %in% names(datasetX))!=1) stop("'ind_gr' does not exist in 'datasetX'!")
          if (min(ind_gr %in% names(datasetX))==1) ind_gr <- datasetX[, ind_gr] }
      if(!is.null(g)) {
          if (min(g %in% names(datasetX))!=1) stop("'g' does not exist in 'datasetX'!")
          if (min(g %in% names(datasetX))==1) g <- datasetX[, g] }
      if(!is.null(q)) {
          if (min(q %in% names(datasetX))!=1) {
               if (length(q) != nrow(datasetX))  stop("'q' does not exist in 'datasetX'!") }
          if (min(q %in% names(datasetX))==1) q <- datasetX[, q] } 
      }

  # inc
  inc <- data.frame(inc)
  n <- nrow(inc)
  if (ncol(inc) != 1) stop("'inc' must have vector or 1 column data.frame, matrix, data.table")
  inc <- inc[,1]
  if (!is.numeric(inc)) stop("'inc' must be numerical")
  if (any(is.na(inc))) warning("'inc' has unknown values")
              
  # id
  if (is.null(id)) id <- 1:n
  id <- data.table(id)
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (nrow(id) != n) stop("'id' must be the same length as 'inc'")
  if (is.null(names(id))||(names(id)=="id")) setnames(id,names(id),"ID")

  # ID_household
  if (is.null(ID_household)) stop("'ID_household' must be defined")
  ID_household <- data.table(ID_household)
  if (ncol(ID_household) != 1) stop("'ID_household' must be 1 column data.frame, matrix, data.table")
  if (nrow(ID_household) != n) stop("'ID_household' must be the same length as 'inc'")
  if (is.null(names(ID_household))) setnames(ID_household,names(ID_household),"ID_household")
  if (names(id)==names(ID_household)) setnames(id,names(id),paste(names(id),"_id",sep=""))

  # w_final 
  w_final <- data.frame(w_final)
  if (is.null(w_final)) w_final <- data.frame(rep.int(1, n))
  if (nrow(w_final) != n) stop("'w_final' must have the same length as 'inc'")
  if (ncol(w_final) != 1) stop("'w_final' must have vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[,1]
  if (!is.numeric(w_final)) stop("'w_final' must be numerical")

  # income_thres
  if (!is.null(income_thres)) {
       income_thres <- data.frame(income_thres)
       if (nrow(income_thres) != n) stop("'income_thres' must have the same length as 'inc'")
       if (ncol(income_thres) != 1) stop("'income_thres' must have vector or 1 column data.frame, matrix, data.table")
       income_thres <- income_thres[,1]
       if (!is.numeric(income_thres)) stop("'income_thres' must be numerical")
     } else income_thres <- inc

  # wght_thres
  if (is.null(wght_thres)) wght_thres <- w_final
  wght_thres <- data.frame(wght_thres)
  if (nrow(wght_thres) != n) stop("'wght_thres' must have the same length as 'inc'")
  if (ncol(wght_thres) != 1) stop("'wght_thres' must have vector or 1 column data.frame, matrix, data.table")
  wght_thres <- wght_thres[,1]
  if (!is.numeric(wght_thres)) stop("'wght_thres' must be a numeric vector")
 
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' must have the same length as 'inc'")
  if (ncol(H) != 1) stop("'H' must have 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")

  # PSU
  PSU <- data.table(PSU)
  if (nrow(PSU) != n) stop("'PSU' must have the same length as 'inc'")
  if (ncol(PSU) != 1) stop("'PSU' must have vector or 1 column data.frame, matrix, data.table")
  if (any(is.na(PSU))) stop("'PSU' has unknown values")

  # gender
  if (!is.null(gender)) {
      if (!is.factor(gender)) stop("'gender' must be a factor.")
      if (length(levels(gender)) != 2) stop("'gender' must have exactly two levels")
      if (!all(levels(gender) == c(1, 2))) {
           gender <- factor(gender, labels=c(1,2))
           warning("The levels of gender were internally recoded - your first level has to correspond to males")
          } 
      if (length(gender) != n) stop("'gender' must have the same length as 'x'")
    }

  # period     
  if (!is.null(period)) {
      period <- data.table(period)
      if (any(duplicated(names(period)))) 
                stop("'period' are duplicate column names: ", 
                     paste(names(period)[duplicated(names(period))], collapse = ","))
      if (nrow(period) != n) stop("'period' must be the same length as 'inc'")
      if(any(is.na(period))) stop("'period' has unknown values")  
  } 
  np <- sum(ncol(period))

  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (ncol(N_h) != np+2) stop(paste0("'N_h' should be ",toString(np+2)," columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numerical")
      if (any(is.na(N_h))) stop("'N_h' has unknown values") 
      if (is.null(names(N_h))) stop("'N_h' must be colnames")
      if (is.null(period)) {
             if (names(H) != names(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
             if (any(is.na(merge(unique(H), N_h, by=names(H), all.x = T)))) stop("'N_h' is not defined for all stratas")
       } else { pH <- data.frame(period, H)
                if (any(names(pH) != names(N_h)[c(1:(1+np))])) stop("Strata titles for 'period' with 'H' and 'N_h' is not equal")
                if (any(is.na(merge(unique(pH), N_h, by=names(pH), all.x = T)))) stop("'N_h' is not defined for all stratas and periods")
                pH <- NULL 
     }
    setkeyv(N_h, names(N_h)[c(1:(1+np))])
  }

  # sort
  if (!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
        stop("'sort' must be a vector or ordered factor") }
  if (!is.null(sort) && length(sort) != n) stop("'sort' must have the same length as 'x'")     

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' have different row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
  }

 # X_ID_household
  if (!is.null(X)) {
    X_ID_household <- data.table(X_ID_household)
    if (ncol(X_ID_household) != 1) stop("'X_ID_household' must be 1 column data.frame, matrix, data.table")
    if (any(is.na(X_ID_household))) stop("'X_ID_household' has unknown values")

    IDh <- data.table(unique(ID_household))
    if (!is.null(period)) {X_ID_household <- data.table(periodX, X_ID_household)
                           IDh <- data.table(unique(data.table(period, ID_household)))}
    if (any(duplicated(X_ID_household))) stop("'X_ID_household' have duplicates")
    setkeyv(X_ID_household, names(X_ID_household))
    setkeyv(IDh, names(IDh))

    if (!is.null(period)) {
        if (nrow(IDh) != nrow(X_ID_household)) stop("'period' with 'X_ID_household' and 'unique(period, ID_household)' have different row count")
        if (any(IDh != X_ID_household)) stop("'period' with 'X_ID_household' and 'unique(period, ID_household)' records have different")
      } else {
        if (nrow(IDh) != nrow(X_ID_household)) stop("'X_ID_household' and 'unique(ID_household)' have different row count")
        if (any(IDh != X_ID_household)) stop("'X_ID_household' and 'unique(ID_household)' records have different")
    }}


  # X
  if (!is.null(X)) {
    X <- data.table(X, check.names=T)
    if (nrow(X) != nrow(X_ID_household)) stop("'X' and 'X_ID_household' have different row count")
  }

  # periodX
  if (!is.null(X)) {
     if(!is.null(periodX)) {
        periodX <- data.table(periodX)
        periX <- data.table(unique(periodX))
        setkeyv(periX, names(periX))
        peri <- data.table(unique(period))
        setkeyv(peri, names(peri))
        if (any(duplicated(names(periodX)))) 
                    stop("'periodX' are duplicate column names: ", 
                         paste(names(periodX)[duplicated(names(periodX))], collapse = ","))
        if (nrow(periodX) != nrow(X)) stop("'periodX' length must be equal with 'X' row count")
        if (any(is.na(periodX))) stop("'periodX' has unknown values")
        if (any(peri != periX)) stop("'unique(period)' and 'unique(periodX)' records have different")
      }
   }

  # ind_gr
  if (!is.null(X)) {
     if(is.null(ind_gr)) ind_gr <- rep.int(1, nrow(X)) 
     ind_gr <- data.table(ind_gr, check.names=TRUE)
     if (nrow(ind_gr) != nrow(X)) stop("'ind_gr' length must be equal with 'X' row count")
     if (ncol(ind_gr) != 1) stop("'ind_gr' must be 1 column data.frame, matrix, data.table")
     if (any(is.na(ind_gr))) stop("'ind_gr' has unknown values")
   }

  # X
  if (!is.null(X)) {
       X1 <- data.table(X, check.names=T)
       nX1 <- names(X1)
       ind_gr1 <- copy(ind_gr) 
       if (!is.null(periodX)) ind_gr1 <- data.table(periodX, ind_gr1, check.names=TRUE)
       X2 <- data.table(ind_gr1, X1)
       X1 <- X2[, .N, keyby=names(ind_gr1)][[ncol(ind_gr1)+1]]
       X2 <- X2[,lapply(.SD, function(x) sum(!is.na(x))), keyby=names(ind_gr1), .SDcols=nX1]
       X2 <- X2[, !(names(X2) %in% names(ind_gr)), with=F]
       if (!all(X2==0 | X1==X2)) stop("X has unknown values")
       ind_gr1 <- nX1 <- X1 <- X2 <- NULL
    }

  # g
  if (!is.null(X)) {
    if (is.null(class(g))| class(g)=="function") stop("'g' must be numerical")
    g <- data.frame(g)
    if (nrow(g) != nrow(X)) stop("'g' length must be equal with 'X' row count")
    if (ncol(g) != 1) stop("'g' must be 1 column data.frame, matrix, data.table")
    g <- g[,1]
    if (!is.numeric(g)) stop("'g' must be numerical")
    if (any(is.na(g))) stop("'g' has unknown values")
    if (any(g == 0)) stop("'g' value can not be 0")
   }
    
  # q
  if (!is.null(X)) {
    if (is.null(class(q))| class(q)=="function") stop("'q' must be numerical")
    q <- data.frame(q)
    if (nrow(q) != nrow(X)) stop("'q' length must be equal with 'X' row count")
    if (ncol(q) != 1) stop("'q' must be 1 column data.frame, matrix, data.table")
    q <- q[,1]
    if (!is.numeric(q)) stop("'q' must be numerical")
    if (any(is.na(q))) stop("'q' has unknown values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }


  # Design weights
  if (!is.null(X)) {
             idh <- ID_household
             if (!is.null(period)) idh <- data.table(period, idh)
             idhx <- data.table(X_ID_household, g)
             setnames(idhx, names(idhx)[c(1:(ncol(idhx)-1))], names(idh))
             idg <- merge(idh, idhx, by=names(idh))
             w_design <- w_final / idg[[ncol(idg)]]
             idhx <- idh <- NULL
      } else w_design <- w_final

  ### Calculation

  Dom1 <- Dom
  if (!is.null(period)) {
       if (!is.null(Dom1)) { Dom1 <- data.table(period, Dom1)
        } else Dom1 <- period } 

  Domans <- NULL
  estim <- c()
  estim2 <- c()
  estim_eu <- c()
  aH <- names(H)
  idper <- id
  if (!is.null(period)) idper <- data.table(idper, period)
  Y1 <- data.table(idper, ID_household, H, PSU, w_final)
  Y1a <- data.table(idper, ID_household, H, PSU, w_design)

  if ("linarpt" %in% type) {
       varpt <- linarpt(inc=inc, id=id, weight=w_final,
                        sort=sort, Dom=Dom, period=period,
                        dataset=NULL, percentage=percentage,
                        order_quant=order_quant, na.rm=na.rm,
                        var_name="lin_arpt")

       varpta <- linarpt(inc=inc, id=id, weight=w_design,
                         sort=sort, Dom=Dom, period=period,
                         dataset=NULL, percentage=percentage,
                         order_quant=order_quant, na.rm=na.rm,
                         var_name="lin_arpt")

       Y1 <- merge(Y1, varpt$lin, by=names(idper), all.x=T)
       Y1a <- merge(Y1a, varpta$lin, by=names(idper), all.x=T)

       vals <- data.frame(varpt$value)
       varpt <- varpta <- NULL
       if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
       colnames(vals)[ncol(vals)] <- "value"
       esti <- data.frame("ARPT", vals, NA)
       colnames(esti)[c(1, ncol(esti))] <- c("type", "value_eu")
       estim <- rbind(estim, esti)
       esti <- NULL
     }
  if ("linarpr" %in% type) {
       varpr <- linarpr(inc=inc, id=id, weight=w_final,
                        income_thres=income_thres, wght_thres=wght_thres,
                        sort=sort, Dom=Dom, period=period,  
                        dataset=NULL, percentage=percentage,
                        order_quant=order_quant, na.rm=na.rm, "lin_arpr")
       varpra <- linarpr(inc=inc, id=id, weight=w_design,
                         income_thres=income_thres, wght_thres=wght_thres,
                         sort=sort, Dom=Dom, period=period, 
                         dataset=NULL, percentage=percentage,
                         order_quant=order_quant, na.rm=na.rm, "lin_arpr")

       Y1 <- merge(Y1, varpr$lin, by=names(idper), all.x=T)
       Y1a <- merge(Y1a, varpra$lin, by=names(idper), all.x=T)

       vals <- data.frame(varpr$value)
       varpr <- varpra <- NULL
       if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
       colnames(vals)[ncol(vals)] <- "value"
       esti <- data.frame("ARPR", vals, NA)  
       colnames(esti)[c(1, ncol(esti))] <- c("type", "value_eu")
       estim <- rbind(estim, esti)
       esti <- NULL
     }
  if (("lingpg" %in% type)&&(!is.null(gender))) {
        vgpg <- lingpg(inc=inc, gender=gender, id=id,
                       weight=w_final, sort=sort,
                       Dom=Dom, period=period, dataset=NULL, 
                       na.rm=na.rm, var_name="lin_gpg")
        vgpga <- lingpg(inc=inc, gender=gender, id=id,
                        weight=w_design, sort=sort,
                        Dom=Dom, period=period, dataset=NULL, 
                        na.rm=na.rm, var_name="lin_gpg")

        Y1 <- merge(Y1, vgpg$lin, by=names(idper), all.x=T)
        Y1a <- merge(Y1a, vgpga$lin, by=names(idper), all.x=T)

        vals <- data.frame(vgpg$value)
        vgpg <- vgpga <- NULL
        if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
        colnames(vals)[ncol(vals)] <- "value"
        esti <- data.frame("GPG", vals, NA)  
        colnames(esti)[c(1, ncol(esti))] <- c("type", "value_eu")
        estim <- rbind(estim,esti)
        esti <- NULL
     }
  if ("linpoormed" %in% type) {
        vporm <- linpoormed(inc=inc, id=id, weight=w_final,
                            sort=sort, Dom=Dom, period=period, 
                            dataset=NULL, percentage=percentage,
                            order_quant=order_quant, na.rm=na.rm,
                            var_name="lin_poormed")
        vporma <- linpoormed(inc=inc, id=id, weight=w_design,
                             sort=sort, Dom=Dom, period=period, 
                             dataset=NULL, percentage=percentage,
                             order_quant=order_quant, na.rm=na.rm,
                             var_name="lin_poormed")
        Y1 <- merge(Y1, vporm$lin, by=names(idper), all.x=T)
        Y1a <- merge(Y1a, vporma$lin, by=names(idper), all.x=T)

        vals <- data.frame(vporm$value)
        vporm <- vporma <- NULL
        if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
        colnames(vals)[ncol(vals)] <- "value"
        esti <- data.frame("linpoormed", vals, NA)  
        colnames(esti)[c(1, ncol(esti))] <- c("type", "value_eu")
        vporm <- vporma <- NULL
        estim <- rbind(estim, esti)
        esti <- NULL
     }
  if ("linrmpg" %in% type) {
        vrmpg <- linrmpg(inc=inc, id=id, weight=w_final,
                         sort=sort, Dom=Dom, period=period,
                         dataset=NULL, percentage=percentage,
                         order_quant=order_quant,
                         na.rm=na.rm, var_name="lin_rmpg")

        vrmpga <- linrmpg(inc=inc, id=id, weight=w_design,
                          sort=sort, Dom=Dom, period=period,
                          dataset=NULL, percentage=percentage,
                          order_quant=order_quant,
                          na.rm=na.rm, var_name="lin_rmpg")

        Y1 <- merge(Y1, vrmpg$lin, by=names(idper), all.x=T)
        Y1a <- merge(Y1a, vrmpga$lin, by=names(idper), all.x=T)

        vals <- data.frame(vrmpg$value)
        vrmpg <- vrmpga <- NULL
        if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[1:ncol(Dom1)]
        colnames(vals)[ncol(vals)] <- "value"
        esti <- data.frame("RMPG", vals, NA)  
        colnames(esti)[c(1, ncol(esti))] <- c("type", "value_eu")
        estim <- rbind(estim, esti)
      }
  if ("linqsr" %in% type) {
       vqsr <- linqsr(inc=inc, id=id, weight=w_final, 
                      sort=sort, Dom=Dom, period=period,
                      dataset=NULL, alpha=alpha, 
                      na.rm=na.rm, var_name="lin_qsr") 
       vqsra <- linqsr(inc=inc, id=id, weight=w_design,
                      sort=sort, Dom=Dom, period=period,
                      dataset=NULL, alpha=alpha, 
                      na.rm=na.rm, var_name="lin_qsr") 

       Y1 <- merge(Y1, vqsr$lin, by=names(idper), all.x=T)
       Y1a <- merge(Y1a, vqsra$lin, by=names(idper), all.x=T)

       vals <- data.frame(vqsr$value)
       vqsr <- vqsra <- NULL
       if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
       colnames(vals)[c(ncol(vals)-1, ncol(vals))] <- c("value", "value_eu")
       esti <- data.frame("QSR", vals)  
       colnames(esti)[1] <- "type"
       estim <- rbind(estim, esti)
       esti <- NULL
    }
  if ("lingini" %in% type) {
       vgini <- lingini(inc=inc, id=id, weight=w_final,
                        sort=sort, Dom=Dom, period=period,
                        dataset=NULL, na.rm=na.rm, var_name="lin_gini")
       vginia <- lingini(inc=inc, id=id, weight=w_design,
                         sort=sort, Dom=Dom, period=period,
                         dataset=NULL, na.rm=na.rm, var_name="lin_gini")

       Y1 <- merge(Y1, vgini$lin, by=names(idper), all.x=T)
       Y1a <- merge(Y1a, vginia$lin, by=names(idper), all.x=T)

       vals <- data.frame(vgini$value)
       vgini <- vginia <- NULL
       if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
       colnames(vals)[c(ncol(vals)-1, ncol(vals))] <- c("value", "value_eu")
       esti <- data.frame("GINI", vals)  
       colnames(esti)[1] <- "type"
       estim <- rbind(estim, esti)
       esti <- NULL
     }
  if ("lingini2" %in% type) {
       vgini2 <- lingini2(inc=inc, id=id, weight=w_final,
                          sort=sort, Dom=Dom, period=period,
                          dataset=NULL, na.rm=na.rm, var_name="lin_gini2")
       vgini2a <- lingini2(inc=inc, id=id, weight=w_design,
                          sort=sort, Dom=Dom, period=period,
                          dataset=NULL, na.rm=na.rm, var_name="lin_gini2")

       Y1 <- merge(Y1, vgini2$lin, by=names(idper), all.x=T)
       Y1a <- merge(Y1a, vgini2a$lin, by=names(idper), all.x=T)

       vals <- data.frame(vgini2$value)
       vgini2 <- vgini2a <- NULL
       if ((is.null(Domans))&(!is.null(Dom1))) Domans <- vals[,1:ncol(Dom1)]
       colnames(vals)[c(ncol(vals)-1, ncol(vals))] <- c("value", "value_eu")
       esti <- data.frame("GINI2", vals)  
       colnames(esti)[1] <- "type"
       estim <- rbind(estim, esti)
       esti <- NULL
     }
  estim2 <- data.table(estim[, ncol(estim)-1])
  estim <- data.table(estim)
  Y1 <- data.table(Y1)
  Y1a <- data.table(Y1a)

  .SD <- lin_outp <- NULL
  if (outp_lin) lin_outp <- Y1[, c(-(3:5)-np), with=F]

  Y2 <- Y1[, lapply(.SD, sum, na.rm = T), by = c(names(Y1)[c(2:(5+np))]), .SDcols = names(Y1)[-(1:(5+np))]]
  Y2a <- Y1a[, lapply(.SD, sum, na.rm = T), by = c(names(Y1a)[c(2:(5+np))]), .SDcols = names(Y1a)[-(1:(5+np))]]
  
  Y3 <- Y2[, c(-(1:(4+np))), with=F]
  Y3a <- Y2a[, c(-(1:(4+np))), with=F]
  
  period <- NULL
  if (np>0) period <- Y2[, c(1:np), with=F]

  IDh <- Y2[, np+1, with=F]
  H <- Y2[, np+2, with=F]
  setnames(H, names(H), aH)

  PSU <- Y2[, np+3, with=F]
  setnames(PSU, names(PSU), aPSU)

  w_final2 <- data.frame(Y2)[, np+4]
  w_design2 <- data.frame(Y2a)[, np+4]
  
  Y1 <- Y1a <- NULL
  Y2 <- Y2a <- NULL

  # Calibration
  res_outp <- NULL
  if (!is.null(X)) {
       if (np>0) IDh <- data.table(period, IDh)
       setnames(IDh, names(IDh), names(X_ID_household))
       X0 <- data.table(X_ID_household, g, q, X)
       D1 <- merge(IDh, X0, by=names(IDh))
       g <- data.frame(D1)[,np+2]
       q <- data.frame(D1)[,np+3]
       X <- D1[,(np+4):ncol(D1),with=F]
            
       Y4 <- residual_est(Y=Y3, X=X, weight=w_design2,
                          q=q, period=period, ind_gr=ind_gr) 

       if (outp_res) res_outp <- data.table(IDh, PSU, w_final2, Y4)
   } else Y4 <- Y3

  var_est <- variance_est(Y=Y4, H=H, PSU=PSU, w_final=w_final2,
                          N_h=N_h, fh_zero=fh_zero, PSU_level=PSU_level,
                          period=period, dataset=NULL)
  np <- sum(ncol(period))
  if (np>0) var_est <- data.table(t(colSums(var_est[,-c(1:np),with=F], na.rm=T)))

  # Variance of HT estimator under current design
  var_cur_HT <- variance_est(Y=Y3a, H=H, PSU=PSU, w_final=w_design2, 
                             N_h=N_h, fh_zero=fh_zero, PSU_level=PSU_level,
                             period=period, dataset=NULL)  
  if (np>0) var_cur_HT <- data.table(t(colSums(var_cur_HT[,-c(1:np),with=F], na.rm=T)))
                        
  var_cur_HT <- data.table(t(var_cur_HT))
  H <- PSU <- N_h <- NULL

  # Variance of HT estimator under SRS
  var_srs_HT <- var_srs(Y=Y3a, w = w_design2)
  var_srs_HT <- data.table(t(var_srs_HT))
  
  # Variance of calibrated estimator under SRS
  var_srs_ca <- var_srs(Y=Y4, w = w_final2)
  var_srs_ca <- data.table(t(var_srs_ca))
  Y3a <- Y4 <- H <- PSU <- N_h <- NULL
  var_est2 <- data.table(t(var_est))
  setnames(var_est2, names(var_est2), "var")

  test_v <- (var_est2<0)
  test_v[is.na(test_v)] <- FALSE
  if (any(test_v)) stop("Estimation of variance are negative!")

  # Effect of sample design
  deff_sam <- var_cur_HT / var_srs_HT
  
  # Effect of estimator
  deff_est <- var_est2 / var_cur_HT
  
  # Overall effect of sample design and estimator
  deff <- deff_sam * deff_est

  var_est3 <- var_est2
  var_est3[xor(is.na(var_est3), test_v)] <- 0
  se <- sqrt(var_est3)
  se[xor(is.na(var_est3), test_v)] <- NA
  rse <- se/estim2
  rse[estim2==0] <- NA
  cv <- rse*100
  tsad <- qnorm(0.5*(1+confidence))
  Absolute_margin_of_error <- tsad*se 
  Relative_margin_of_error <- tsad*cv
  CI_lower <- estim2 - tsad*se
  CI_upper <- estim2 + tsad*se

 
  cv <- rse*100
  tsad <- qnorm(0.5*(1+confidence))
  absolute_margin_of_error <- tsad*se 
  relative_margin_of_error <- tsad*cv
  CI_lower <- estim2 - tsad*se
  CI_upper <- estim2 + tsad*se
   
  setnames(se, names(se), "se")
  setnames(rse, names(rse), "rse")
  setnames(cv, names(cv), "cv")
  setnames(absolute_margin_of_error, names(absolute_margin_of_error), "absolute_margin_of_error")
  setnames(relative_margin_of_error, names(relative_margin_of_error), "relative_margin_of_error")
  setnames(CI_lower, names(CI_lower), "CI_lower")
  setnames(CI_upper, names(CI_upper), "CI_upper")
  setnames(var_srs_HT, names(var_srs_HT), "var_srs_HT")
  setnames(var_srs_ca, names(var_srs_ca), "var_srs_ca")
  setnames(var_cur_HT, names(var_cur_HT), "var_cur_HT")
  setnames(deff_sam, names(deff_sam), "deff_sam")
  setnames(deff_est, names(deff_est), "deff_est")
  setnames(deff, names(deff), "deff")

  all_result <- cbind(estim, var_est2, se, rse, cv,
                      absolute_margin_of_error,
                      relative_margin_of_error,  
                      CI_lower, CI_upper, var_srs_HT,
                      var_cur_HT, var_srs_ca,
                      deff_sam, deff_est, deff)

  list(estim = estim,
       var = var_est2,
       se = se,
       rse = rse,
       cv = cv,
       absolute_margin_of_error = absolute_margin_of_error,
       relative_margin_of_error = relative_margin_of_error,
       CI_lower = CI_lower,
       CI_upper = CI_upper,
       var_srs_HT = var_srs_HT,
       var_cur_HT = var_cur_HT,
       var_srs_ca = var_srs_ca,
       deff_sam = deff_sam,
       deff_est = deff_est,
       deff = deff,
       lin_out = lin_outp,
       res_out = res_outp,
       all_result = all_result)
}

