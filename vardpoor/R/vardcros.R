vardcros <- function(Y, H, PSU, w_final, id,
                     Dom = NULL,
                     Z = NULL, 
                     country, period,
                     dataset = NULL,
                     linratio = FALSE,
                     household_level_max = TRUE,
                     withperiod = TRUE,
                     netchanges = TRUE,
                     confidence = .95) {
 
  ### Checking
  if (!is.logical(linratio)) stop("'linratio' must be logical")
  if (!is.logical(household_level_max)) stop("'household_level_max' must be logical")
  if (!is.logical(withperiod)) stop("'withperiod' must be logical")
  if (!is.logical(netchanges)) stop("'netchanges' must be logical")
  if (is.null(Z)==linratio & linratio==TRUE) stop("'linratio' must be FALSE")

  if(!is.numeric(confidence) || length(confidence) != 1 || confidence[1] < 0 || confidence[1] > 1) {
          stop("'confidence' must be a numeric value in [0,1]")  }

  if(!is.null(dataset)) {
      dataset <- data.frame(dataset)
      aY <- Y
      if (min(Y %in% names(dataset))!=1) stop("'Y' does not exist in 'dataset'!")
      if (min(Y %in% names(dataset))==1) {
                                Y <- data.frame(dataset[, Y], check.names=FALSE)
                                names(Y) <- aY }
      if(!is.null(H)) {
          aH <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- as.data.frame(dataset[, aH], stringsAsFactors=FALSE)
                                names(H) <- aH }}
      if(!is.null(id)) {
          aid <- id  
          if (min(id %in% names(dataset))!=1) stop("'id' does not exist in 'dataset'!")
          if (min(id %in% names(dataset))==1) {
                                id <- as.data.frame(dataset[, aid], stringsAsFactors=FALSE)
                                names(id) <- aid }}
     if(!is.null(PSU)) {
          aPSU <- PSU  
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) {
                                PSU <- as.data.frame(dataset[, aPSU], stringsAsFactors=FALSE)
                                names(PSU) <- aPSU }}
      if(!is.null(w_final)) {
          aw_final <- w_final  
          if (min(w_final %in% names(dataset))!=1) stop("'w_final' does not exist in 'dataset'!")
          if (min(w_final %in% names(dataset))==1) {
                                w_final <- data.frame(dataset[, aw_final])
                                names(w_final) <- aw_final }}
      if(!is.null(Z)) {
          aZ <- Z
          if (min(Z %in% names(dataset))!=1) stop("'Z' does not exist in 'dataset'!")
          if (min(Z %in% names(dataset))==1) {
                                Z <- data.frame(dataset[, aZ], check.names=FALSE, stringsAsFactors=FALSE)
                                names(Z) <- aZ }}
      if(!is.null(country)) {
          country2 <- country
          if (min(country %in% names(dataset))!=1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset))==1) country <- as.data.frame(dataset[, country], stringsAsFactors=FALSE)
          names(country) <- country2  }

      if(!is.null(period)) {
          period2 <- period
          if (min(period %in% names(dataset))!=1) stop("'period' does not exist in 'dataset'!")
          if (min(period %in% names(dataset))==1) period <- data.frame(dataset[, period])
          names(period) <- period2  }
     
      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom2], stringsAsFactors=FALSE) 
                  names(Dom) <- Dom2 }    }
      }

  # Y
  Y <- data.table(Y, check.names=TRUE)
  n <- nrow(Y)
  m <- ncol(Y)
  if (!all(sapply(Y, is.numeric))) stop("'Y' must be numeric values")
  if (any(is.na(Y))) stop("'Y' has unknown values")
  if (is.null(names(Y))) stop("'Y' must be colnames")
  
  # H
  H <- data.table(H)
  if (nrow(H) != n) stop("'H' length must be equal with 'Y' row count")
  if (ncol(H) != 1) stop("'H' must be 1 column data.frame, matrix, data.table")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(names(H))) stop("'H' must be colnames")
  
  # id
  id <- data.table(id)
  if (any(is.na(id))) stop("'id' has unknown values")
  if (nrow(id) != n) stop("'id' length must be equal with 'Y' row count")
  if (ncol(id) != 1) stop("'id' must be 1 column data.frame, matrix, data.table")
  if (is.null(names(id))||(names(id)=="id")) setnames(id, names(id), "h_ID")

  # PSU
  PSU <- data.table(PSU)
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  if (nrow(PSU) != n) stop("'PSU' length must be equal with 'Y' row count")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  
  # w_final 
  w_final <- data.frame(w_final)
  if (nrow(w_final) != n) stop("'w_final' must be equal with 'Y' row count")
  if (ncol(w_final) != 1) stop("'w_final' must be vector or 1 column data.frame, matrix, data.table")
  w_final <- w_final[,1]
  if (!is.numeric(w_final)) stop("'w_final' must be numerical")
  if (any(is.na(w_final))) stop("'w_final' has unknown values") 
  
  # country
  country <- data.table(country)
  if (any(is.na(country))) stop("'country' has unknown values")
  if (nrow(country) != n) stop("'country' length must be equal with 'Y' row count")
  if (ncol(country) != 1) stop("'country' has more than 1 column")

  # period
  if (withperiod) {
      period <- data.table(period)
      if (any(is.na(period))) stop("'period' has unknown values")
      if (nrow(period) != n) stop("'period' length must be equal with 'Y' row count")
    } else if (!is.null(period)) stop("'period' must be NULL for those data")

  # Dom
  if (!is.null(Dom)) {
    Dom <- data.table(Dom)
    if (any(duplicated(names(Dom)))) 
           stop("'Dom' are duplicate column names: ", 
                 paste(names(Dom)[duplicated(names(Dom))], collapse = ","))
    if (nrow(Dom) != n) stop("'Dom' and 'Y' must be equal row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(names(Dom))) stop("'Dom' must be colnames")
  }
  
  # Z
  if (!is.null(Z)) {
    Z <- data.table(Z, check.names=TRUE)
    if (!all(sapply(Z, is.numeric))) stop("'Z' must be numeric values")
    if (nrow(Z) != n) stop("'Z' and 'Y' must be equal row count")
    if (ncol(Z) != m) stop("'Z' and 'Y' must be equal column count")
    if (any(is.na(Z))) stop("'Z' has unknown values")
    if (is.null(names(Z))) stop("'Z' must be colnames")
  }
      
  
 # Calculation
      
 # Domains
 n_h <- stratasf <- name1 <- name2 <- nhcor <- n_h <- num1 <- num <- NULL
 den1 <- den <- num_den1 <- dom <- name2 <- variable <- name1 <- NULL
 namess <- num1 <- grad1 <- grad2 <- den1 <- num_den1 <- estim <- NULL
 total <- pop_size <- N <- stderr_nw <- sample_size <- sd_nw <- NULL
 stderr_w <- sd_w <- se <- rse <- cv <- absolute_margin_of_error <- NULL
 relative_margin_of_error <- CI_lower <- CI_upper <- nams <- NULL
 totalZ <- Z1 <- namesY <- namesZ <- namesZ1 <- NULL

 if (!is.null(Dom)) Y1 <- domain(Y, Dom) else Y1 <- Y

 size <- data.table(size=rep(1, nrow(Y1)))
 if (!is.null(Dom)) size1 <- domain(size, Dom) else size1 <- copy(size)
 
 namesDom <- names(Dom)
 DTp <- copy(country)
 if (withperiod) DTp <- data.table(period, country)
 namesperc <- names(DTp)
 namesperc2 <- c("period_country", names(DTp))
 period_country <- do.call("paste", c(as.list(DTp), sep="_"))
 if (!is.null(Y1)) namesY1 <- names(Y1) else namesY1 <- NULL
 namesYZ <- namesY1
 nams <- data.table(name1=namesY1)
      
 if (!is.null(Z)) {
      if (!is.null(Dom)) Z1 <- domain(Z, Dom) else Z1 <- Z               
      if (!is.null(Z1)) namesZ1 <- names(Z1) else namesZ1 <- NULL
      namesYZ <- c(namesYZ, namesZ1)
      nams <- data.table(nams, nameZ=namesZ1)
      if (linratio){ 
                  sorts <- unlist(split(Y1[, .I], period_country))
                  lin1 <- lapply(split(Y1[, .I], period_country), 
                                 function(i) lin.ratio(Y1[i], Z1[i], w_final[i], Dom=NULL))
                  Y2 <- rbindlist(lin1)[sorts]
                  if (any(is.na(Y2))) print("Results are calculated, but there are cases where Z = 0")
                 } else Y2 <- data.table(copy(Y1), copy(Z1))
   } else Y2 <- copy(Y1)
 
 Y2 <- data.table(Y2, size1, check.names=T)
 names_size1 <- names(Y2)[-ncol(size1):-1+1+ncol(Y2)]

 Y2w <- Y2 * w_final
 namesY2 <- names(Y2)

 namesY2w <- paste0(namesY2, "w")
 setnames(Y2w, namesY2, namesY2w)

 DT <- data.table(period_country, DTp, H, PSU, id, w_final, Y2, Y2w)

 DTp <- data.table(period_country, w_final*Y1)
 if (!is.null(Z)) DTp <- data.table(DTp, w_final*Z1)

 DTp <- DTp[,lapply(.SD, sum, na.rm=T), keyby="period_country", .SDcols=namesYZ]
 setnames(DTp, namesYZ, paste0(namesYZ, "_sum"))

 DT1 <- copy(DT)
 names_id <- names(id)
 names_H <- names(H)
 names_PSU <- names(PSU)
 names_country <- names(country)
 period_country <- ech <- ech1 <-  NULL
 id <- Dom <- Z1 <- country <-  NULL
 H <- PSU <- Y1 <- Y2 <- Y2W <- NULL 

 DT2 <- DT1[, lapply(.SD, sum, na.rm=TRUE),
                      keyby=namesperc2,
                     .SDcols = c(names_size1, namesY2w)]
 setnames(DT2, names(DT2)[-c(1:length(namesperc2))],
               paste0(names(DT2)[-c(1:length(namesperc2))], "s"))


 #--------------------------------------------------------*
 # AGGREGATION AT PSU LEVEL ("ULTIMATE CLUSTER" APPROACH) |
 #--------------------------------------------------------*

 DT3 <- DT1[, lapply(.SD, sum, na.rm=TRUE), keyby=c(namesperc2,
              names_H, names_PSU), .SDcols = namesY2w]
 setnames(DT3, namesY2w, namesY2)
 DT1 <- copy(DT3)
 DT1[, (c("period_country", paste0(names_size1, ""))):=NULL]
 if (!netchanges) DT1 <- NULL

 setkeyv(DT2, namesperc2)
 DT3 <- merge(DT2, DT3, all=T)
 
 # VECTOR OF THE PARTIAL DERIVATIVES (GRADIENT FUNCTION)

 if (!is.null(namesZ1) & !linratio) {
     Z1ws <- paste0(namesZ1, "ws")
     Y1ws <- paste0(namesY1, "ws") 

     DT4 <- DT3[, lapply(paste0(namesZ1, "ws"), function(z) 1/get(z))]
     setnames(DT4, names(DT4), paste0("g1__", namesY1, "___", namesZ1))

     DT3 <- cbind(DT3, DT4)

     DT4 <- data.table(DT3[, mapply(function(y, z) -get(y)/(get(z))^2, Y1ws, Z1ws)])
     setnames(DT4, names(DT4), paste0("g2__", namesY1, "___", namesZ1))
     DT3 <- cbind(DT3, DT4)
 }

 # NUMBER OF PSUs PER STRATUM
 setkeyv(DT3, c(namesperc2, names_H))
 DT3[, n_h:=.N, keyby=c(namesperc2, names_H)]
 
 setkeyv(DT3, c(namesperc, names_H, names_PSU))

 #--------------------------------------------------------------------------*
 # MULTIVARIATE REGRESSION APPROACH USING STRATUM DUMMIES AS REGRESSORS AND |
 # STANDARD ERROR ESTIMATION 								    |
 #--------------------------------------------------------------------------*

 DT3H <- DT3[[names_H]]
 DT3H <- factor(DT3H)
 if (length(levels(DT3H))==1) { DT3[, stratasf:= 1]
                                DT3H <- "stratasf"
                      }  else { DT3H <- data.table(model.matrix( ~ DT3H-1))
                                DT3 <- cbind(DT3, DT3H)
                                DT3H <- names(DT3H) }
 

 fit <-lapply(1:length(namesY1), function(i) {
           fits <- lapply(split(DT3, DT3$period_country), function(d) {
                  	y <- namesY1[i]
                        if ((!is.null(namesZ1))&(!linratio)) z <- paste0(",", toString(namesZ1[i])) else z <- ""
	                  funkc <- as.formula(paste("cbind(", trim(toString(y)), z, ")~",
                                       paste(c(-1, DT3H), collapse= "+")))
                   	res1 <- data.table(lm(funkc, data=d)$res)
                        ncolr <- ncol(res1)
                        setnames(res1, names(res1)[1], "num") 
                        res1[, namesY1:=y]
                        res1[ncolr>1, namesZ1:=names(res1)[2]]

                        if (!is.null(namesZ1) & !linratio) setnames(res1, names(res1)[2], "den")
                        res1 <- data.table(res1, d)
                        res1[, nhcor:=ifelse(n_h==1, 1, n_h/(n_h-1))]
                        res1[, num1:=nhcor*num*num]
                        res1[ncolr>1, den1:=nhcor*den*den]
                        res1[ncolr>1, num_den1:=nhcor*num*den]
                        namep <- c("namesY1", "namesZ1")
                        namep <- namep[namep %in% names(res1)]
                        varsp <- c("num1", "den1", "num_den1")
                        varsp <- varsp[varsp %in% names(res1)]
                        fits <- res1[, lapply(.SD, sum), 
                                       keyby=c("period_country",
                                               namesperc, namep),
                                       .SDcols=varsp]
                    })
            data.table(do.call("rbind", fits))      
        })

  res <- data.table(do.call("rbind", fit))
  a0 <- unlist(lapply(res$namesY1, function(d) regexpr("__", d)[1]))+2
  res[!is.null(namesDom), Dom:=substr(res$namesY1, a0, nchar(res$namesY1))]
  a0 <- fit <- DT3H <- NULL

  sd1 <- paste0(c(names_size1, namesY2w), "s")
  if ((!is.null(namesZ1))&(!linratio)) {
                 sd1 <- c(sd1, paste0("g1__", namesY1, "___", namesZ1),
                               paste0("g2__", namesY1, "___", namesZ1))}

  DT2 <- DT3[, lapply(.SD, mean, na.rm=TRUE),
                       keyby="period_country", 
                      .SDcols = sd1]

  main <- melt(DT2[, c(paste0(names_size1, "s"), "period_country"), with=F],
                       id="period_country")
  main[, variable:=trim(as.character(variable))]
  main[nchar(variable)>6, Dom:=substr(variable, 7, nchar(variable)-1)]
  if (is.null(namesDom)) npcs <- "period_country" else npcs <- c("period_country", "Dom")
  main[, variable:=NULL]
  setnames(main, "value", "sample_size")
  setkeyv(main, npcs)
  setkeyv(res, npcs)
  res <- merge(res, main, all=T) 

  main <- melt(DT2[, c(paste0(names_size1, "ws"), "period_country"), with=F], id="period_country")
  main[, variable:=trim(as.character(variable))]
  main[nchar(variable)>6, Dom:=substr(variable, 7, nchar(variable)-2)] 
  main[, variable:=NULL]
  setnames(main, "value", "pop_size")
  setkeyv(main, npcs)
  res <- merge(res, main, all=T)

  total <- totalY <- totalZ <- NULL
  main <- melt(DT2[, c(paste0(namesY1, "ws"), "period_country"), with=F], id="period_country")
  main[, variable:=trim(as.character(variable))]
  main[, namesY1:=substr(variable, 1, nchar(variable)-2)] 
  main[, variable:=NULL]
  setnames(main, "value", "total")
  setkeyv(main, c("period_country", "namesY1"))
  setkeyv(res, c("period_country", "namesY1"))
  res <- merge(res, main, all=T)

  main <- melt(DTp[, c(paste0(namesY1, "_sum"), "period_country"), with=F], id="period_country")
  main[, variable:=trim(as.character(variable))]
  main[, namesY1:=substr(variable, 1, nchar(trim(as.character(variable)))-4)] 
  main[, variable:=NULL]
  setnames(main, "value", "totalY")
  setkeyv(main, c("period_country", "namesY1"))
  setkeyv(res, c("period_country", "namesY1"))
  res <- merge(res,  main, all=T)

  if (!is.null(Z1)) { main <- melt(DTp[, c(paste0(namesZ1, "_sum"), "period_country"), with=F], id="period_country")
                      main[, variable:=trim(as.character(variable))]
                      main[, namesZ1:=substr(variable, 1, nchar(variable)-4)] 
                      main[, variable:=NULL]
                      setnames(main, "value", "totalZ")
                      setkeyv(main, c("period_country", "namesZ1"))
                      setkeyv(res, c("period_country", "namesZ1"))
                      res <- merge(res, main, all=T)
 }
	
 res[, namess:=namesY1]
 res[!is.null(namesZ1), namess:=paste0(namesY1, "___", namesZ1)]

 if (!is.null(namesZ1) & !linratio) {
             main <- melt(DT2[,c(paste0("g1__", namesY1,"___", namesZ1), "period_country"), with=F], id="period_country")
             main[, variable:=trim(as.character(variable))]
             main[, namess:=substr(variable, 5, nchar(variable))] 
             main[, variable:=NULL]
             setnames(main, "value", "grad1")
	       setkeyv(main, c("period_country", "namess"))
       	 setkeyv(res, c("period_country", "namess"))
	 	 res <- merge(res, main, all=T)	 

		 main <- melt(DT2[,c(paste0("g2__", namesY1,"___", namesZ1), "period_country"), with=F], id="period_country")
             main[, variable:=trim(as.character(variable))]
	 	 main[, namess:=substr(variable, 5, nchar(variable))] 
	 	 main[, variable:=NULL]
	 	 setnames(main, "value", "grad2")
	 	 setkeyv(main, c("period_country", "namess"))
	 	 res <- merge(res, main, all=T)
 }
 rm(DT2, DT3, DTp)
 res[, var:=num1]
 res[!is.null(namesZ1) & !linratio, var:=(grad1*grad1*num1)+
                                          (grad2*grad2*den1)+
                                        2*(grad1*grad2*num_den1)]
 res[, estim:=totalY]
 res[!is.null(totalZ), estim:=totalY/totalZ]

 res[!is.null(namesZ1), namesZ:=namesZ1]
 res[, namesY:=paste0(namesY1, "__")] 
 res[, namesY:=substr(namesY, 1, regexpr("__", namesY)-1)]

 res[!is.null(namesZ1), namesZ:=paste0(namesZ1, "__")]
 res[!is.null(namesZ1), namesZ:=substr(namesZ, 1, regexpr("__", namesZ)-1)]

  
 dom <- NULL
 main <- namesperc
 if (!is.null(namesDom)) {
       nosr <- data.table(Dom=res$Dom, t(data.frame(strsplit(res$Dom, "__"))))
       nosr <- nosr[!duplicated(nosr)]
       nosr <- nosr[, lapply(nosr, as.character)]     
       setnames(nosr, names(nosr)[2:ncol(nosr)], namesDom)
       setkeyv(nosr, "Dom")
       setkeyv(res, "Dom")
       res <- merge(nosr, res)
       main <- c(main, "Dom", namesDom)
       nosr <- NULL }

 main <- c(main, "namesY", "namesY1")
 if (!is.null(res$namesZ)) main <- c(main, "namesZ", "namesZ1") 
 main2 <- c(main, "estim", "num1")
 if (!is.null(namesZ1) & !linratio) main2 <- c(main2, "den1", "grad1", "grad2")

 if (netchanges) { res1 <- res[, main2[!(main2 %in% c("Dom", "namesY1", "namesZ1"))], with=F]                  
                 } else res1 <- NULL

 main <- c(main, "sample_size", "pop_size", "estim", "var")
 res <- res[, .N, keyby=main]

 #-------------------------------------------------------------------------*
 # DESIGN EFFECT (DEFF) ESTIMATION - VARIANCE UNDER SIMPLE RANDOM SAMPLING |
 #-------------------------------------------------------------------------*

 # We aggregate the target variables at household level
 
 DTs <- DT[, lapply(.SD, sum, na.rm=TRUE), keyby=c(namesperc2, names_id, "w_final"), .SDcols = namesY2]
 if (household_level_max) {    
          DTm <- DT[, lapply(.SD, max, na.rm=TRUE), keyby=c(namesperc2, names_id), .SDcols = names_size1]
      } else {
         DTm <- DT[, lapply(.SD, sum, na.rm=TRUE), keyby=c(namesperc2, names_id), .SDcols = names_size1]
    }

 setnames(DTm, names_size1, paste0(names_size1, "m"))
 DTs <- merge(DTs, DTm)

 # Linearised variables

 if ((!is.null(namesZ1))&(!linratio)) {
                  lin1 <- lapply(split(DTs[, .I], DTs$period_country), function(i) 
                               lin.ratio(Y=DTs[i, namesY1, with=F],
                                         Z=DTs[i, namesZ1, with=F],
                                         weight=DTs[["w_final"]][i], Dom=NULL))
                  Y2a <- rbindlist(lin1)
                  setnames(Y2a, names(Y2a), paste0("lin___", namesY1))
                  DTs <- data.table(DTs, Y2a)
                  Y2a <- paste0("lin___", namesY1)
              } else Y2a <- namesY1

 w_final <- DTs[["w_final"]]
 DTsd <- DTs[, lapply(.SD[, Y2a, with = F], function(x) 
                      sum(w_final*((x-sum(w_final*x)/sum(w_final))^2))/(sum(w_final)-1)),
                      keyby="period_country"]

 setnames(DTsd, Y2a, paste0("sd_w__", namesY1))
 setkeyv(DTs, "period_country")
 DTs <- merge(DTs, DTsd)

 DTm <- DTs[, lapply(.SD[, paste0(names_size1, "m"), with = F], function(x) sum(w_final*x, na.rm=TRUE)),
                      keyby="period_country"]
 setnames(DTm, paste0(names_size1, "m"), paste0("pop_", names_size1))
 DTs <- merge(DTs, DTm)

 DTsd <- DTs[, lapply(.SD, sd, na.rm=TRUE), keyby="period_country", .SDcols = Y2a]
 setnames(DTsd, Y2a, paste0("sd_nw__", namesY1))
 DTs <- merge(DTs, DTsd)

 DTm <- DTs[, lapply(.SD, sum, na.rm=TRUE), keyby="period_country", .SDcols = names_size1]
 setnames(DTm, names_size1, paste0("samp_", names_size1))
 DTs <- merge(DTs, DTm)
    
 DTx <- DTs[, .N, keyby=c(namesperc, paste0("sd_w__", namesY1),
                              paste0("sd_nw__", namesY1),
                              paste0("pop_", names_size1),
                              paste0("samp_", names_size1))]
 DTx[, N:=NULL]

 main <- melt(DTx[, c(namesperc, paste0("sd_w__", namesY1)), with=F],  id=namesperc)
 main[, namesY1:=substr(variable, 7, nchar(trim(as.character(variable))))] 
 main[, variable:=NULL]
 setnames(main, "value", "sd_w")
 setkeyv(main, c(namesperc, "namesY1"))
 setkeyv(res, c(namesperc, "namesY1"))
 res <- merge(res, main, all=T)

 main <- melt(DTx[, c(namesperc, paste0("sd_nw__", namesY1)), with=F],  id=namesperc)
 main[, namesY1:=substr(variable, 8, nchar(trim(as.character(variable))))] 
 main[, variable:=NULL]
 setnames(main, "value", "sd_nw")
 setkeyv(main, c(namesperc, "namesY1"))
 setkeyv(res, c(namesperc, "namesY1"))
 res <- merge(res, main, all=T)

 main <- melt(DTx[, c(namesperc, paste0("pop_", names_size1)), with=F],  id=namesperc)
 main[!is.null(namesDom), Dom:=substr(variable, 11, nchar(trim(as.character(variable))))] 
 main[, variable:=NULL]
 setnames(main, "value", "pop")
 if (is.null(namesDom)) nds <- namesperc else nds <- c(namesperc, "Dom")
 setkeyv(main, nds)
 setkeyv(res, nds)
 res <- merge(res, main, all=T)

 main <- melt(DTx[, c(namesperc, paste0("samp_", names_size1)), with=F],  id=namesperc)
 main[!is.null(namesDom), Dom:=substr(variable, 12, nchar(trim(as.character(variable))))] 
 main[, variable:=NULL]
 setnames(main, "value", "sampl_siz")
 setkeyv(main, nds)
 setkeyv(res, nds)
 res <- merge(res, main, all=T)

 res[, stderr_nw:=100*sqrt((1-(sample_size/pop_size))/pop_size * sd_nw * sd_nw/sample_size )]
 res[, stderr_w:=100*sqrt((1-(sample_size/pop_size))/pop_size * sd_w * sd_w/sample_size )]

 DT <- DTw <- DTx <- DTs <- DTsd <- sd1 <- nds <-NULL


 res[, namesY1:=NULL]
 res[!is.null(namesZ1), namesZ1:=NULL]
 res[, N:=NULL]
 res[, se:=sqrt(var)]
 res[, rse:=se/estim]
 res[, cv:=rse*100]
 tsad <- qnorm(0.5*(1+confidence))
 res[, absolute_margin_of_error:=tsad*se]
 res[, relative_margin_of_error:=tsad*cv]
 res[, CI_lower:=estim - tsad*se]
 res[, CI_upper:=estim + tsad*se]

 main <- namesperc 
 if (!is.null(namesDom))  main <- c(main, namesDom)
 main <- c(main, "namesY")
 if (!is.null(res$namesZ)) main <- c(main, "namesZ")
 main <- c(main, "sample_size", "pop_size", "estim", "se", 
           "var", "rse", "cv", "absolute_margin_of_error",
           "relative_margin_of_error", "CI_lower", "CI_upper", 
           "sd_w", "sd_nw", "pop", "sampl_siz", "stderr_nw",
           "stderr_w")
 res <- res[, main, with=F]
 list(data_net_changes=DT1, var_grad=res1, results=res)
}   



