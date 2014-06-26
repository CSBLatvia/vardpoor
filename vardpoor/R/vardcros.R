
vardcros <- function(Y, H, PSU, w_final, id,
                     Dom = NULL,
                     Z = NULL,
                     country, period,
                     dataset = NULL,
                     meanY=TRUE,
                     withoutperiod=TRUE,
                     netchanges=TRUE,
                     confidence = .95) {
 
  ### Checking
  if (!is.logical(meanY)) stop("'meanY' must be the logical value")
  if (!is.logical(withoutperiod)) stop("'withoutperiod' must be the logical value")
  if (!is.logical(netchanges)) stop("'netchanges' must be the logical value")

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
                                Z <-data.frame(dataset[, aZ],check.names=FALSE, stringsAsFactors=FALSE)
                                names(Z) <- aZ }}
      if(!is.null(country)) {
          country2 <- country
          if (min(country %in% names(dataset))!=1) stop("'country' does not exist in 'dataset'!")
          if (min(country %in% names(dataset))==1) country <- data.frame(dataset[, country])
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
  if (!withoutperiod) {
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
    Z <- data.table(Z)
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
 relative_margin_of_error <- CI_lower <- CI_upper <- NULL

 if (!is.null(Dom)) Y1 <- domain(Y, Dom) else Y1 <- Y

 if (!is.null(Z)) {
    if (!is.null(Dom)) Z1 <- domain(Z, Dom) else Z1 <- Z
    Y2 <- lin.ratio(Y1, Z1, w_final, Dom=NULL)
  } else {
          Y2 <- Y1
         }

 ech <- data.table(ech=rep(1, nrow(Y1)))
 if (!is.null(Dom)) ech1 <- domain(ech, Dom) else ech1 <- ech
 ech <- NULL
 
 Y2 <- data.table(Y2, ech1)

 Ypw <- Y2 * w_final
 namesYp <- names(Y2)
 namesYpw <- paste0(names(Y2), "w")
 setnames(Ypw, names(Ypw), namesYpw)

 if (!withoutperiod) {
                  namesperc <- c(names(period), names(country))
                  DT <- data.table(period, country, H, PSU, id, w_final, Y2, Ypw)
               } else{
                  namesperc <- names(country)
                  DT <- data.table(country, H, PSU, id, w_final, Y2, Ypw)
                 }
 DT1 <- copy(DT)
 DT2 <- DT1[, lapply(.SD, sum, na.rm=TRUE),
                      keyby=namesperc,
                     .SDcols = c(names(ech1), namesYpw)]
 setnames(DT2, names(DT2)[-c(1:length(namesperc))],
               paste0(names(DT2)[-c(1:length(namesperc))], "s"))


 #--------------------------------------------------------*
 # AGGREGATION AT PSU LEVEL ("ULTIMATE CLUSTER" APPROACH) |
 #--------------------------------------------------------*

 DT3 <- DT1[, lapply(.SD, sum, na.rm=TRUE), keyby=c(namesperc,
              names(H), names(PSU)),.SDcols = namesYpw]
 setnames(DT3, namesYpw, namesYp)
 if (netchanges)  DT1 <- DT3 else DT1 <- NULL

 setkeyv(DT2, namesperc)
 DT3 <- merge(DT2, DT3, all=T)
 
 # VECTOR OF THE PARTIAL DERIVATIVES (GRADIENT FUNCTION)

 ech1names <- rep(names(ech1), ncol(Y))

 if (meanY) {
     ech1ws <- paste0(names(ech1), "ws") 
     Y1ws <- paste0(names(Y1), "ws") 

     DT4 <- DT3[, lapply(paste0(ech1names, "ws"), function(ech) 1/get(ech))]
     setnames(DT4, names(DT4), paste0("g1__", names(Y1), "___", ech1names))

     DT3 <- cbind(DT3, DT4)

     DT4 <- data.table(DT3[, mapply(function(y, ech) -get(y)/(get(ech))^2, Y1ws, rep(ech1ws, ncol(Y)))])
     setnames(DT4, names(DT4), paste0("g2__", names(Y1), "___", ech1names))
     DT3 <- cbind(DT3, DT4)
 }

 # NUMBER OF PSUs PER STRATUM
 setkeyv(DT3, c(namesperc, names(H)))
 DT3[, n_h:=.N, keyby=c(namesperc, names(H))]
 DT3$period_country <- do.call("paste", c(as.list(DT3[, namesperc, with=F]), sep="_"))
  
 setkeyv(DT3, c(namesperc, names(H), names(PSU)))

 #--------------------------------------------------------------------------*
 # MULTIVARIATE REGRESSION APPROACH USING STRATUM DUMMIES AS REGRESSORS AND |
 # STANDARD ERROR ESTIMATION 								    |
 #--------------------------------------------------------------------------*

 DT3H <- DT3[[names(H)]]
 DT3H <- factor(DT3H)
 if (length(levels(DT3H))==1) { DT3[, stratasf:= 1]
                                DT3H <- "stratasf"
                      }  else { DT3H <- data.table(model.matrix( ~ DT3H-1))
                                DT3 <- cbind(DT3, DT3H)
                                DT3H <- names(DT3H) }
 
 fit <-lapply(1:ncol(Y1), function(i) {
           fits <- lapply(split(DT3, DT3$period_country), function(d) {
                  	y <- names(Y1)[i]
                        if (meanY) z <- paste0(",", toString(ech1names[i])) else z <- ""
	                  funkc <- as.formula(paste("cbind(", trim(toString(y)), z, ")~",
                                       paste(c(-1, DT3H), collapse= "+")))
                   	res1 <- data.table(lm(funkc, data=d)$res)
                        ncolr <- ncol(res1)
                        setnames(res1, names(res1)[1], "num") 
                        res1[, name1:=y]
                        res1[ncolr>1, name2:=names(res1)[2]]

                        if (meanY) setnames(res1, names(res1)[2], "den")
                        res1 <- data.table(res1, d)
                        res1[, nhcor:=ifelse(n_h==1, 1, n_h/(n_h-1))]
                        res1[, num1:=nhcor*num*num]
                        res1[ncolr>1, den1:=nhcor*den*den]
                        res1[ncolr>1, num_den1:=nhcor*num*den]
                        namep <- c("name1", "name2")
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
  a0 <- unlist(lapply(res$name1, function(d) regexpr("__", d)[1]))+2
  res[!is.null(Dom), dom:=substr(res$name1, a0, nchar(res$name1))]
  a0 <- fit <- DT3H <- NULL

  if (!meanY) { res$name2 <- "ech" 
                if (!is.null(Dom)) res$name2 <- paste0(res$name2, "__", res$dom)
              }  
  sd1 <- paste0(c(names(ech1), names(Ypw)), "s")
  if (meanY) { sd1 <- c(sd1, paste0("g1__", names(Y1), "___", ech1names),
                               paste0("g2__", names(Y1), "___", ech1names))}

  DT2 <- DT3[, lapply(.SD, mean, na.rm=TRUE),
                       keyby="period_country", 
                      .SDcols = sd1]

  echs <- melt(DT2[, c(paste0(names(ech1), "s"), "period_country"), with=F],
                       id="period_country")
  echs[, name2:=substr(variable, 1, nchar(trim(as.character(variable)))-1)] 
  echs[, variable:=NULL]
  setnames(echs, "value", "sample_size")
  setkeyv(echs, c("period_country", "name2"))
  setkeyv(res, c("period_country", "name2"))
  res <- merge(res, echs, all=T) 

  echs <- melt(DT2[, c(paste0(names(ech1),"ws"), "period_country"), with=F], id="period_country")
  echs[, name2:=substr(variable, 1, nchar(trim(as.character(variable)))-2)] 
  echs[, variable:=NULL]
  setnames(echs, "value", "pop_size")
  setkeyv(echs, c("period_country", "name2"))
  res <- merge(res, echs, all=T)

  echs <- melt(DT2[, c(paste0(names(Y1), "ws"), "period_country"), with=F], id="period_country")
  echs[, name1:=substr(variable, 1, nchar(trim(as.character(variable)))-2)] 
  echs[, variable:=NULL]
  setnames(echs, "value", "total")
  setkeyv(echs, c("period_country", "name1"))
  setkeyv(res, c("period_country", "name1"))
  res <- merge(res, echs, all=T)	
  res[, namess:=paste0(name1, "___", name2)]

  if (!meanY) {setnames(res, "num1", "variance") 
         } else {
              g1 <- melt(DT2[,c(paste0("g1__", names(Y1),"___", ech1names), "period_country"), with=F], id="period_country")
	        g1[, namess:=substr(variable, 5, nchar(trim(as.character(variable))))] 
              g1[, variable:=NULL]
              setnames(g1, "value", "grad1")
	        setkeyv(g1, c("period_country", "namess"))
       	  setkeyv(res, c("period_country", "namess"))
	 	  res <- merge(res, g1, all=T)	 

		  g2 <- melt(DT2[,c(paste0("g2__", names(Y1),"___", ech1names), "period_country"), with=F], id="period_country")
	 	  g2[, namess:=substr(variable, 5, nchar(trim(as.character(variable))))] 
	 	  g2[, variable:=NULL]
	 	  setnames(g2, "value", "grad2")
	 	  setkeyv(g2, c("period_country", "namess"))
	 	  res <- merge(res, g2, all=T)
	 	  echs <- g1 <- g2 <- NULL
  }
  rm(DT2, DT3)
  res[, var:=num1]
  res[meanY, var:=(grad1*grad1*num1)+
                  (grad2*grad2*den1)+
                2*(grad1*grad2*num_den1)]
  res[, estim:=total] 
  res[meanY, estim:=total/pop_size]  
  if (netchanges) { if (meanY) res1 <- res[, c(namesperc, "name1", "name2",
                                               "estim", "num1", "den1",
                                               "grad1", "grad2"), with=F]                                                       
                  } else res1 <- NULL
  
  if (!is.null(Dom)) { res <- res[, .N, c(namesperc, "dom", "name1", "sample_size",
                                      "pop_size", "estim", "var")]
                } else res <- res[, .N, c(namesperc, "name1", "sample_size",
                                      "pop_size", "estim", "var")]

  #-------------------------------------------------------------------------*
  # DESIGN EFFECT (DEFF) ESTIMATION - VARIANCE UNDER SIMPLE RANDOM SAMPLING |
  #-------------------------------------------------------------------------*

  # We aggregate the target variables at household level
 
  DTs <- DT[, lapply(.SD, sum, na.rm=TRUE), keyby=c(namesperc, names(id), "w_final"), .SDcols = namesYp]

     if (meanY) {    
          DTm <- DT[, lapply(.SD, max, na.rm=TRUE), keyby=c(namesperc, names(id)), .SDcols = names(ech1)]
      } else {
         DTm <- DT[, lapply(.SD, sum, na.rm=TRUE), keyby=c(namesperc, names(id)), .SDcols = names(ech1)]
    }

  setnames(DTm, names(ech1), paste0(names(ech1), "m"))
  DTs <- merge(DTs, DTm)

  setkeyv(DTs, namesperc)

  # Linearised variables

  if (meanY) { Y2a <- lin.ratio(Y=DTs[, names(Y1), with=F],
                                  Z=DTs[, ech1names, with=F],
                                  weight=DTs[["w_final"]], Dom=NULL)
                 setnames(Y2a, names(Y2a), paste0("lin___", names(Y1)))
                 DTs <- data.table(DTs, Y2a)
                 Y2a <- paste0("lin___", names(Y1))
            } else Y2a <- names(Y1)

  svars <- DTs[["w_final"]]
  DTsd <- DTs[, lapply(.SD[, Y2a, with = F], function(x) 
                       sum(w_final*((x-sum(w_final*x)/sum(w_final))^2))/(sum(w_final)-1)),
                       keyby=namesperc]
  setnames(DTsd, Y2a, paste0("sd_w__", names(Y1)))
  DTs <- merge(DTs, DTsd)

  DTm <- DTs[, lapply(.SD[, paste0(names(ech1), "m"), with = F], function(x) sum(w_final*x, na.rm=TRUE)),
                       keyby=namesperc]
  setnames(DTm, paste0(names(ech1), "m"), paste0("pop_", names(ech1)))
  DTs <- merge(DTs, DTm)

  DTsd <- DTs[, lapply(.SD, sd, na.rm=TRUE), keyby=namesperc, .SDcols = Y2a]
  setnames(DTsd, Y2a, paste0("sd_nw__", names(Y1)))
  DTs <- merge(DTs, DTsd)

  DTm <- DTs[, lapply(.SD, sum, na.rm=TRUE), keyby=namesperc, .SDcols = names(ech1)]
  setnames(DTm, names(ech1), paste0("samp_size__", names(ech1)))
  DTs <- merge(DTs, DTm)
    
  DTx <- DTs[, .N, keyby=c(namesperc, paste0("sd_w__", names(Y1)),
                               paste0("sd_nw__", names(Y1)),
                               paste0("pop_", names(ech1)),
                               paste0("samp_size__", names(ech1)))]
  DTx[, N:=NULL]

  sd1 <- melt(DTx[, c(namesperc, paste0("sd_w__", names(Y1))), with=F],  id=namesperc)
  sd1[, name1:=substr(variable, 7, nchar(trim(as.character(variable))))] 
  sd1[, variable:=NULL]
  setnames(sd1, "value", "sd_w")
  setkeyv(sd1, c(namesperc, "name1"))
  setkeyv(res, c(namesperc, "name1"))
  res <- merge(res, sd1, all=T)

  sd1 <- melt(DTx[, c(namesperc, paste0("sd_nw__", names(Y1))), with=F],  id=namesperc)
  sd1[, name1:=substr(variable, 8, nchar(trim(as.character(variable))))] 
  sd1[, variable:=NULL]
  setnames(sd1, "value", "sd_nw")
  setkeyv(sd1, c(namesperc, "name1"))
  setkeyv(res, c(namesperc, "name1"))
  res <- merge(res, sd1, all=T)

  sd1 <- melt(DTx[, c(namesperc, paste0("pop_", names(ech1))), with=F],  id=namesperc)
  sd1[!is.null(Dom), dom:=substr(variable, 10, nchar(trim(as.character(variable))))] 
  sd1[, variable:=NULL]
  setnames(sd1, "value", "pop")
  if (is.null(Dom)) nds <- namesperc else nds <- c(namesperc, "dom")
  setkeyv(sd1, nds)
  setkeyv(res, nds)
  res <- merge(res, sd1, all=T)

  sd1 <- melt(DTx[, c(namesperc, paste0("samp_size__", names(ech1))), with=F],  id=namesperc)
  sd1[!is.null(Dom), dom:=substr(variable, 17, nchar(trim(as.character(variable))))] 
  sd1[, variable:=NULL]
  setnames(sd1, "value", "sampl_siz")
  setkeyv(sd1, nds)
  setkeyv(res, nds)
  res <- merge(res, sd1, all=T)

  res[, stderr_nw:=100*sqrt((1-(sample_size/pop_size))/pop_size * sd_nw * sd_nw/sample_size )]
  res[, stderr_w:=100*sqrt((1-(sample_size/pop_size))/pop_size * sd_w * sd_w/sample_size )]

  DT <- DTw <- DTx <- DTs <- DTsd <- sd1 <- nds <-NULL


  res[, N:=NULL]
  res[, se:=sqrt(var)]
  res[, rse:=se/estim]
  res[, cv:=rse*100]
  tsad <- qnorm(0.5*(1+confidence))
  res[, absolute_margin_of_error:=tsad*se]
  res[, relative_margin_of_error:=tsad*cv]
  res[, CI_lower:=estim - tsad*se]
  res[, CI_upper:=estim + tsad*se]

  list(data_net_changes=DT1, var_grad=res1, results=res)
}   
