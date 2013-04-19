
vardpoor <- function(inc, w_final, 
                     income_thres = NULL,
                     wght_thres = NULL,
                     id = NULL, H, PSU, N_h,
                     sort = NULL,
                     Dom = NULL,
                     gender = NULL, 
                     X = NULL,
                     g = NULL,
                     q = rep(1, if (is.null(dataset)) 
                            nrow(as.data.frame(H)) else nrow(dataset)),
                     dataset = NULL,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     confidence = .95,
                     na.rm=FALSE,
                     several.ok=FALSE,
                     type="lin_rmpg") {

  ### Checking
  if (min(dim(as.data.frame(var_name))==1)!=1) {
      stop("'var_name' must have defined name of the linearized variable")}

  all_choices <- c("linarpr","linarpt","lingpg","linpoormed",
                   "linrmpg","lingini","lingini2","linqsr")
  choices<-c("all_choises",all_choices)
  type<-tolower(type)
  type <- match.arg(type,choices, several.ok)
  if (type=="all_choises") type=all_choices

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
          if (min(id %in% names(dataset))==1) id <- data.frame(dataset[, id])
          names(id)<-id2  }
      if(!is.null(H)) {
          aH <- H  
          if (min(H %in% names(dataset))!=1) stop("'H' does not exist in 'dataset'!")
          if (min(H %in% names(dataset))==1) {
                                H <- data.frame(dataset[, H])
                                names(H) <- aH }}
      if(!is.null(PSU)) {
          aPSU <- PSU  
          if (min(PSU %in% names(dataset))!=1) stop("'PSU' does not exist in 'dataset'!")
          if (min(PSU %in% names(dataset))==1) {
                                PSU <- data.frame(dataset[, PSU])
                                names(PSU) <- aPSU }}
      if(!is.null(gender)) {
          if (min(gender %in% names(dataset))!=1) stop("'gender' does not exist in 'dataset'!")
          if (min(gender %in% names(dataset))==1) gender <- dataset[, gender] }
      if(!is.null(sort)) {
          if (min(sort %in% names(dataset))!=1) stop("'sort' does not exist in 'dataset'!")
          if (min(sort %in% names(dataset))==1) sort <- dataset[, sort] }
      if(!is.null(X)) {
          if (min(X %in% names(dataset))!=1) stop("'X' does not exist in 'dataset'!")
          if (min(X %in% names(dataset))==1) X <- dataset[, X] }
      if(!is.null(g)) {
          if (min(g %in% names(dataset))!=1) stop("'g' does not exist in 'dataset'!")
          if (min(g %in% names(dataset))==1) g <- dataset[, g] }
      if ((!is.null(q))&(length(q)==1)) {
          if (min(q %in% names(dataset))!=1) stop("'q' does not exist in 'dataset'!")
          if (min(q %in% names(dataset))==1) q <- dataset[, q] }
      if (!is.null(Dom)) {
          Dom2 <- Dom
          if (min(Dom %in% names(dataset))!=1) stop("'Dom' does not exist in 'data'!")
          if (min(Dom %in% names(dataset))==1) {  
                  Dom <- as.data.frame(dataset[, Dom]) 
                  names(Dom) <- Dom2 }    }
      }

  # inc
  if(!is.numeric(as.vector(inc))) stop("'inc' must be a numeric vector")
  inc <- as.data.frame(inc)
  n <- nrow(inc)
  if (any(is.na(inc))) warning("'inc' has unknown values")
              
  # id 
  if(is.null(id)) id <- 1:n
  id <- as.data.frame(id)
  if (is.null(names(id))||(names(id)=="1:n")) names(id) <- "ID"

  # w_final 
  w_final <- as.vector(w_final)
  if(is.null(w_final)) { w_final <- rep(1, n)
      } else if(!is.numeric(w_final)) stop("'weight' must be a numeric vector")
  if (length(w_final) != n) stop("'w_final' must have the same length as 'inc'")

  # income_thres
  if(!is.null(income_thres)) {
        if(!is.numeric(as.vector(income_thres))) stop("'income_thres' must be a numeric vector")
        if (any(is.na(income_thres))) warning("'income_thres' has unknown values")
      } else income_thres <- inc[,1]

  # w_final 
  wght_thres <- as.vector(wght_thres)
  if(is.null( wght_thres)) { wght_thres <- weight
      } else if(!is.numeric(wght_thres)) stop("'wght_thres' must be a numeric vector")
  if (length(w_final) != n) stop("'w_final' must have the same length as 'inc'")

  # H
  H <- as.matrix(H)
  if (nrow(H) != n) stop("'H' must have the same length as 'inc'")
  if (ncol(H) != 1) stop("'H' has more than 1 column")
  if (any(is.na(H))) stop("'H' has unknown values")
  if (is.null(colnames(H))) stop("'H' must be colnames")
  
  # PSU
  PSU <- as.matrix(PSU)
  if (any(is.na(PSU))) stop("'PSU' has unknown values")
  if (nrow(PSU) != n) stop("'PSU' must have the same length as 'inc'")
  if (ncol(PSU) != 1) stop("'PSU' has more than 1 column")
  
  # gender
  if(!is.null(gender)) {
      if(!is.factor(gender)) stop("'gender' must be a factor.")
      if(length(levels(gender)) != 2) stop("'gender' must have exactly two levels")
      if(!all(levels(gender) == c(1, 2))) {
          gender <- factor(gender, labels=c(1,2))
          warning("The levels of gender were internally recoded - your first level has to correspond to males")
         } 
      if(length(gender) != n) stop("'gender' must have the same length as 'x'")
    }

  # N_h
  N_h <- as.data.frame(N_h)
  if (ncol(N_h) != 2) stop("'N_h' should be two columns")
  if (!is.numeric(N_h[, 2])) stop("The second column of 'N_h' should be numerical")
  if (any(is.na(N_h))) stop("'N_h' has unknown values")
  if (is.null(colnames(N_h))) stop("'N_h' must be colnames")
  if (colnames(H) != colnames(N_h)[1]) stop("Strata titles for 'H' and 'N_h' is not equal")
  if (any(is.na(merge(unique(H), N_h, all.x = T)))) stop("'N_h' is not defined for all stratas")
  
  # sort
  if(!is.null(sort) && !is.vector(sort) && !is.ordered(sort)) {
        stop("'sort' must be a vector or ordered factor") }
  if(!is.null(sort) && length(sort) != n) stop("'sort' must have the same length as 'x'")     

  # Dom
  if (!is.null(Dom)) {
    Dom <- as.matrix(Dom)
    if (nrow(Dom) != n) stop("'Dom' and 'Y' have different row count")
    if (any(is.na(Dom))) stop("'Dom' has unknown values")
    if (is.null(colnames(Dom))) stop("'Dom' must be colnames")
  }
         
  # X
  if (!is.null(X)) {
    X <- as.matrix(X)
    if (nrow(X) != n) stop("'X' and 'Y' have different row count")
    if (any(is.na(X))) stop("'X' has unknown values")
  }
      
  # g
  if (!is.null(X)) {
    g <- as.vector(g)
    if (!is.numeric(g)) stop("'g' must be a numeric vector")
    if (length(g) != n) stop("'g' must have the same length as 'inc'")
    if (any(is.na(g))) stop("'g' has unknown values")
    if (any(g == 0)) stop("'g' value can not be 0")
  }
    
  # q
  if (!is.null(X)) {
    q <- as.vector(q)
    if (!is.numeric(q)) stop("'q' must be a numeric vector")
    if (length(q) != n) stop("'q' must have the same length as 'inc'")
    if (any(is.na(q))) stop("'q' has unknown values")
    if (any(is.infinite(q))) stop("'q' value can not be infinite")
  }

  ### Calculation

  Y1 <- id
  Domans <- NULL
  estim<-c()
  estim2<-c()
  estim_eu<-c()
  if ("linarpt" %in% type) {
       varpt <- linarpt(inc, id, w_final, sort, Dom, dataset=NULL,
                        percentage, order_quant, na.rm, "lin_arpt")
       Y1 <- merge(Y1,varpt$lin,by=colnames(id),all.x=T)
       vals<-data.frame(varpt$value)
       if (is.null(Domans)) Domans <- vals[1:ncol(Dom)]
       colnames(vals)[ncol(vals)]<-"value" 
       esti<-data.frame("ARPT",vals,NA)
       colnames(esti)[c(1,ncol(esti))]<-c("type","value_eu")
       estim<-rbind(estim,esti)
       estim2<- rbind(estim2,vals[ncol(vals)])
     }
  if ("linarpr" %in% type) {
       varpr <- linarpr(inc, id, w_final, income_thres=NULL, wght_thres=NULL, sort, Dom, 
                        dataset=NULL, percentage, order_quant, na.rm, "lin_arpr")
       Y1 <- merge(Y1,varpr$lin,by=colnames(id),all.x=T)
       vals<-data.frame(varpr$value)
       if (is.null(Domans)) Domans <- vals[1:ncol(Dom)]
       colnames(vals)[ncol(vals)]<-"value" 
       esti<-data.frame("ARPR",vals,NA)  
       colnames(esti)[c(1,ncol(esti))]<-c("type","value_eu")
       estim<-rbind(estim,esti)
       estim2<- rbind(estim2,vals[ncol(vals)]) 
     }
  if (("lingpg" %in% type)&&(!is.null(gender))) {
        vgpg <- lingpg(inc, id, gender, w_final, sort, Dom,
                       dataset=NULL, alpha, na.rm, "lin_gpg")
        Y1 <- merge(Y1,vgpg$lin,by=colnames(id),all.x=T)
        vals<-data.frame(vgpg$value)
        if (is.null(Domans)) Domans <- vals[1:ncol(Dom)]
        colnames(vals)[ncol(vals)]<-"value" 
        esti<-data.frame("GPG",vals,NA)  
        colnames(esti)[c(1,ncol(esti))]<-c("type","value_eu")
        estim<-rbind(estim,esti)
        estim2<- rbind(estim2,vals[ncol(vals)]) 
     }
  if ("linpoormed" %in% type) {
        vporm <- linpoormed(inc, id, w_final, sort, Dom, dataset=NULL,
                            percentage, order_quant, na.rm, "lin_poor")
        Y1 <- merge(Y1,vporm$lin,by=colnames(id),all.x=T)
        vals<-data.frame(vporm$value)
        if (is.null(Domans)) Domans <- vals[1:ncol(Dom)]
        colnames(vals)[ncol(vals)]<-"value" 
        esti<-data.frame("linpoormed",vals,NA)  
        colnames(esti)[c(1,ncol(esti))]<-c("type","value_eu")
        estim<-rbind(estim,esti)
        estim2<- rbind(estim2,vals[ncol(vals)]) 
      }
  if ("linrmpg" %in% type) {
       vrmpg <- linrmpg(inc, id, w_final, sort, Dom, dataset=NULL,
                        percentage, order_quant, na.rm, "lin_rmpg")
       Y1 <- merge(Y1,vrmpg$lin,by=colnames(id),all.x=T)
       vals<-data.frame(vrmpg$value)
       if (is.null(Domans)) Domans <- vals[1:ncol(Dom)] 
       colnames(vals)[ncol(vals)]<-"value" 
       esti<-data.frame("RMPG",vals,NA)  
       colnames(esti)[c(1,ncol(esti))]<-c("type","value_eu")
       estim<-rbind(estim,esti)
       estim2<- rbind(estim2,vals[ncol(vals)])
     }
  if ("lingini" %in% type) {
       vgini <- lingini(inc, id, w_final, sort, Dom,
                        dataset=NULL, na.rm, "lin_gini")
       Y1 <- merge(Y1,vgini$lin,by=colnames(id),all.x=T)
       vals<-data.frame(vgini$value)
       if (is.null(Domans)) Domans <- vals[1:ncol(Dom)]
       colnames(vals)[c(ncol(vals)-1,ncol(vals))]<-c("value","value_eu")
       esti<-data.frame("GINI",vals)  
       colnames(esti)[1]<-"type"
       estim<-rbind(estim,esti)
       estim2<- rbind(estim2,vals[ncol(vals)-1])
     }
  if ("lingini2" %in% type) {
       vgini2 <- lingini2(inc, id, w_final, sort, Dom,
                          dataset=NULL, na.rm, "lin_gini2")
       Y1 <- merge(Y1,vgini2$lin,by=colnames(id),all.x=T)
       vals<-data.frame(vgini2$value)
       if (is.null(Domans)) Domans <- vals[1:ncol(Dom)] 
       colnames(vals)[c(ncol(vals)-1,ncol(vals))]<-c("value","value_eu")
       esti<-data.frame("GINI2",vals)  
       colnames(esti)[1]<-"type"
       estim<-rbind(estim,esti)
       estim2<- rbind(estim2,vals[ncol(vals)-1])
     }
  if ("linqsr" %in% type) {
       vqsr <- linqsr(inc, id, w_final, sort, Dom, dataset=NULL,
                      alpha, na.rm, "lin_qsr") 
       Y1 <- merge(Y1,vqsr$lin,by=colnames(id),all.x=T)
       vals<-data.frame(vqsr$value)
       if (is.null(Domans)) Domans <- vals[1:ncol(Dom)] 
       colnames(vals)[c(ncol(vals)-1,ncol(vals))]<-c("value","value_eu")
       esti<-data.frame("QSR",vals)  
       colnames(esti)[1]<-"type"
       estim<-rbind(estim,esti)
       estim2<- rbind(estim2,vals[ncol(vals)-1])
    }
  Y2 <- data.frame(Y1)[-1]

  # Calibration
  if (!is.null(X)) Y3 <- residual_est(Y2, X, w_final/g, q)  else Y3 <- Y2
  #colnames(Y3)

  var_est <-variance_est(Y3, H, PSU, w_final, N_h, dataset=NULL, breakdown="TOTAL")

  # Var srs
  Nn <- sum(w_final)
  konst<-Nn*Nn*(1-n/Nn)/n

  z <- colSums(Y3*w_final)
  z_z <- colSums(Y3^2*w_final)
  zm<-do.call(cbind, lapply(1:NCOL(Y3), function(i) rep(z[i]/Nn,n)))

  var_srs_o <- as.matrix(konst/(Nn-1)*colSums(w_final*(Y3-zm)^2))
  var_srs_l <- as.matrix(konst/(Nn-1)*(z_z-1/Nn*z^2))
 
  var_est2 <-  as.matrix(var_est)
  colnames(var_est2)<-"var"
 
  test_v <- (var_est2<0)
  test_v[is.na(test_v)] <- FALSE
  if (any(test_v)) stop("Estimation of variance are negative!")

  deff_o <- var_est2/var_srs_o
  deff_l <- var_est2/var_srs_l
  se <- sqrt(ifelse(var_est2>=0,var_est2,NA))
  rse <- se/estim2
  rse[estim2==0] <- NA
  rownames(rse)<-rownames(se) 
  cv <- rse*100
  tsad <- qnorm(0.5*(1+confidence))
  Absolute_margin_of_error <- tsad*se 
  Relative_margin_of_error <- tsad*cv
  CI_lower <- estim2 - tsad*se
  CI_upper <- estim2 + tsad*se
   
  colnames(se) <- "se"
  colnames(rse) <- "rse"
  colnames(cv) <- "cv"
  colnames(Relative_margin_of_error) <- "Relative_margin_of_error"
  colnames(Absolute_margin_of_error) <- "Absolute_margin_of_error"
  colnames(CI_lower) <- "CI_lower"
  colnames(CI_upper) <- "CI_upper"
  colnames(var_srs_o) <- "Var_SRS_o"
  colnames(var_srs_l) <- "Var_SRS_l"
  colnames(deff_o) <- "Deff_o"
  colnames(deff_l) <- "Deff_l"
 
  all_result<-cbind(estim, var_est,se,rse,cv,Absolute_margin_of_error,
                    Relative_margin_of_error,CI_lower,CI_upper,
                    var_srs_o,var_srs_l,deff_o,deff_l) 

  list(estim=estim,
       var=var_est2,
       se=se,
       rse=rse,
       cv=cv,
       absolute_margin_of_error=Absolute_margin_of_error,
       relative_margin_of_error=Relative_margin_of_error,
       CI_lower=CI_lower,
       CI_upper=CI_upper,
       var_srs_o=var_srs_o,
       var_srs_l=var_srs_l,
       deff_o=deff_o,
       deff_l=deff_l,
       all_result=all_result)
}

