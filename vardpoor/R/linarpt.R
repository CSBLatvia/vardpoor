#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************
#***                                                                                         ***
#***                                                                                         ***
#***                    LINEARIZATION OF THE AT-RISK-OF-POVERTY THRESHOLD                    ***
#***                                                                                         ***
#***                                                                                         ***
#***********************************************************************************************
#***********************************************************************************************
#***********************************************************************************************

linarpt <- function(Y, id = NULL, weight = NULL, sort = NULL,
                    Dom = NULL, period=NULL, dataset = NULL,
                    percentage = 60, order_quant = 50,
                    var_name = "lin_arpt", kern_method = "gaussian",
                    r = NULL, ro = NULL, h_breaks = NULL,
                    checking = TRUE) {

   ## initializations
   if (min(dim(as.data.frame(var_name)) == 1) != 1) {
        stop("'var_name' must have defined name of the linearized variable")}
   if (min(dim(as.data.frame(kern_method)) == 1) != 1) {
        stop("'kern_method' must have defined method")}
   
   if (checking) {
       percentag <- check_var(vars = percentage, varn = "percentage",
                               varntype = "numeric0100") 

       order_quant <- check_var(vars = order_quant, varn = "order_quant",
                                varntype = "numeric0100")

       kern_method <- check_var(vars = kern_method, varn = "kern_method",
                                varntype = "kern_method")
       
       r <- check_var(vars = r, varn = "r", varntype = "pinteger",
                       kern_method = kern_method)
       
       ro <- check_var(vars = ro, varn = "ro", varntype = "numeric01",
                       kern_method = kern_method)
       
       h_breaks <- check_var(vars = h_breaks, varn = "h_breaks",
                             varntype = "pinteger", kern_method = kern_method)
       
       Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                      ncols = 1, isnumeric = TRUE,
                      isvector = TRUE, grepls = "__")
       Ynrow <- length(Y)

       weight <- check_var(vars = weight, varn = "weight",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, isnumeric = TRUE,
                           isvector = TRUE)

       sort <- check_var(vars = sort, varn = "sort",
                         dataset = dataset, ncols = 1,
                         Ynrow = Ynrow, mustbedefined = FALSE,
                         isnumeric = TRUE, isvector = TRUE)

       period <- check_var(vars = period, varn = "period",
                           dataset = dataset, Ynrow = Ynrow,
                           ischaracter = TRUE, mustbedefined = FALSE,
                           duplicatednames = TRUE)

       Dom <- check_var(vars = Dom, varn = "Dom", dataset = dataset,
                        Ynrow = Ynrow, ischaracter = TRUE,
                        mustbedefined = FALSE, duplicatednames = TRUE,
                        grepls = "__")

       id <- check_var(vars = id, varn = "id", dataset = dataset,
                       ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                       periods = period)

   }
   dataset <- NULL

   ## computations
   ind0 <- rep.int(1, length(Y))
   period_agg <- period1 <- NULL
   if (!is.null(period)) { period1 <- copy(period)
                           period_agg <- data.table(unique(period))
                       } else period1 <- data.table(ind = ind0)
   period1_agg <- data.table(unique(period1))

   # ARPT by domain (if requested)

   quantile <- incPercentile(Y = Y,
                             weights = weight,
                             sort = sort,
                             Dom = Dom,
                             period = period,
                             k = order_quant,
                             dataset = NULL,
                             checking = FALSE)

   quantile <- data.table(quantile)
   setnames(quantile, names(quantile)[ncol(quantile)], "quantile")
   if (ncol(quantile) > 1) setkeyv(quantile, head(names(quantile), -1))
   threshold <- copy(quantile)
   threshold[, threshold := percentage / 100 * quantile]
   threshold[, quantile := NULL]

   arpt_id <- id
   if (!is.null(period)) arpt_id <- data.table(arpt_id, period)

   if(!is.null(Dom)) {
       Dom_agg <- data.table(unique(Dom))
       setkeyv(Dom_agg, names(Dom_agg))

       arpt_m <- copy(arpt_id)
       for(i in 1 : nrow(Dom_agg)) {
             g <- c(var_name, paste(names(Dom), as.matrix(Dom_agg[i,]), sep = "."))
             var_nams <- do.call(paste, as.list(c(g, sep = "__")))
             ind <- as.integer(rowSums(Dom == Dom_agg[i,][ind0,]) == ncol(Dom))
             arpt_l <- lapply(1:nrow(period1_agg), function(j) {
                              if (!is.null(period)) {
                                      rown <- cbind(period_agg[j], Dom_agg[i])
                                      } else rown <- Dom_agg[i]

                              setkeyv(rown, names(rown))
                              rown2 <- copy(rown)
                              rown <- merge(rown, quantile, all.x = TRUE)
                              ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                              arptl <- arptlinCalc(inco = Y[ind2],
                                                   ids = arpt_id[ind2],
                                                   wght = weight[ind2],
                                                   indicator = ind[ind2],
                                                   order_quants = order_quant,
                                                   quant_val = rown[["quantile"]],
                                                   percentag = percentage,
                                                   kern_method = kern_method, r = r,
                                                   ro = ro, h_breaks = h_breaks)
                               })
           arptl <- rbindlist(arpt_l)
           setnames(arptl, names(arptl), c(names(arpt_id), var_nams))
           arpt_m <- merge(arpt_m, arptl, all.x = TRUE, by = names(arpt_id))
        }
    } else { arptl <- lapply(1 : nrow(period1_agg), function(j) {
                           if (!is.null(period)) {
                                         rown <- period_agg[j]
                                         setkeyv(rown, names(rown))
                                         rown <- merge(rown, quantile, all.x = TRUE)
                                       } else rown <- quantile
                           ind2 <- (rowSums(period1 == period1_agg[j,][ind0,]) == ncol(period1))

                           arptl <- arptlinCalc(inco = Y[ind2],
                                                ids = arpt_id[ind2],
                                                wght = weight[ind2],
                                                indicator = ind0[ind2],
                                                order_quants = order_quant,
                                                quant_val = rown[["quantile"]],
                                                percentag = percentage,
                                                kern_method = kern_method, r = r,
                                                ro = ro, h_breaks = h_breaks)
    
                       })
               arpt_m <- rbindlist(arptl)
               setnames(arpt_m, names(arpt_m), c(names(arpt_id), var_name))
            }
    arpt_m[is.na(arpt_m)] <- 0
    setkeyv(arpt_m, names(arpt_id))
    return(list(quantile = quantile, value = threshold, lin = arpt_m))
 }

bandwith_plug <- function(y, w) {
        N <- sum(w)
        # h=S/N^(1/5)
        1 / N * sqrt(N * sum(w * y ^ 2) - (sum(y * w)) ^ 2) * N ^ (-0.2)
 }


piecewis1 <- function(x, xi, pak) {as.numeric(x >= xi) * (x - xi) ^ pak}
piecewis2 <- function(x, xi, pak) {as.numeric(x >= xi) * 1 / (pak + 1) * (x - xi) ^ (pak + 1)}


# Smooth_spline
smooth_spline <- function(inco, wght, quant_val, r, ro, h_breaks) {
  ..density.. <- dat <- eqIncome1 <- NULL
  YY <- N <- a1 <- a2 <- v1 <- vv <- NULL
  dataset <- data.table(YY = inco, wght = wght, sv = 1)
  hh <- bandwith_plug(y = inco, w = wght)
  pec <- incPercentile(Y = "YY", weights = "sv",
                       k = 10, dataset = dataset)
  dataset[, eqIncome1 := (quant_val -  YY) / hh]
 #sum(dataset$eqIncome1)
 #hist(dataset$YY, prob = TRUE, breaks = h_breaks)

  rez <- hist(dataset$eqIncome1, prob = TRUE, breaks = h_breaks)
  tt <- rez$breaks[rez$density != 0]
  n <- sum(rez$density != 0)
  fs <- rez$density[rez$density != 0]
  k <- rez$counts[rez$density != 0]

  h <- rep(0, n)
  for (i in (1:n)) h[i] <- tt[i+1] - tt[i]

  v <- h * fs
#  sum(v)
#  tt
#  fs
#  v

#  k/sum(k)

  gd <- ggplot(dataset) + geom_histogram(aes(x = eqIncome1, y = ..density..),
                                         fill = "grey", bins = n, color = "black")

  gd

  dat2 <- data.table::data.table(k, v)
  dat[, tt := 1:.N]

  stf <- function(x, r1, koef = NULL) {
    aa <- data.table(do.call(cbind, lapply(0:(r1 - 1), function(j) {
      x ^ j * ifelse(is.null(koef), 1, koef[[paste0("a", j)]])})))
    setnames(aa, names(aa), paste0("a", 0:(r1 - 1)))
    lambda <- data.table(do.call(cbind, lapply(1:n + 1, function(i) {
        (-1) ^ (r1 + 1) / factorial(2 * r1) * (piecewis1(x, xi = tt[i], pak = 2 * r1) -
                                               piecewis1(x, xi = tt[i - 1], pak = 2 * r1)) *
        ifelse(is.null(koef), 1, koef[[paste0("lambda", i - 1)]])})))
    setnames(lambda, names(lambda), paste0("lambda", 1:n))
  
  #mju <- data.table((-1) ^ r1 / factorial(2 * r1 - 1) * (piecewis1(x, xi = tt[1], pak = 2 * r1 - 1)))
  #setnames(mju, names(mju), paste0("mju", 1))
  #  (-1)^r/(2*r-1)!*mi[3]*piecewise(t>=3.0785185166691328390,(t-3.0785185166691328390)^(2*r-1))+
  #  (-1)^r/(2*r-1)!*mi[4]*piecewise(t>=6.0859765779637910368,(t-6.0859765779637910368)^(2*r-1))+
  #  (-1)^r/(2*r-1)!*mi[5]*piecewise(t>=8.8467990563625822271,(t-8.8467990563625822271)^(2*r-1)):
  
  #apv <- cbind(aa, lambda, mju)[]
    apv <- cbind(aa, lambda)[]
    if (!is.null(koef))  apv <- rowSums(apv)
    apv}

  stf_int <- function(x, r1, zim = 1, koef = NULL) {
      aa <- data.table(do.call(cbind, lapply(0:(r1 - 1), function(j) {
                zim / (j + 1) * x ^ (j + 1) * ifelse(is.null(koef), 1, koef[[paste0("a", j)]])}))
            )
  
      setnames(aa, names(aa), paste0("a", 0:(r1 - 1)))
      lambda <- data.table(do.call(cbind, lapply(1:n + 1, function(i) {
                            zim * (-1) ^ (r1 + 1) / factorial(2 * r1) * (piecewis2(x, xi = tt[i], pak = 2 * r1) -
                                                                         piecewis2(x, xi = tt[i - 1], pak = 2 * r1)) *
                            ifelse(is.null(koef), 1, koef[[paste0("lambda", i - 1)]])}))
          )
      setnames(lambda, names(lambda), paste0("lambda", 1:n))
     #mju <- data.table(zim * (-1) ^ r1 / factorial(2 * r1) * (piecewis1(x, xi = tt[1], pak = 2 * r1)))
     #setnames(mju, names(mju), paste0("mju", 1))
     #apv <- cbind(aa, lambda, mju)[]
     apv <- cbind(aa, lambda)[]
     if (!is.null(koef))  apv <- rowSums(apv)
     apv }

  st <- function(y) {sum(stf(x = y, r1 = r, koef = rez))}

  sss <- lapply(1:n, function(i) {
      apr <- data.table(t(rep( -sqrt(ro) / n, n)))
      setnames(apr, names(apr), paste0("lambda", 1:n))  
      apr2 <- data.table(sqrt(ro))
      setnames(apr2, names(apr2), paste0("lambda", i))
      apr3 <- stf_int(x = tt[i], r1 = r, zim = -1)
      apr4 <- stf_int(x = tt[i + 1], r1 = r)
      apr5 <- data.table(v = - v[i])
      apr <- rbindlist(list(apr4, apr3, apr, apr2, apr5), fill =TRUE)
      rez1 <- apr[, lapply(.SD, sum, na.rm = TRUE), .SDcols = names(apr)] 
      rez1[] })
  sss <- rbindlist(sss, fill = TRUE)

  sss0 <- stf(x = tt[1], r1 = r)
sssn1 <- stf(x = tt[n + 1], r1 = r)
#sss[n+2]:=st1(3.0785185166691328390)
#sss[n+3]:=st1(6.0859765779637910368)
#sss[n+4]:=st1(8.8467990563625822271)

sssn1
#sss <- rbindlist(list(sss0, sss, sssn1), fill = TRUE)
sss[is.na(v), v := 0]                 

sar <- c(tt[1], tt[n + 1])
apst <- lapply(0:(r-1), function(j){
  ap1 <- data.table(t(sapply(1:n, function(i) {(tt[i+1])^(j+1)-(tt[i])^(j+1)})))
  setnames(ap1, names(ap1), paste0("lambda", 1:n))
  #mju <- data.table((tt[1]) ^ j,  tt[n + 1])
  #setnames(mju, names(mju), paste0("mju", 1:2))
  #apv <- cbind(ap1, mju)[]
  apv <- ap1
  apv[]
  #  mi[3]*(3.0785185166691328390)^j+
  #  mi[4]*(6.0859765779637910368)^j+
  #  mi[5]*(8.8467990563625822271)^j
})

apst <- rbindlist(apst, fill = TRUE)
sss2 <- rbindlist(list(sss, apst), fill = TRUE)

for (i in 0:(r - 1)) sss2[is.na(get(paste0("a", i))), (paste0("a", i)):=0]
#sss2[is.na(mju2), mju2:=0]
#sss2[is.na(mju1), mju1:=0]
sss2[is.na(v), v:=0]

#Sistemu risinashana#
bv <- - sss2$v
sss2[, v:= NULL]
rez <- solve(sss2, bv)

#grafiks := plot(st1(t), t = 0 .. n, thickness = 2):
#display(grafiks, histogramma2);

sss <- lapply(1:n, function(i) {
  apr3 <- stf_int(x = tt[i], r1 = r, zim = -1, koef = rez)
  apr4 <- stf_int(x = tt[i + 1], r1 = r, koef = rez)
  rez <- data.table(a1 = apr3, a2 = apr4)
  rez[, vv := a1 + a2]
  rez[, v1 := v[i]]
  rez[, ]
})
sss <- rbindlist(sss)
s_spline <- NULL
  calcss <- dataset[, .N, by = c("eqIncome1")]
  calcs <- rbindlist(lapply(1:nrow(calcss), function(i) { 
                  vv <- calcss$eqIncome1[i]   
                  data.table(eqIncome1 = vv, s_spline = st(vv))}))
  dataset <- merge(dataset, calcs, by = "eqIncome1", all = TRUE)
  f_vert <- dataset[, sum(s_spline * wght)  / (sum(wght) * hh) ]
  return(f_vert)  
}


gaussian_kern <- function(inco, wt, quant_val, hh){
     N <- sum(wt); # Estimated (sub)population size
     u <- (quant_val - inco) / hh
     vect_f <- exp(-(u^2) / 2) / sqrt(2 * pi)
     f_quant <- sum(vect_f * wt) / (N * hh) # Estimate of F'(quantile)
    return(f_quant)
}

    ## workhorse
arptlinCalc <- function(inco, ids, wght, indicator, order_quants,
                        quant_val, percentag, kern_method, r, ro, h_breaks) {
    wt <- wght * indicator
    N <- sum(wt); # Estimated (sub)population size
    h <- bandwith_plug(y = inco, w = wt)

    if (kern_method == "gaussian") {f_quant <- gaussian_kern(inco = inco, wt = wt,
                                                        quant_val = quant_val, hh = h)}
    if (kern_method == "smooth_splines") {f_quant <- smooth_spline(inco = inco, wght = wt,
                                                                   quant_val = quant_val,
                                                                   r = r, ro = ro,
                                                                   h_breaks = h_breaks) }

 #****************************************************************************************
 #*                    LINEARIZED VARIABLE OF THE POVERTY THRESHOLD                      *
 #****************************************************************************************
    lin <- - (percentag / 100) * (1 / N) * indicator * ((inco <= quant_val) - order_quants / 100) / f_quant
    lin_id <- data.table(ids, lin)
    return(lin_id)
}



