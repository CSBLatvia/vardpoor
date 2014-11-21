
library("data.table")
library("reshape2")
source("T:/!Vadiba/MatNodrD/R/vardpoor/expvar.R")
source("T:/!Vadiba/MatNodrD/R/vardpoor/opt_var.R")

H <- data.table(H=1:3,H2=1:3)
Yh <- data.table(Yh=10*1:3, Yh2=10*4:6)
S2h <- data.table(S2h=10*runif(3), S2h2=10*runif(3))
nh <- data.table(nh=4*1:3)
poph <- data.table(Nh=8*1:3)
Rh <- data.table(Rh=rep(1,3))
w  <- data.table (w=12*1:3,w2=12*1:3)
y <- data.table (yh=5*1:3, yh2=6*4:6)
n <- data.table (n=rep(2,3), n2=rep(2,3))
deffh <- data.table(deffh=rep(2,3), deffh2=rep(3,3))
Dom <- data.table(dd=c(1,1,1))
dataset <- NULL
confidence = .95

#n - sample size
#pop - population size
#s2 - S^2 estimation
#RR - respondence level
#deff - design effect


# Function S^2 estimation
S2est <- function(y, w = rep(1, length(y))) {
  N <- sum(w)
  S2est <- (sum(y^2 * w) - sum(y * w)^2 / N) / (N-1)
  return(S2est)}

H <- data.table(H=c(1:3,1:3))
Yh <- data.table(S2h=10*runif(6), S2h2=10*runif(6))
YY <- names(Yh)
data <- data.table(H, Yh)

S2h <- data[, lapply(.SD, S2est), keyby="H", .SDcols=YY]
S2h[, H:=NULL]
str(data)
Yh <- data[, lapply(.SD, sum), keyby="H", .SDcols=YY]
Yh[, H:=NULL]

H <- data.table(H=c(1:3))

n=10
n_izl_izviet <- opt_var(H=H, S2h=S2h, n=n, Nh=Nh, Rh=Rh)

nh <- n_izl_izviet[variable=="S2h", "nh", with=F]
result <- expvar(Yh, H, S2h, nh, poph, Rh, deffh, Dom=NULL, dataset = NULL, confidence = .95)
result



