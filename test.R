



rm(list = ls())
library("laeken")
library("foreach")
library("data.table") 
### Define
data(eusilc)
dati <- data.frame(rep(2013,nrow(eusilc)), 1:nrow(eusilc),
                   db031=eusilc$db030, eusilc)
colnames(dati)[c(1,2)] <- c("rb010", "IDd0")
dati1 <- data.frame(rep(2012,nrow(eusilc)), 1:nrow(eusilc),
                    db031=eusilc$db030, eusilc)
colnames(dati1)[c(1,2)] <- c("rb010", "IDd0")
dati <- rbind(dati, dati1)
dati <- data.frame(1:nrow(dati), dati)
colnames(dati)[1] <- "IDd"
head(dati)

library(vardpoor)
aa<-varpoord("eqIncome", "rb050", 
                     income_thres = NULL,
                     wght_thres = NULL,
                     ID_household = "db030",
                     id = NULL, H="db040",
                     PSU="db031", N_h=NULL,
                     sort = NULL,
                     Dom = "db040",
                     gender = NULL, 
                     X = NULL,
                     g = NULL,
                     q = rep(1, if (is.null(dataset)) 
                            nrow(as.data.frame(H)) else nrow(dataset)),
                     dataset =  dati1,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     confidence = .95,
                     outp_lin = T,
                     outp_res = FALSE,
                     na.rm=FALSE,
                     several.ok=FALSE,
                     type="linrmpg")

celsd="D:/Program/vardpoor/R/"
celsd="G:/!Vadiba/MatNodrD/!!R/vardpoor/vardpoor/R/"

setwd(celsd)
source("domain.R")
source("incPercentile.R")
source("residual_est.R")
source("variance_est.R")
source("var_srs.R")
source("vardom.R")
source("linarpr.R")
source("linarpt.R")
source("lingini.R")
source("lingini2.R")
source("lingpg.R")
source("linpoormed.R")
source("linqsr.R")
source("linrmpg.R")
source("varpoord.R")



 period <- "rb010"
 inc <- "eqIncome"
 Y <- "eqIncome"
 Z <- "rb050"
 w_final <- "rb050"
 weight <- "rb050"
 weights <- "rb050"
 Dom <- c("db040")
 sort <- NULL
 na.rm <- FALSE
 alpha <- 20
 var_name <- "lin"
 income_thres <- NULL
 wght_thres <- "rb050"
 percentage <- 60
 order_quant <- 50
 alpha <- 20
 confidence = .95
 datasetX <- dati
 X <- NULL
 periodX <- NULL
 X_ID_household <- NULL
 g <- NULL
 q <- rep(1, nrow(datasetX))
 k = c(20, 80)

 ind_gr = NULL
 gender <- NULL
 PSU <- "db031"
 H <- "db040"
 type<-"all_choises"
 several.ok=FALSE
 N_h=NULL
 ID_household <- "db030"
outp_lin <- T
 Dom <- c("db040")
 Dom <- NULL
 dataset <- dati
 id <- "IDd"

Y=Y4
w_final=w_final2

dati=data.frame(1:nrow(eusilc),eusilc)
colnames(dati)[1] <- "IDd"

aa<-vardom("eqIncome", H="db040", PSU="IDd", w_final="rb050",
            Dom = "db040", N_h=NULL, Z = NULL, X = NULL, g = NULL,
            dataset = dataset,
            q = rep(1, if (is.null(dataset)) 
                        nrow(as.data.frame(H)) else nrow(dataset)),
             confidence = .95, breakdown="TOTAL")

d<-linarpt(inc="eqIncome", id="IDd", weight = "rb050", Dom = NULL,
         dataset = dati1, percentage = 60, order_quant=50, na.rm = FALSE)

period<-NULL
inco <- inc
ids <- id
wght <- weight
ind <- ind
percentag <- p
order_quants <- order_quant
quant_val <- quantile

dd<-linarpr("eqIncome", id="IDd", weight = "rb050", Dom = "db040",
         dataset = dati, percentage = 60, order_quant=50, na.rm = FALSE)


a<-gini("eqIncome", weights = "rb050", data = eusilc)

tt<-bootVar("eqIncome", weights = "rb050", design = "db040",
    data = eusilc, indicator = a, R = 50,
    bootType = "naive", seed = 123)

aa<-varpoord(inc, w_final, 
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
                     dataset =  dataset,
                     percentage=60,
                     order_quant=50,
                     alpha = 20,
                     confidence = .95,
                     na.rm=FALSE,
                     several.ok=FALSE,
                     type="lingini")
aa$lin[20:40]

aa$var
aa$estim
tt$var


