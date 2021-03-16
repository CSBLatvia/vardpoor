#' Variance estimation for measures of annual net change or annual for single stratified sampling designs
#' 
#' @description Computes the variance estimation for measures of annual net change or annual for single stratified sampling designs.
#'
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param Dom Optional variables used to define population domains. If supplied, variables are calculated for each domain. An object convertible to \code{data.table} or variable names as character vector, column numbers.
#' @param periods Variable for the all survey periods. The values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param periods1 The vector of periods from variable \code{periods} describes the first period for measures of change.
#' @param periods2 The vector of periods from variable \code{periods} describes the second period for measures of change.
#' @param in_sample Sample variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param in_frame Frame variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param percentratio Positive numeric value. All linearized variables are multiplied with \code{percentratio} value, by default - 1.
#' @param confidence optional; either a positive value for confidence interval. This variable by default is 0.95.
#' @param correction Logical value. If TRUE calculate variance without covariance (negative variance correction).
#' 
#' @return A list with objects are returned by the function:
#'  \itemize{
#'     \item \code{crossectional_results} - a \code{data.table} containing: \cr
#'       \code{year} -  survey years, \cr
#'       \code{subperiods} -  survey sub-periods, \cr
#'       \code{variable} - names of variables of interest, \cr
#'       \code{Dom} - optional variable of the population domains, \cr
#'       \code{estim} - the estimated value, \cr
#'       \code{var} - the estimated variance of cross-sectional and longitudinal measures, \cr
#'       \code{sd_w} - the estimated weighted variance of simple random sample, \cr
#'       \code{se} - the estimated standard error of cross-sectional or longitudinal, \cr
#'       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'       \code{absolute_margin_of_error} - the estimated absolute margin of error, \cr
#'       \code{relative_margin_of_error} - the estimated relative margin of error, \cr
#'       \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'       \code{CI_upper} - the estimated confidence interval upper bound, \cr 
#'       \code{confidence_level} - the positive value for confidence interval.
#'    \item \code{annual_results} - a \code{data.table} containing:
#'       \code{year_1} -  survey years of \code{years1} for measures of annual net change, \cr
#'       \code{year_2} -  survey years of \code{years2} for measures of annual net change, \cr
#'       \code{Dom} - optional variable of the population domains, \cr
#'       \code{variable} - names of variables of interest, \cr
#'       \code{estim_2} - the estimated value for period2 for measures of annual net change, \cr
#'       \code{estim_1} - the estimated value for period1 for measures of annual net change, \cr
#'       \code{estim} - the estimated value, \cr
#'       \code{var} - the estimated variance, \cr
#'       \code{se} - the estimated standard error, \cr
#'       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'       \code{absolute_margin_of_error} - the estimated absolute margin of error for period1 for measures of annual, \cr
#'       \code{relative_margin_of_error} - the estimated relative margin of error in percentage for measures of annual, \cr
#'       \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'       \code{CI_upper} - the estimated confidence interval upper bound, \cr
#'       \code{confidence_level} - the positive value for confidence interval, \cr 
#'       \code{significant} - is the the difference significant.
#'   \item\code{annual_results_correction} - a \code{data.table} of corrected variables (if correction TRUE) containing:
#'    \code{year_1} -  survey years of \code{years1} for measures of annual net change, \cr
#'       \code{year_2} -  survey years of \code{years2} for measures of annual net change, \cr
#'       \code{Dom} - optional variable of the population domains, \cr
#'       \code{variable} - names of variables of interest, \cr
#'       \code{estim_2} - the estimated value for period2 for measures of annual net change, \cr
#'       \code{estim_1} - the estimated value for period1 for measures of annual net change, \cr
#'       \code{estim} - the estimated value, \cr
#'       \code{var} - the estimated variance, \cr
#'       \code{se} - the estimated standard error, \cr
#'       \code{rse} - the estimated relative standard error (coefficient of variation), \cr
#'       \code{cv} - the estimated relative standard error (coefficient of variation) in percentage, \cr
#'       \code{absolute_margin_of_error} - the estimated absolute margin of error for period1 for measures of annual, \cr
#'       \code{relative_margin_of_error} - the estimated relative margin of error in percentage for measures of annual, \cr
#'       \code{CI_lower} - the estimated confidence interval lower bound, \cr
#'       \code{CI_upper} - the estimated confidence interval upper bound, \cr
#'       \code{confidence_level} - the positive value for confidence interval, \cr 
#'       \code{significant} - is the the difference significant.
#'  }
#'       
#' @references
#' Guillaume OSIER, Virginie RAYMOND, (2015), Development of methodology for the estimate of variance of annual net changes for LFS-based indicators. Deliverable 1 - Short document with derivation of the methodology.
#' 
#' @seealso \code{\link{vardchanges}},
#'          \code{\link{vardannual}}
#'          
#' @keywords vardannual
#'
#' @import data.table
#' @import stringr
#' 
#' @export vardchangstrs

vardchangstrs <- function(Y, H, PSU, w_final,
                          Dom = NULL, periods = NULL,
                          dataset, periods1, periods2,
                          in_sample, in_frame,
                          confidence = 0.95,
                          percentratio = 1, correction=FALSE){

B_l <- Bl_sum <- CI_lower <- CI_upper <- D_l <- NULL
Dl_sum <- P_hl <- ap_hl <- covv <- cv <- estim <- NULL
estim_1 <- estim_2 <- fGhl_sum <- fa1_hl <- fa2_hl <- NULL
g_hl <- ids <- in_sample_1 <- in_sample_2 <- ind_1 <- NULL
ind_2 <- kor <- n_q <- nh1 <- nh2 <- pop1 <- pop2 <- NULL
pop_q <- rse <- rse2 <- sa1_hl <- sa1hl_sum <- NULL
sa2_hl <- sa2hl_sum <- se <- sghl_sum <- type <- NULL
var_1 <- var_2 <- variable_new <- NULL
  
dataset[, (periods) := lapply(.SD[, periods, with = FALSE], as.character)]
calc <- vardom(Y = Y, H = H, PSU = PSU,
               w_final = w_final,
               Dom = Dom, period = periods,
               dataset = dataset[in_sample == 1])$all_result
calc3 <- copy(calc)

outvars <- c("variable", Dom, periods, "estim", "var")
calc <- calc[, outvars, with = FALSE]

setnafill(dataset, type = "const", fill = 0, cols = Y)
if (!is.null(Dom)) { dats <- domain(Y, D = Dom, dataset = dataset)
                     Yvars <- names(dats)
                     dataset <- data.table(dataset, dats) 
                     rm(dats)
              } else Yvars <- Y

periods1 <- data.table(as.character(periods1))
setnames(periods1, names(periods1), periods)
periods1[, ids := 1:.N] 
sar <- c(periods, Yvars, H, w_final, in_sample, in_frame)
frame1 <- merge(periods1, dataset[, c(PSU, sar), with = FALSE],
                all.x = TRUE, by = periods, sort = FALSE, allow.cartesian = TRUE)

setnames(frame1, sar, paste0(sar, "_1"))

periods2 <- data.table(as.character(periods2))
setnames(periods2, names(periods2), periods)
periods2[, ids := 1:.N] 
frame2 <- merge(periods2, dataset[, c(PSU, sar), with = FALSE],
                all.x = TRUE, by = periods, sort = FALSE, allow.cartesian = TRUE)
setnames(frame2, sar, paste0(sar, "_2"))

dat_ids <- merge(periods1, periods2, by = "ids", all = TRUE)
setnames(dat_ids, names(dat_ids), c("ids", paste0(periods, "_", 1:2)))

frame <- merge(frame1, frame2, by = c("ids", PSU), all = TRUE)
rm(list = c(paste0("periods", 1:2), paste0("frame", 1:2)))
setnafill(frame, type = "const", fill = 0,
          cols = c(paste0(c(in_sample, in_frame), "_1"),
                   paste0(c(in_sample, in_frame), "_2")))

frame[, D_l := as.numeric(get(paste0(in_frame, "_1")) == 1 & get(paste0(in_frame, "_2")) == 0)]
frame[, P_hl := as.numeric(get(paste0(in_frame, "_1")) == 1 & get(paste0(in_frame, "_2")) == 1)]
frame[, B_l := as.numeric(get(paste0(in_frame, "_1")) == 0 & get(paste0(in_frame, "_2")) == 1)]

frame[, g_hl := as.numeric(get(paste0(in_sample, "_1")) == 1 & get(paste0(in_sample, "_2")) == 1)]
frame[, sa1_hl := as.numeric(P_hl == 1 & get(paste0(in_sample, "_1")) == 1)]
frame[, sa2_hl := as.numeric(P_hl == 1 & get(paste0(in_sample, "_2")) == 1)]
frame[, fa1_hl := as.numeric(P_hl == 1 & get(paste0(in_frame, "_1")) == 1)]
frame[, fa2_hl := as.numeric(P_hl == 1 & get(paste0(in_frame, "_2")) == 1)]

frame[, `:=`(fGhl_sum = .N, sghl_sum = sum(g_hl),
             sa1hl_sum = sum(sa1_hl), sa2hl_sum = sum(sa2_hl),
             fa1hl_sum = sum(fa1_hl), fa2hl_sum = sum(fa2_hl),
             Dl_sum = sum(D_l), Bl_sum = sum(B_l)),
             keyby = c("ids", paste0(H, "_", 1:2))]

frame[, pop1 := .N, by = c("ids", paste0(H, "_", 1))]
frame[, pop2 := .N, by = c("ids", paste0(H, "_", 2))]
frame[, nh1 := sum(get(paste0(in_sample, "_1"))), by = c("ids", paste0(H, "_", 1))]
frame[, nh2 := sum(get(paste0(in_sample, "_2"))), by = c("ids", paste0(H, "_", 2))]

frame[sa1hl_sum >= 1 & sa2hl_sum >= 1 & sghl_sum >= 1, type := "Type1"]
frame[sa1hl_sum >= 1 & sa2hl_sum >= 1 & sghl_sum == 0, type := "Type3"]
frame[is.na(type) & (sa1hl_sum == 0 | sa2hl_sum == 0) & sghl_sum == 0, type := "Type2"]

sample_data  <- frame[get(paste0(in_sample, "_1")) == 1 | get(paste0(in_sample, "_2")) == 1]
sample_data[P_hl > 0, ap_hl := sa1hl_sum * sa2hl_sum / sghl_sum ]
sample_data[type == "Type1", pop_q := fGhl_sum]
sample_data[type == "Type1", n_q := as.numeric(ap_hl)]
sample_data[type == "Type1", ind_1 := get(paste0(w_final, "_1")) * ap_hl / fGhl_sum]
sample_data[type == "Type1", ind_2 := get(paste0(w_final, "_2")) * ap_hl / fGhl_sum]

sample_data[type == "Type2" & sa1hl_sum > 0 & sa2hl_sum == 0, pop_q := pop1]
sample_data[type == "Type2" & sa1hl_sum == 0 & sa2hl_sum > 0, pop_q := pop2]
sample_data[type == "Type2" & sa1hl_sum > 0 & sa2hl_sum == 0, n_q := as.numeric(nh1)]
sample_data[type == "Type2" & sa1hl_sum == 0 & sa2hl_sum > 0, n_q := as.numeric(nh2)]

sample_data[type == "Type2", ind_1 := 1]
sample_data[type == "Type2", ind_2 := 1]

sample_data[type == "Type3", pop_q := fGhl_sum]
sample_data[type == "Type3", n_q := as.numeric(fGhl_sum)]
sample_data[type == "Type3", ind_1 := get(paste0(w_final, "_1"))]
sample_data[type == "Type3", ind_2 := get(paste0(w_final, "_2"))]

sample_data[D_l == 1, pop_q := pop1] 
sample_data[D_l == 1, n_q := as.numeric(nh1)]
sample_data[D_l == 1, ind_1 := 1]
sample_data[D_l == 1, ind_2 := 0]

sample_data[B_l == 1, pop_q := pop2]
sample_data[B_l == 1, n_q := as.numeric(nh2)]
sample_data[B_l == 1, ind_1 := 0]
sample_data[B_l == 1, ind_2 := 1]

rm(list = c("frame", "dataset"))

setnafill(sample_data, type = "const", fill = 0,
          cols = c(paste0(Yvars, "_1"), paste0(Yvars, "_2")))


aggr1 <- sample_data[, lapply(Yvars, function(x) { sum(get(paste0(x, "_1")) * get(paste0(x, "_2")) * ind_1 * ind_2  * in_sample_1 * in_sample_2)}), 
                     keyby = c("ids", paste0(H, "_", 1:2), "pop_q", "n_q")]
setnames(aggr1, paste0("V", 1:length(Yvars)), paste0(Yvars, "d1"))
aggr2 <- sample_data[, lapply(Yvars, function(x) { sum(get(paste0(x, "_1")) * ind_1 * in_sample_1) * sum(get(paste0(x, "_2")) * ind_2 * in_sample_2)}), 
                     keyby = c("ids", paste0(H, "_", 1:2), "pop_q", "n_q")]
setnames(aggr2, paste0("V", 1:length(Yvars)), paste0(Yvars, "d2"))

aggr1 <- merge(aggr1, aggr2, all = TRUE)
aggr1[, kor := as.numeric(pop_q * (pop_q - n_q) / (n_q * (n_q - 1)))]
aggr1[pop_q == n_q | n_q < 2, kor := 0]

apgr200 <- copy(aggr1)

aggr2 <- aggr1[, lapply(Yvars, function(x) { sum(kor * (get(paste0(x, "d1")) - 1 / n_q * get(paste0(x, "d2"))))}), 
               by = c("ids")]

setnames(aggr2, paste0("V", 1:length(Yvars)), Yvars)

aggr2 <- melt(aggr2, id.vars = c("ids"), variable.name = "variable_new")
aggr2 <- merge(dat_ids, aggr2, by = "ids", all.y = TRUE)
rm(list = c("dat_ids", "aggr1", "PSU", "w_final", "sar", "Yvars", "H"))

setnames(aggr2, "value", "covv")

calc2 <- copy(calc)
if (!is.null(Dom)) calc2[, (paste0(Dom, "at1at")) := lapply(Dom, function(x) paste(x, get(x), sep = "."))]
vDom <- calc2[, "variable", with = FALSE]
if (!is.null(Dom)) vDom <- calc2[, c("variable", paste0(Dom, "at1at")), with = FALSE]

calc2$variable_new <- do.call("paste", c(as.list(vDom), sep = "__"))
calc2[, variable_new := str_replace_all(variable_new, "[ ]", ".")]
if (!is.null(Dom)) calc2[, (paste0(Dom, "at1at")) := NULL]

vsars <- names(calc2)[which(outvars == periods):(ncol(calc2)-1)]
setnames(calc2, vsars, paste0(vsars, "_1"))

all_result <- merge(aggr2, calc2, by = c("variable_new", paste0(periods, "_1")), all.x = TRUE)
setnames(calc2, paste0(vsars, "_1"), paste0(vsars, "_2"))
vsars2 <- c("variable", Dom)
all_result <- merge(all_result, calc2, by = c("variable_new", vsars2, paste0(periods, "_2")), all.x = TRUE)
all_result[, estim := estim_2 / estim_1 * percentratio]
all_result[, rse2 := var_1 / (estim_1) ^ 2 + var_2 / (estim_2) ^ 2 - 2 * covv / (estim_1 * estim_2)]
all_result[, var := estim * rse2 * percentratio ^ 2]
all_result[var >= 0, se := sqrt(var)]

if ( correction==FALSE){ 
  all_result[rse2 > 0, rse := sqrt(rse2)]
  all_result[, cv := 100 * rse]
  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, CI_lower := estim - tsad * se]
  all_result[, CI_upper := estim + tsad * se]
  
  sars <- c(Dom, "variable")
  all_result <- all_result[, c(paste0(periods, "_", 1:2), sars,
                               paste0("estim_", 1:2), "estim",
                               paste0("var_", 1:2), "covv", "var", "se",
                               "cv", "CI_lower", "CI_upper"), with = FALSE]
  return(list(vardom_results = calc3,
              all_result = all_result[]))
}else{ 
  all_result[,index_correct:=ifelse(var<0,1,0)]#
  all_result[rse2<0, rse2:=var_1 / (estim_1) ^ 2 + var_2 / (estim_2) ^ 2]#
  all_result[rse2 > 0, rse := sqrt(rse2)]
  
  all_result[, var := estim * rse2 * percentratio ^ 2]#
  all_result[var >= 0, se := sqrt(var)]#
  all_result[, cv := 100 * rse]
  
  
  
  tsad <- qnorm(0.5 * (1 + confidence))
  all_result[, CI_lower := estim - tsad * se]
  all_result[, CI_upper := estim + tsad * se]
  
  sars <- c(Dom, "variable")
  all_result <- all_result[, c(paste0(periods, "_", 1:2), sars,
                               paste0("estim_", 1:2), "estim",
                               paste0("var_", 1:2), "covv", "var", "se",
                               "cv", "CI_lower", "CI_upper","index_correct"), with = FALSE]
  correction <- all_result[index_correct==1]
  all_result <- all_result[,-"index_correct"]
  correction <- correction[,-"index_correct"]
  return(list(vardom_results = calc3,
              all_result = all_result[], correction= correction))
  }
}
