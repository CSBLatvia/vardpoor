#' Variance estimation for sample surveys by the ultimate cluster method
#'
#' @description Computes the variance estimation by the ultimate cluster method.
#' 
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param PSU Primary sampling unit variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number.
#' @param N_h Number of primary sampling units in population for each stratum (and period if \code{period} is not \code{NULL}). If \code{N_h = NULL} and \code{fh_zero = FALSE} (default), \code{N_h} is estimated from sample data as sum of weights (\code{w_final}) in each stratum (and period if \code{period} is not \code{NULL}).
#' Optional for single-stage sampling design as it will be estimated from sample data. Recommended for multi-stage sampling design as \code{N_h} can not be correctly estimated from the sample data in this case. If \code{N_h} is not used in case of multi-stage sampling design (for example, because this information is not available), it is advisable to set \code{fh_zero = TRUE}.
#' If \code{period} \bold{is} \code{NULL}. A two-column matrix with rows for each stratum. The first column should contain stratum code. The second column - the number of primary sampling units in the population of each stratum.
#' If \code{period} \bold{is not} \code{NULL}. A three-column matrix with rows for each intersection of strata and period. The first column should contain period. The second column should contain stratum code. The third column - the number of primary sampling units in the population of each stratum and period.
#' @param fh_zero by default FALSE; \code{fh} is calculated as division of n_h and N_h in each strata, if TRUE, \code{fh} value is zero in each strata.
#' @param PSU_level by default TRUE; if PSU_level is TRUE, in each strata \code{fh} is calculated as division of count of PSU in sample (n_h) and count of PSU in frame (N_h). if PSU_level is FALSE, in each strata \code{fh} is calculated as division of count of units in sample (n_h) and count of units in frame (N_h), which calculated as sum of weights.
#' @param PSU_sort optional; if PSU_sort is defined, then variance is calculated for systematic sample.
#' @param period Optional variable for the survey periods. If supplied, the values for each period are computed independently. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset an optional name of the individual dataset  \code{data.table}.
#' @param msg an optional printed text, when function print error.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' @return a \code{data.table} containing the  values of the variance estimation by totals.
#' 
#' @details
#'If we assume that \eqn{n_h \geq 2}{n_h>=2} for all \eqn{h}, that is, two or more PSUs are selected from each stratum, then the variance of \eqn{\hat{\theta}}{\theta} can be estimated from the variation among the estimated PSU totals of the variable \eqn{Z}:
#' \deqn{\hat{V} \left(\hat{\theta} \right)=\sum\limits_{h=1}^{H} \left(1-f_h \right) \frac{n_h}{n_{h}-1} \sum\limits_{i=1}^{n_h} \left( z_{hi\bullet}-\bar{z}_{h\bullet\bullet}\right)^2, }{V(\theta)=\sum h=1...H  (1-f_h)*n_h/(n_h-1)* \sum  i=1...n_h ( z_hi.- z_h..)^2, }
#'
#' where
#' \eqn{\bullet}{}
#' \eqn{z_{hi\bullet}=\sum\limits_{j=1}^{m_{hi}} \omega_{hij} z_{hij}}{z_hi.=\sum j=1...m_hi \omega_hij * z_hij}
#' 
#' \eqn{\bullet}{}
#' \eqn{\bar{z}_{h\bullet\bullet}=\frac{\left( \sum\limits_{i=1}^{n_h} z_{hi\bullet} \right)}{n_h}}{z_h..=(\sum i=1...n_h z_hi.)/n_h}
#' 
#' \eqn{\bullet}{}
#' \eqn{f_h} is the sampling fraction of PSUs within stratum
#' 
#' \eqn{\bullet}{}
#' \eqn{h} is the stratum number, with a total of H strata
#' 
#' \eqn{\bullet}{}
#' \eqn{i} is the primary sampling unit (PSU) number within stratum \eqn{h}, with a total of \eqn{n_h} PSUs
#' 
#' \eqn{\bullet}{}
#' \eqn{j} is the household number within cluster \eqn{i} of stratum \eqn{h}, with a total of \eqn{m_{hi}}{m_hi} household
#'
#' \eqn{\bullet}{}
#' \eqn{w_{hij}}{w_hij} is the sampling weight for household \eqn{j} in PSU \eqn{i} of stratum \eqn{h}
#' 
#' \eqn{\bullet}{}
#' \eqn{z_{hij}}{z_hij} denotes the observed value of the analysis variable \eqn{z} for household \eqn{j} in PSU \eqn{i} of stratum \eqn{h}
#' 
#'
#' @references
#' Morris H. Hansen, William N. Hurwitz, William G. Madow, (1953), Sample survey methods and theory Volume I Methods and applications, 257-258, Wiley. \cr
#' Guillaume Osier and Emilio Di Meglio. The linearisation approach implemented by Eurostat for the first wave of EU-SILC: what could be done from the second onwards? 2012 \cr
#' Eurostat Methodologies and Working papers, Standard error estimation for the EU-SILC indicators of poverty and social exclusion, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr 
#' Yves G. Berger, Tim Goedeme, Guillame Osier (2013). Handbook on standard error estimation and other related sampling issues in EU-SILC, URL \url{https://ec.europa.eu/eurostat/cros/content/handbook-standard-error-estimation-and-other-related-sampling-issues-ver-29072013_en} \cr
#' Eurostat Methodologies and Working papers, Handbook on precision requirements and variance estimation for ESS household surveys, 2013, URL \url{https://ec.europa.eu/eurostat/documents/3859598/5927001/KS-RA-13-029-EN.PDF}. \cr
#' 
#' @seealso \code{\link{domain}},   \code{\link{lin.ratio}},    \code{\link{linarpr}},
#'          \code{\link{linarpt}},  \code{\link{lingini}},      \code{\link{lingini2}},
#'          \code{\link{lingpg}},   \code{\link{linpoormed}},   \code{\link{linqsr}},
#'          \code{\link{linrmpg}},  \code{\link{residual_est}}, \code{\link{vardom}},
#'          \code{\link{vardomh}}, \code{\link{varpoord}},     \code{\link{variance_othstr}}
#'          
#' @keywords vardpoor
#' 
#' @examples
#' Ys <- rchisq(10, 3)
#' w <- rep(2, 10)
#' PSU <- 1 : length(Ys)
#' H <- rep("Strata_1", 10)
#' 
#' # by default without using fh_zero (finite population correction)
#' variance_est(Y = Ys, H = H, PSU = PSU, w_final = w)
#' 
#' 
#' \dontrun{
#'  # without using fh_zero (finite population correction)
#'  variance_est(Y = Ys, H = H, PSU = PSU, w_final = w, fh_zero = FALSE)
#'  
#'  # with using fh_zero (finite population correction)
#'  variance_est(Y = Ys, H = H, PSU = PSU, w_final = w, fh_zero = TRUE)
#'  }
#' 
#' @import data.table
#' @export variance_est





variance_est <- function(Y, H, PSU, w_final, N_h = NULL, fh_zero = FALSE,
                         PSU_level = TRUE, PSU_sort = NULL, period = NULL, 
                         dataset = NULL, msg = "", checking = TRUE) {

  ### Checking
  . <- NULL
  if (checking) {
        fh_zero <- check_var(vars = fh_zero, varn = "fh_zero", varntype = "logical") 
        PSU_level <- check_var(vars = PSU_level, varn = "PSU_level", varntype = "logical") 
  
        Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                       check.names = TRUE, isnumeric = TRUE)
        Ynrow <- nrow(Y)
        Yncol <- ncol(Y)
    
        H <- check_var(vars = H, varn = "H", dataset = dataset,
                       ncols = 1, Ynrow = Ynrow, isnumeric = FALSE,
                       ischaracter = TRUE)
    
        w_final <- check_var(vars = w_final, varn = "w_final",
                             dataset = dataset, ncols = 1, Ynrow = Ynrow,
                             isnumeric = TRUE, isvector = TRUE)
        
        period <- check_var(vars = period, varn = "period",
                           dataset = dataset, Ynrow = Ynrow,
                           ischaracter = TRUE, mustbedefined = FALSE,
                           duplicatednames = TRUE)

        PSU <- check_var(vars = PSU, varn = "PSU", dataset = dataset,
                         ncols = 1, Ynrow = Ynrow, ischaracter = TRUE)
     
        PSU_sort <- check_var(vars = PSU_sort, varn = "PSU_sort", dataset = dataset,
                              ncols = 1, Ynrow = Ynrow, ischaracter = TRUE,
                              isvector = TRUE, mustbedefined = FALSE, PSUs = PSU)
    }

  np <- sum(ncol(period))
  vars <- names(period)


  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (anyNA(N_h)) stop("'N_h' has missing values") 
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", toString(np + 2)," columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numeric")
      nams <- c(names(period), names(H))
      if (all(nams %in% names(N_h))) {N_h[, (nams) := lapply(.SD, as.character), .SDcols = nams]
             } else stop(paste0("All strata titles of 'H'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h'"))
   
      if (is.null(period)) {
             if (any(is.na(merge(unique(H), N_h, by = names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
             if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                }
    setnames(N_h, names(N_h)[ncol(N_h)], "N_h")
    setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  } else {
    Nh <- data.table(H, w_final)
    if (!is.null(period)) Nh <- data.table(period, Nh)
    N_h <- Nh[, .(N_h = sum(w_final, na.rm = TRUE)), keyby = c(names(Nh)[1 : (1 + np)])]
  }
  psuag <- pH <- NULL  

  ### Calculation
  namY <- names(Y)

  # z_hi
  ids <- nhc <- f_h <- .SD <- N <- NULL
  hpY <- data.table(H, PSU, Y * w_final)
  if (!is.null(PSU_sort)) hpY <- data.table(H, PSU, PSU_sort, Y * w_final)
  if (!is.null(period)) hpY <- data.table(period, hpY)
  psusn <- as.integer(!is.null(PSU_sort))
  z_hi <- hpY[, lapply(.SD, sum, na.rm = TRUE), 
                       keyby = c(names(hpY)[1 : (2 + np + psusn)]),
                       .SDcols = names(hpY)[-(1 : (2 + np + psusn))]]
  setkeyv(z_hi, names(z_hi)[c(1 : (1 + np))]) 

  # n_h
  n_h <- data.table(z_hi[, c(1 : (1 + np)), with = FALSE])
  n_h <- n_h[, .(n_h = .N), keyby = c(names(n_h)[1 : (1 + np)])]

  # var_z_hi
  var_z_hi <- z_hi[, lapply(.SD, var, na.rm = FALSE), keyby = c(names(z_hi)[1 : (1 + np)]), .SDcols = namY]

  if (!is.null(PSU_sort)) {
       setkeyv(z_hi, c(names(z_hi)[c(1:(1 + np),3 + np)]))
       z_hi[, (paste0("lag_", namY)) := lapply(.SD, function(x) shift(x, 1)),
                               by=c(names(z_hi)[1 : (1 + np)]), .SDcols = namY]


       laY <- paste0("lag_", namY[1])
       z_hi <- z_hi[!is.na(get(laY))]

       var_z_hi <- z_hi[, lapply(namY, function(x) 
                                 sum((get(x) - get(paste0("lag_", x)))^2)),
                                 keyby = c(names(z_hi)[1 : (1 + np)])]
       setnames(var_z_hi, names(var_z_hi)[(2 + np) : ncol(var_z_hi)], namY)
   }

  # f_h
  F_h <- merge(N_h, n_h, by = names(hpY)[c(1 : (1 + np))], sort = TRUE)
  F_h[, N_h := round2(N_h, 8)]
  F_h[, f_h := n_h / N_h]

  if (nrow(F_h[n_h == 1 & f_h != 1]) > 0) {
    print(msg)
    print("There are strata, where n_h == 1 and f_h <> 1")
    print("Not possible to estimate the variance in these strata!")
    print("At these strata estimation of variance was not calculated")
    nh <- F_h[n_h == 1 & f_h != 1]
    print(nh)
  }
  
  if (nrow(F_h[f_h > 1]) > 0) {    
     print(msg)
     print("There are strata, where f_h > 1")
     print("At these strata estimation of variance will be 0")
     print(F_h[f_h > 1])
     F_h[f_h > 1, f_h := 1]
   }

  # fh1
  if (!(PSU_level)) {
       n_h1 <- Nh1 <- NULL
       fh1 <- data.table(hpY[, c(1 : (1 + np)), with = FALSE], w_final)
       fh1 <- fh1[, .(n_h1 = .N, Nh1 = sum(w_final, na.rm = TRUE)), keyby = c(names(fh1)[1 : (1 + np)])]
       F_h <- merge(F_h, fh1, by = c(names(fh1)[1 : (1 + np)]))
       F_h[, f_h := n_h1 / Nh1]
     }

  var_z_hi <- merge(F_h, var_z_hi, by = c(names(F_h)[1 : (1 + np)]))
  fh1 <- F_h <- NULL

  # var_h

  if (!is.null(PSU_sort)) {
         var_z_hi[, nhc := ifelse(n_h > 1, n_h / (2 * (n_h - 1)), NA)]
    } else var_z_hi[, nhc := n_h]

  var_z_hi[, ids := 1 : .N]
  var_z_hi[, (paste0("var_", namY)) := lapply(.SD[, namY, with = FALSE],
                                           function(x) (1 - f_h * (1 - fh_zero)) * nhc * x), by = "ids"]
  # Variance_est 
  
  var_est <- var_z_hi[, lapply(.SD, sum, na.rm = TRUE), 
                                  keyby = vars, .SDcols = paste0("var_", namY)]
  setnames(var_est, paste0("var_", namY), namY)
  return(var_est)
}


round2 <- function(x, n) {
  sign(x) * trunc(abs(x) * 10 ^ n + 0.5) / 10 ^ n
}

