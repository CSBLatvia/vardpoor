#' Variance estimation for sample surveys by the new stratification
#' 
#' @description Computes s2g and the variance estimation by the new stratification.
#' 
#' @param Y Variables of interest. Object convertible to \code{data.table} or variable names as character, column numbers or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
#' @param H The unit stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
#' @param H2 The unit new stratum variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
#' @param w_final Weight variable. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
#' @param N_h optional; either a \code{data.frame} giving the first column - stratum, but the second column - the total of the population in each stratum.
#' @param N_h2 optional; either a \code{data.frame} giving the first column - new stratum, but the second column - the total of the population in each new stratum.
#' @param period Optional variable for the survey periods. If supplied, the values for each period are computed independently. One dimensional object convertible to one-column \code{data.table} or variable name as character, column number or logical vector with only one \code{TRUE} value (length of the vector has to be the same as the column count of \code{dataset}).
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' @return A list with objects are returned by the function:
#'   \itemize{
#'       \item betas A numeric \code{data.table} containing the estimated coeffients of calibration.
#'       \item s2g A \code{data.table} containing the s^2g value.        
#'       \item var_est A \code{data.table} containing the values of the variance estimation.
#'   }
#'   
#'   
#' @details
#' 
#' It is possible to compute population size \eqn{M_g} from sampling frame. The standard deviation of \eqn{g}-th stratum is
#' \deqn{S_g^2 =\frac{1}{M_g-1} \sum\limits_{k=1}^{M_g} \left(y_{gk}-\bar{Y}_g \right)^2= \frac{1}{M_g-1} \sum\limits_{k=1}^{M_g} y_{gk}^2 - \frac{M_g}{M_g-1}\bar{Y}_g^2}{S_g^2 =1/(M_g-1) \sum k=1...M_g (y_gk - Ym_g)^2= 1/(M_g-1) \sum k=1...M_g  (y_gk)^2 - M_g/(M_g-1)*(Ym_g)^2}
#'
#'\eqn{\sum\limits_{k=1}^{M_g} y_{gk} ^2}{\sum k=1...M_g (y_gk)^2} and \eqn{\bar{Y}_g^2}{Ym_g^2} have to be estimeted to estimate \eqn{S_g^2}. Estimate of \eqn{\sum\limits_{k=1}^{M_g} y_{gk}^2}{\sum k=1...M_g (y_gk)^2} is \eqn{\sum\limits_{h=1}^{H} \frac{N_h}{n_h} \sum\limits_{i=1}^{n_h} y_{gi}^2 z_{hi}}{\sum h=1...H N_h/n_h \sum i=1...n_h (y_gi)^2*z_hi}, where
#'
#'\eqn{ z_{hi} = \left\{
#'  \begin{array}{ll}
#'  0, & h_i \notin \theta_g \\
#'  1, & h_i \in \theta_g
#'  \end{array}
#'  \right. }{z_hi=if(0, h_i notin  \theta_g; 1, h_i in \theta_g)}
#', \eqn{\theta_g}{\theta_g}  is the index group of successfully surveyed units belonging to \eqn{g}{g}-th stratum. #'Estimate of \eqn{\bar{Y}_g^2}{(Y_g)^2}
#'is 
#'
#'\deqn{\hat{\bar{Y}}_g^2=\left( \hat{\bar{Y}}_g \right)^2-\hat{Var} \left(\hat{\bar{Y}} \right)}{Ym_g^2=(Ym_g)^2- Var(Ym)}
#'
#'
#'
#'\deqn{\hat{\bar{Y}}_g =\frac{\hat{Y}_g}{M_g}= \frac{1}{M_g} \sum\limits_{h=1}^{H} \frac{N_h}{n_h} \sum\limits_{i=1}^{n_h} y_{hi} z_{hi}}{Ym_g =Ym_g/M_g= 1/M_g \sum h=1...H N_h/n_h \sum i=1...n_h  y_hi z_hi}
#'
#'
#'So the estimate of \eqn{S_g^2} is
#'
#'\eqn{s_g^2=\frac{1}{M_g-1} \sum\limits_{h=1}^{H} \frac{N_h}{n_h} \sum\limits_{i=1}^{n_h} y_{hi}^2 z_{hi} -}{s_g^2=\1/(M_g-1) \sum h=1...H  N_h/n_h \sum i=1...n_h (y_hi)^2 * z_hi -}
#'
#'
#'\eqn{-\frac{M_g}{M_g-1} \left( \left( \frac{1}{M_g} \sum\limits_{h=1}^{H} \frac{N_h}{n_h} \sum\limits_{i=1}^{n_h} y_{hi} z_{hi} \right)^2 - \frac{1}{M_g^2} \sum\limits_{h=1}^{H} N_h^2 \left(\frac{1}{n_h} - \frac{1}{N_h}\right) \frac{1}{n_h-1} \sum\limits_{i=1}^{n_h} \left(y_{hi} z_{hi} - \frac{1}{n_h} \sum\limits_{t=1}^{n_h} y_{ht} z_{ht} \right)^2 \right)}{-M_g/(M_g-1) (1/M_g \sum h=1...H N_h/n_h \sum i=1...n_h y_hi z_hi)^2}
#'
#'
#'Two conditions have to realize to estimate \eqn{S_g^2: n_h>1, \forall g}{S_g^2: n_h>1, forall g} and \eqn{\theta_g \ne 0, \forall g.}{\theta_g <> 0, forall g.}
#'
#'Variance of \eqn{\hat{Y}}{Y} is
#'\deqn{ Var\left( \hat{Y} \right) = \sum\limits_{g=1}^{G} M_g^2 \left( \frac{1}{m_g} - \frac{1}{M_g} \right) S_g^2 }{Var(Y) = \sum g=1...G M_g^2 (1/m_g - 1/M_g)*(S_g)^2 }
#'
#'
#'Estimate of \eqn{\hat{Var}\left( \hat{Y} \right)}{Var(Y)} is
#'
#'\deqn{\hat{Var}\left( \hat{Y} \right) = \sum\limits_{g=1}^{G} M_g^2 \left( \frac{1}{m_g} - \frac{1}{M_g} \right)s_g^2}{Var(Y)= \sum g=1...G M_g^2 (1/m_g - 1/M_g)*(s_g)^2}
#'
#'  
#' @references 
#' M. Liberts. (2004) Non-response Analysis and Bias Estimation in a Survey on Transportation of Goods by Road.
#'
#' @seealso
#'    \code{\link{domain}},         \code{\link{lin.ratio}},    \code{\link{linarpr}},
#'    \code{\link{linarpt}},        \code{\link{lingini}},      \code{\link{lingini2}},
#'    \code{\link{lingpg}},         \code{\link{linpoormed}},   \code{\link{linqsr}},
#'    \code{\link{linrmpg}},        \code{\link{residual_est}}, \code{\link{vardom}},
#'    \code{\link{vardom_othstr}},  \code{\link{vardomh}},      \code{\link{varpoord}}
#'    
#' @keywords vardpoor

#' @examples
#' library("data.table")
#' Y <- data.table(matrix(runif(50) * 5, ncol = 5))
#'    
#' H <- data.table(H = as.integer(trunc(5 * runif(10))))
#' H2 <- data.table(H2 = as.integer(trunc(3 * runif(10))))
#'    
#' N_h <- data.table(matrix(0 : 4, 5, 1))
#' setnames(N_h, names(N_h), "H")
#' N_h[, sk:= 10]
#'    
#' N_h2 <- data.table(matrix(0 : 2, 3, 1))
#' setnames(N_h2, names(N_h2), "H2")
#' N_h2[, sk2:= 4]
#'    
#' w_final <- rep(2, 10)
#'    
#' vo <- variance_othstr(Y = Y, H = H, H2 = H2,
#'                       w_final = w_final,
#'                       N_h = N_h, N_h2 = N_h2,
#'                       period = NULL,
#'                       dataset = NULL)
#' vo
#'
#' @import data.table
#' 
#' @export variance_othstr




variance_othstr <- function(Y, H, H2, w_final, N_h = NULL, N_h2, period = NULL, dataset = NULL, checking = TRUE) {
  . <- NULL
  ### Checking
  if (checking) {   
    Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                   check.names = TRUE, isnumeric = TRUE)
    Ynrow <- nrow(Y)
    Yncol <- ncol(Y)
    
    H <- check_var(vars = H, varn = "H", dataset = dataset,
                   ncols = 1, isnumeric = FALSE, ischaracter = TRUE)
    
    H2 <- check_var(vars = H2, varn = "H2", dataset = dataset,
                    ncols = 1, Ynrow = Ynrow, isnumeric = FALSE,
                    ischaracter = TRUE, dif_name = names(H))
    
    w_final <- check_var(vars = w_final, varn = "w_final",
                         dataset = dataset, ncols = 1, Ynrow = Ynrow,
                         isnumeric = TRUE, isvector = TRUE)
    
    period <- check_var(vars = period, varn = "period",
                        dataset = dataset, Ynrow = Ynrow,
                        ischaracter = TRUE, mustbedefined = FALSE,
                        duplicatednames = TRUE)    
  }
  
  np <- sum(ncol(period))
  
  # N_h
  if (!is.null(N_h)) {
      N_h <- data.table(N_h)
      if (anyNA(N_h)) stop("'N_h' has missing values")
      if (ncol(N_h) != np + 2) stop(paste0("'N_h' should be ", np + 2, " columns"))
      if (!is.numeric(N_h[[ncol(N_h)]])) stop("The last column of 'N_h' should be numerical")
       
      nams <- c(names(period), names(H))
      if (all(nams %in% names(N_h))) {N_h[, (nams) := lapply(.SD, as.character), .SDcols = nams]
             } else stop(paste0("All strata titles of 'H'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h'"))
   
      if (is.null(period)) {
             if (any(is.na(merge(unique(H), N_h, by = names(H), all.x = TRUE)))) stop("'N_h' is not defined for all strata")
             if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique")
       } else { pH <- data.table(period, H)
                if (any(is.na(merge(unique(pH), N_h, by = names(pH), all.x = TRUE)))) stop("'N_h' is not defined for all strata and periods")
                if (any(duplicated(N_h[, head(names(N_h), -1), with = FALSE]))) stop("Strata values for 'N_h' must be unique in all periods")
                pH <- NULL
              }
      setkeyv(N_h, names(N_h)[c(1 : (1 + np))])
  } else {
    Nh <- data.table(H, w_final)
    if (!is.null(period)) Nh <- data.table(period, Nh)
    N_h <- Nh[, .(N_h = sum(w_final, na.rm = TRUE)), keyby = c(names(Nh)[1 : (1 + np)])]
  }
  Nh1 <- names(N_h)[ncol(N_h)]


  # N_h2
  if (!is.null(N_h2)) {
      N_h2 <- data.table(N_h2)
      if (anyNA(N_h2)) stop("'N_h2' has missing values") 
      if (ncol(N_h2) != np + 2) stop(paste0("'N_h2' should be ", np + 2, " columns"))
      if (!is.numeric(N_h2[[ncol(N_h2)]])) stop("The last column of 'N_h2' should be numerical")

      nams2 <- c(names(period), names(H2))
      if (all(nams2 %in% names(N_h2))) {N_h2[, (nams2) := lapply(.SD, as.character), .SDcols = nams2]
             } else stop(paste0("All strata titles of 'H2'", ifelse(!is.null(period), "and periods titles of 'period'", ""), " have not in 'N_h2'"))
   
      if (is.null(period)) {
             if (names(H2) != names(N_h2)[1]) stop("Strata titles for 'H2' and 'N_h2' is not equal")
             if (any(is.na(merge(unique(H2), N_h2, by = names(H2), all.x = TRUE)))) stop("'N_h2' is not defined for all stratas")
       } else { pH2 <- data.table(period, H2)
                if (any(names(pH2) != names(N_h2)[c(1 : (1 + np))])) stop("Strata titles for 'period' with 'H2' and 'N_h2' is not equal")
                if (any(is.na(merge(unique(pH2), N_h2, by = names(pH2), all.x = TRUE)))) stop("'N_h2' is not defined for all stratas and periods")
                } 
    setkeyv(N_h2, names(N_h2)[c(1 : (1 + np))])
  } else stop ("N_h2 is not defined!")
  Nh2 <- names(N_h2)[ncol(N_h2)]

  ### Calculation
  
  # z_hi
  f_h1 <- .SD <- .N <- NULL
  Ys <- copy(Y)
  Ys[, paste0(names(Y),"_sa") := lapply(Y, function(x) w_final * x^2)]
  Ys[, paste0(names(Y),"_sb") := lapply(Y, function(x) x * w_final)]
  Ys[, paste0(names(Y),"_sc") := lapply(Y, function(x) x ^ 2)]
  Ys[, paste0(names(Y),"_sd") := Y]

  Ys <- data.table(H, H2, Ys)
  if (!is.null(period)) Ys <- data.table(period, Ys)

  # n_h1
  n_h1 <- data.table(H)
  if (!is.null(period))   n_h1 <- data.table(period, n_h1)
  n_h1 <- n_h1[, .(n_h1 = .N), keyby = c(names(n_h1))]

  F_h1 <- merge(N_h, n_h1, keyby = c(names(N_h)[1 : (1 + np)]))
  F_h1[, f_h1 := n_h1 / get(Nh1)]

  if (nrow(F_h1[n_h1 == 1 & f_h1 != 1]) > 0) {
    print("There are strata, where n_h1 == 1 and f_h1 <> 1")
    print("Not possible to estimate the variance in these strata!")
    print("At these strata estimation of variance was not calculated")
    nh1 <- F_h1[n_h1 == 1 & f_h1 != 1]
    print(nh1)
  }

  # n_h2
  n_h2 <- data.table(H2)
  if (!is.null(period)) n_h2 <- data.table(period, n_h2)
  nn_h2 <- names(n_h2)
  n_h2 <- n_h2[, .(n_h2 = .N), keyby = nn_h2]

  F_h2 <- merge(N_h2, n_h2, keyby = nn_h2)
  F_h2[, f_h2 := n_h2 / get(Nh2)]

  if (nrow(F_h2[n_h2 == 1 & f_h2 != 1]) > 0) {
    print("There are strata, where n_h2 == 1 and f_h2 <> 1")
    print("Not possible to estimate the variance in these strata!")
    print("At these strata estimation of variance was not calculated")
    nh2 <- F_h2[n_h2 == 1 & f_h2 != 1]
    print(nh2)
  }
  
  if (nrow(F_h2[f_h2 > 1]) > 0) {    
      print("There are strata, where f_h2 > 1")
      print("At these strata estimation of variance will be 0")
      print(F_h2[f_h2 > 1])
      F_h2[f_h2 > 1, f_h2 := 1]
   }

  z_h_h2 <- Ys[, lapply(.SD, sum, na.rm = TRUE), keyby = c(names(Ys)[1 : (2 + np)]),
                      .SDcols = names(Ys)[-(0 : (ncol(Y) + 2 + np))]]

  z_h_h2 <- merge(z_h_h2, F_h1, keyby = names(z_h_h2)[c(1 : (1 + np))])

  pop <- z_h_h2[[Nh1]]

  z_h_h2[, paste0(names(Y), "_sc") := lapply(.SD[, 
           paste0(names(Y), "_sc"), with = FALSE], function(x)
           x * pop ^ 2 * ( 1 / n_h1 - 1 / pop)/(n_h1 - 1))]

  z_h_h2[, paste0(names(Y), "_sd") := lapply(.SD[,
           paste0(names(Y), "_sd"), with = FALSE], function(x) 
                 (1 / n_h1) * x ^ 2  * pop ^ 2 * (1 / n_h1 - 1 / pop)/(n_h1 - 1))]

  z_h_h2[n_h1 == 1, paste0(names(Y), "_sc") := NA]
  z_h_h2[n_h1 == 1, paste0(names(Y), "_sd") := NA]

  nameszh2 <- names(H2)
  if (!is.null(period)) nameszh2 <- c(names(period), nameszh2)
  
  zh2 <- z_h_h2[, lapply(.SD, sum, na.rm = TRUE), keyby = nameszh2,
                      .SDcols = names(z_h_h2)[-(1 : (2 + np))]] 

  zh2 <- merge(zh2, F_h2, by = nn_h2)
  pop2 <- zh2[[names(N_h2)[ncol(N_h2)]]]
  nh2 <- zh2[["n_h2"]]
  f_h2 <- zh2[["f_h2"]]

  # s2
  s2_g <- zh2[, mapply(function(sa, sb, sc, sd) sa / (pop2 - 1) - pop2 / (pop2 - 1) * ((sb / pop2)^2 - (sc - sd) / pop2^2),
              zh2[, paste0(names(Y), "_sa"), with = FALSE], 
              zh2[, paste0(names(Y), "_sb"), with = FALSE],
              zh2[, paste0(names(Y), "_sc"), with = FALSE],
              zh2[, paste0(names(Y), "_sd"), with = FALSE])]

  # var_g 
  if (is.null(nrow(s2_g))) s2_g <- t(s2_g)
  s2_g <- data.table(s2_g)
  setnames(s2_g, names(s2_g), names(Y))

  s2g <- data.table(zh2[, nn_h2, with = FALSE], s2_g)

  s2_g <- matrix(pop2^2 * 1 / nh2 * (1 - f_h2)) * s2_g

  if (np > 0) s2_g <- data.table(zh2[, names(period), with = FALSE], s2_g)

  # Variance_est
  if (np == 0) {var_est <- data.table(t(colSums(s2_g, na.rm = TRUE)))
             } else var_est <- s2_g[, lapply(.SD, sum, na.rm = TRUE), 
                                              keyby = c(names(s2_g)[c(1 : np)]),
                                             .SDcols = names(Y)]
  list(s2g = s2g,
       var_est = var_est)
}