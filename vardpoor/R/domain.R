#' Extra variables for domain estimation
#'
#' @description The function computes extra variables for domain estimation. Each unique \code{D} row defines a domain. Extra variables are computed for each \code{Y} variable.
#' 
#' 
#' @param Y Matrix of study variables. Any object convertible to \code{data.table} with numeric values, \code{NA} values are not allowed. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param D Matrix of domain variables. Any object convertible to \code{data.table}. The number of rows of \code{D} must match the number of rows of \code{Y}. Duplicated names are not allowed. Object convertible to \code{data.table} or variable names as character, column numbers.
#' @param dataset Optional survey data object convertible to \code{data.table}.
#' @param checking Optional variable if this variable is TRUE, then function checks data preparation errors, otherwise not checked. This variable by default is TRUE.
#' 
#' @return Numeric \code{data.table} containing extra variables for domain estimation.
#' 
#' @references
#' Carl-Erik Sarndal, Bengt Swensson, Jan Wretman. Model Assisted Survey Sampling. Springer-Verlag, 1992, p.70.
#' 
#' @seealso \code{\link{vardom}}, \code{\link{vardomh}}
#' 
#' @keywords surveysampling
#' @examples
#' 
#' ### Example 0
#'  
#' domain(Y = 1, D = "A")
#'  
#'   
#' ### Example 1
#'
#' Y1 <- as.matrix(1 : 10)
#' colnames(Y1) <- "Y1"
#' D1 <- as.matrix(rep(1, 10))
#' colnames(D1) <- "D1"
#' domain(Y = Y1, D = D1)
#'   
#' ### Example 2
#' Y <- matrix(1 : 20, 10, 2)
#' colnames(Y) <- paste0("Y", 1 : 2)
#' D <- matrix(rep(1 : 2, each = 5), 10, 1)
#' colnames(D) <- "D"
#' domain(Y, D)
#' 
#' ### Example 3
#' Y <- matrix(1 : 20, 10, 2)
#' colnames(Y) <- paste0("Y", 1 : 2)
#' D <- matrix(rep(1 : 4, each = 5), 10, 2)
#' colnames(D) <- paste0("D", 1 : 2)
#' domain(Y, D)
#'   
#' ### Example 4
#' Y <- matrix(1 : 20, 10, 2)
#' colnames(Y) <- paste0("Y", 1 : 2)
#' D <- matrix(c(rep(1 : 2, each = 5), rep(3, 10)), 10, 2)
#' colnames(D) <- paste0("D", 1 : 2)
#' domain(Y, D)
#'
#'  
#' @import data.table
#' @import foreach
#' @export domain


domain <- function(Y, D, dataset = NULL, checking = TRUE) {
  if (checking) {
    Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                   check.names = TRUE, isnumeric = TRUE, grepls = "__") }
  Ynrow <- nrow(Y)
  if (checking) {
    D <- check_var(vars = D, varn = "Dom", dataset = dataset,
                   check.names = TRUE, Ynrow = Ynrow, isnumeric = FALSE,
                   ischaracter = TRUE, dif_name = "percoun", grepls = "__") }
  
  Dom_agg <- unique(D)
  setkeyv(Dom_agg, names(Dom_agg))
  i <- k <- NULL
  
  domen <- foreach(i = 1 : ncol(Y), .combine = data.table) %:%
    foreach(k = 1:nrow(Dom_agg), .combine = data.table) %do%
    ifelse(rowSums(D == Dom_agg[k, ][rep(1, Ynrow), ]) == ncol(D), Y[[i]], 0)
  
  domen <- data.table(domen, check.names = TRUE)
  setnames(domen, names(domen), namesD(Y = Y, D = D, uniqueD = TRUE))
  domen <- data.table(domen, check.names = TRUE)
  return(domen[])
}

namesD <- function(Y, D, uniqueD = TRUE) {
  if (uniqueD) {Dom_agg <- unique(D)
  } else Dom_agg <- D
  setkeyv(Dom_agg, names(Dom_agg))
  h <- vector(mode = "character", length = nrow(Dom_agg))
  for (i in 1:nrow(Dom_agg)) {
    cc <- paste(names(D), Dom_agg[i, ], sep = ".")
    h[i] <- paste(cc, collapse = "__")
  }
  foreach(i = 1:ncol(Y), .combine = c) %do% paste(names(Y)[i], h, sep = "__")
}

check_var <- function(vars, varn, varntype = NULL, dataset,
                      check.names = FALSE, ncols = 0, Yncol = 0,
                      Ynrow = 0, Xnrow = 0, isnumeric = FALSE,
                      ischaracter = FALSE, mustbedefined = TRUE,
                      isvector = FALSE, grepls = NULL, dif_name = "",
                      namesID1 = "namesid1", duplicatednames = FALSE,
                      withperiod = TRUE, varnout = NULL, varname = NULL,
                      PSUs = NULL, country = NULL, countryX = NULL,
                      years = NULL, Domen = NULL, yearsX = NULL,
                      periods = NULL, periodsX = NULL, ID_level1 = NULL,
                      dX = "", use.gender = FALSE, kern_method = "gaussian"){
  
  N <- NULL
  if (varn %in%  c("g", "q") & (is.null(class(vars)) | any(class(vars) == "function"))) stop("'g' must be numeric", call. = FALSE)
  
  if (is.null(vars)) {
    if (Xnrow > 0 & varn %in% c("q", "ind_gr")) { vars <- rep(1, Xnrow)
    dataset <- NULL}
    if (Ynrow > 0 & varn == "id") { vars <- 1:Ynrow
    dataset <- NULL}}
  
  if (!is.null(varntype)) {
    if (varntype == "kern_method")  if (length(vars) != 1 | !any(vars %in% c("gaussian", "smooth_splines"))) stop("'kern_method' must be gaussian or smooth_splines", call. = FALSE)
    if (varntype == "pinteger" & (varn %in% c("boots_count", "percentratio") | (varn %in%  c("r", "h_breaks") & kern_method == "smooth_splines"))) {
      if (length(vars) != 1 | any(!is.numeric(vars) | vars < 1)) stop(paste0("'", varn, "' must be a positive integer"), call. = FALSE)
      if (all(is.numeric(vars))) if (vars %% 1 != 0) stop(paste0("'", varn, "' must be a positive integer"), call. = FALSE)
    }
    
    if (varntype == "logical") if (length(vars) != 1 | !any(is.logical(vars))) stop(paste0("'", varn, "' must be logical"), call. = FALSE)
    if (kern_method == "smooth_splines") {
      if (varntype == "numeric01") if (length(vars) != 1 | any(!is.numeric(vars) |  vars < 0 | vars > 1)) {
        stop(paste0("'", varn, "' must be a numeric value in [0, 1]"), call. = FALSE)  }
    } else if (kern_method == "gaussian" & any(varn %in% c("ro", "r"))) vars <- NULL
    if (varntype == "integer0100") {
      if (length(vars) != 1 | any(!is.integer(vars) |  vars < 0 | vars > 100)) {
        stop(paste0("'", varn, "' must be a integer value in [0, 100]"), call. = FALSE)  }}
    
    if (varntype == "numeric0100") {
      if ((length(vars) != 1 & varn != "k") | any(!is.numeric(vars) |  vars < 0 | vars > 100)) {
        stop(paste0("'", varn, "' must be a numeric value", ifelse(varn != "k", "s", ""), " in [0, 100]"), call. = FALSE)  }}
    if (varntype == "change_type") if (length(vars) != 1 | any(!(vars %in% c("absolute", "relative")))) {
      stop("'change_type' must be 'absolute' or 'relative'", call. = FALSE)  }
    if (varntype == "method") if (length(vars) != 1 | any(!(vars %in% c("cros", "netchanges")))) {
      stop("'method' must be 'cros' or 'netchanges'", call. = FALSE)  }
  }
  
  if ((!is.null(vars) & is.null(varntype)) | any(varn %in% c("alpha", "percentratio", "percentage",
                                                             "order_quant", "kern_method", "h_breaks",
                                                             "boots_count", "dh", "method", "use.estVar",
                                                             "use.gender",  "confidence", "frate",
                                                             "change_type", "linratio", "outp_lin", 
                                                             "outp_res", "netchanges", "withperiod",
                                                             "ID_level1_max", "fh_zero", "PSU_level"))
      | (any(varn %in% c("ro", "r")) & (kern_method == "gaussian" | !is.null(vars))))  mustbedefined <- FALSE
  if (!is.null(vars) & is.null(varntype)) {
    if (!withperiod & varn == "period") stop(paste0("'period' must be NULL for those data"), call. = FALSE)
    if(!is.null(dataset)) {
      dataset <- data.table(dataset)
      if (min(vars %in% names(dataset)) != 1) stop(paste0("'", varn, "' does not exist in 'dataset", dX, "'!"), call. = FALSE)
      if (min(vars %in% names(dataset)) == 1)  vars <- dataset[, vars, with = FALSE]}
    
    vars <- data.table(vars, check.names = check.names)
    mkvars <- make.names(rep("vars", length(vars)), unique = TRUE)
    mkvarn <- make.names(rep(varn, length(vars)), unique = TRUE)
    if (all(names(vars) == mkvars)) setnames(vars, mkvars, mkvarn)
    if (ischaracter) vars[, (names(vars)) := lapply(.SD, as.character)]
    if (anyNA(vars)) stop(paste0("'", varn, "' has missing values"), call. = FALSE)
    if (Ynrow > 0) if (nrow(vars) != Ynrow) stop(paste0("'", varn, "' length must be equal with 'Y' row count"), call. = FALSE)
    if (Xnrow > 0) if (nrow(vars) != Xnrow) stop(paste0("'", varn, "' length must be equal with 'X' row count"), call. = FALSE)
    if (Yncol > 0) if (ncol(vars) != Yncol) stop(paste0("'", varn, "' length must be equal with 'Y' column count"), call. = FALSE)
    if (ncols > 0) if (ncol(vars) != ncols) stop(paste0("'", varn, "' must be ", ncols, " column data.frame, matrix, data.table"), call. = FALSE)
    if (isnumeric) if(!all(sapply(vars, is.numeric))) stop(paste0("'", varn, "' must be numeric"), call. = FALSE)
    if (!is.null(grepls)) if (any(grepl(grepls, names(vars)))) stop(paste0("'", varn, "' is not allowed column names with '", grepls, "'"), call. = FALSE)
    if (any(names(vars) %in% dif_name)) stop(paste0("'", varn, "' must be different name"), call. = FALSE)
    if (any(names(vars) == namesID1)) setnames(vars, names(vars), paste0(names(vars), "_", varn))
    
    if (use.gender & varn %in% c("years", "yearsX")){
      parb <- unique(substring(vars[[1]], nchar(vars[[1]])-1, nchar(vars[[1]])))
      if (!all(parb %in% c("_1", "_2")) | length(parb) != 2) {
        stop(paste0("'", varn, "' must be ended with '_1' and '_2'"), call. = FALSE) }}
    
    if (duplicatednames == TRUE & !is.null(vars)) {
      if (any(duplicated(names(vars))))
        stop(paste0("'", varn, "' are duplicate column names: "),
             paste(names(vars)[duplicated(names(vars))], collapse = ","), call. = FALSE) }
    if (ncols == 1 & isvector) vars <- vars[[names(vars)]]
    if (any(vars == 0) & varn == "g") stop("'g' value can not be 0", call. = FALSE)
    if (varn == "q") if (any(is.infinite(vars))) stop("'q' value can not be infinite", call. = FALSE)
    
    varns <- c(switch(as.integer(!is.null(country)) + 1, NULL, "country"),
               switch(as.integer(!is.null(years)) + 1, NULL, "years"),
               switch(as.integer(!is.null(periods) & varn != "yearX") + 1, NULL,
                      paste0(ifelse(varn %in% c("ID_level2", "subperiodsX", "X_ID_level1"), "sub", ""), "periods")))
    
    if (varn %in% c("id", "ID_level2")) {
      dd <- vars
      if (!is.null(years)) dd <- data.table(years, dd)
      if (!is.null(periods)) dd <- data.table(periods, dd)
      if (!is.null(country)) dd <- data.table(country, dd)
      dd <- nrow(dd[, .N, by = names(dd)][N > 1]) > 0
      if (dd) stop(paste0("'", varn, "' by ", paste(varns, collapse = ", "), " are duplicate values"), call. = FALSE)
    }
    
    if (varn %in% c("year1", "year2")) {
      setnames(vars, names(vars), names(years))
      if (anyNA(merge(vars, years, all.x = TRUE,
                      by = names(periods), allow.cartesian = TRUE)))
        stop(paste0("'", varn, "' row must be exist in 'years'"), call. = FALSE)}
    
    if (varn %in% c("period1", "period2")) {
      setnames(vars, names(vars), names(periods))
      if (anyNA(merge(vars, periods, all.x = TRUE,
                      by = names(periods), allow.cartesian = TRUE)))
        stop(paste0("'", varn, "' row must be exist in 'period'"), call. = FALSE)}
    
    if (varn == "PSU_sort") {
      psuag <- data.table(vars, PSUs)
      if (!is.null(periods)) psuag <- data.table(periods, psuag)
      psuag <- psuag[, .N, by = names(psuag)][, N := NULL]
      psuag <- rbindlist(list(psuag[, .N, by = c(names(periods), names(PSUs))],
                              psuag[, .N, by = c(names(periods), "vars")]),
                         use.names = TRUE, fill = TRUE)
      if (nrow(psuag[N > 1]) > 0) stop("'PSU_sort' must be equal for each 'PSU'", call. = FALSE)}
    
    if (varn == "gender") {
      if (length(unique(vars)) != 2) stop("'gender' must be exactly two values", call. = FALSE)
      if (!all(vars %in% 1:2)) stop("'gender' must be value 1 for male, 2 for females", call. = FALSE) }
    
    if (varn %in% c("countryX", "periodX", "yearsX", "subperiodsX", "X_ID_level1")) {
      if (names(vars) != varname) stop(paste0("'", varn, "' must be equal with '", varnout,"' names"), call. = FALSE)
      ncolvars <- ifelse(is.null(vars), 0, ncol(vars))
      if (ncolvars != length(varname)) stop(paste0("'", varn, "' length must be equal with '",varnout,"' row count"), call. = FALSE)
    }
    
    if (varn == "subperiods") {
      subn <- data.table(Domen, years, vars)
      subn <- subn[, .N, by = names(subn)]
      griez <- c(names(years), names(Domen))
      subn <- subn[, .N, by = griez][["N"]]
      griez <- NULL
      if (!is.null(Domen)) griez <- "'Dom', "
      if (any(max(subn) != subn)) stop(paste0(griez, "'years', 'subperiods' must be ", max(subn)), call. = FALSE)
    }
    
    if (varn == "countryX") {
      varsX <- vars[, .N, keyby = names(vars)][, N := NULL]
      country <- country[, .N, keyby = names(country)][, N := NULL]
      if (!identical(country, varsX)) stop("'unique(country)' and 'unique(countryX)' records have different", call. = FALSE)
    }
    
    if (varn %in% c("periodX", "yearsX", "subperiodsX", "X_ID_level1")) {
      periX <- copy(vars)
      if (!is.null(periodsX)) periX <- data.table(periodsX, periX)
      if (!is.null(yearsX)) periX <- data.table(yearsX, periX)
      if (!is.null(countryX)) periX <- data.table(countryX, periX)
      
      peri <- NULL
      if (!is.null(ID_level1)) peri <- ID_level1
      if (!is.null(periods)) peri <- switch(as.integer(!is.null(peri)) + 1, data.table(periods), data.table(periods, peri))
      if (!is.null(years)) peri <- switch(as.integer(!is.null(peri)) + 1, data.table(years), data.table(years, peri))
      if (!is.null(country)) peri <- switch(as.integer(!is.null(peri)) + 1, data.table(country), data.table(country, peri))
      
      peri <- peri[, .N, keyby = names(peri)][, N := NULL]
      periX <- periX[, .N, keyby = names(periX)]
      varnsX <- paste0(varns, "X")
      
      if (varn == "X_ID_level1") {
        varns <- c(varns, "ID_level1")
        varnsX <- c(varnsX, "X_ID_level1")
        if (nrow(periX[N > 1]) > 0) stop("'X_ID_level1' have duplicates", call. = FALSE) }
      periX[, N := NULL]
      
      if (!identical(peri, periX)) {
        stop(paste0("'unique(", paste(varns, collapse = ", "), ")' and 'unique(",
                    paste(varnsX, collapse = ", "), ")' records have different"), call. = FALSE)
      }
      peri <- periX <- NULL
    }
    
  } else if (mustbedefined) stop(paste0("'", varn, "' must be defined!"), call. = FALSE)
  if (is.data.table(vars)) vars <- vars[]
  return(vars)
}

