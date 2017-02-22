
check_var <- function(vars, varn, dataset, check.names = FALSE,
                      ncols = NULL, Yncol = 0, Ynrow = 0, Xnrow = 0,
                      isnumeric = FALSE, ischaracter = FALSE,
                      mustbedefined = TRUE, isvector = FALSE,
                      grepls = NULL, dif_name = "", namesID1 = "id",
                      duplicatednames = FALSE, withperiod = TRUE,
                      varnout = NULL, varname = NULL, PSUs = NULL,
                      periods = NULL, periods_varn  = NULL,
                      periodsX = NULL, country = NULL,
                      countryX = NULL, ID_level1 = NULL){

  if (varn %in%  c("g", "q") & (is.null(class(vars)) | any(class(vars) == "function"))) stop("'g' must be numeric", call. = FALSE)
  if (is.null(vars)) {
    if (Xnrow > 0 & varn %in% c("q", "ind_gr", "id")) { vars <- rep(1, Xnrow)
                                                        dataset <- NULL}
    if (Ynrow > 0 & varn == "id") { vars <- rep(1, Ynrow)
                                    dataset <- NULL}}

  if (!is.null(vars)) {
      if (!withperiod & varn == "period") stop(paste0("'period' must be NULL for those data"), call. = FALSE)
      if(!is.null(dataset)) {
        dataset <- data.table(dataset)
        if (min(vars %in% names(dataset)) != 1) stop(paste0("'", varn, "' does not exist in 'dataset'!"), call. = FALSE)
        if (min(vars %in% names(dataset)) == 1)  vars <- dataset[, vars, with = FALSE]}

      vars <- data.table(vars, check.names = check.names)
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

      if (duplicatednames == TRUE & !is.null(vars)) {
        if (any(duplicated(names(vars))))
          stop(paste0("'", varn, "' are duplicate column names: "),
               paste(names(vars)[duplicated(names(vars))], collapse = ","), call. = FALSE) }
      if (ncols == 1 & isvector) vars <- vars[[names(vars)]]
      if (any(vars == 0) & varn == "g") stop("'g' value can not be 0", call. = FALSE)
      if (varn == "q") if (any(is.infinite(vars))) stop("'q' value can not be infinite", call. = FALSE)

      if (varn %in% c("id", "ID_level2")) {
             dd <- vars
             if (!is.null(periods)) dd <- data.table(periods, vars)
             if (!is.null(country)) dd <- data.table(country, vars)
             dd <- any(duplicated(dd, by = names(dd)))
             if (dd) {
               if (!is.null(country)) {
                   if (!is.null(periods)) { stop(paste0("'", varn, "' by period and country are duplicate values"), call. = FALSE)
                         } else   stop(paste0("'", varn, "' by period and country are duplicate values"), call. = FALSE)
                 } else if (!is.null(periods)) { stop(paste0("'", varn, "' by period are duplicate values"), call. = FALSE)
                           } else   stop(paste0("'", varn, "' are duplicate values"), call. = FALSE)
             }
        }

      if (varn %in% c("year1", "year2")) {
        setnames(vars, names(vars), names(periods))
        if (anyNA(merge(vars, periods, all.x = TRUE,
                        by = names(periods), allow.cartesian = TRUE)))
                               stop(paste0("'", varn, "' row must be exist in '", periods_varn, "'"), call. = FALSE)}

      if (varn == "PSU_sort") {
        psuag <- data.table(vars, PSUs)
        if (!is.null(periods)) psuag <- data.table(periods, psuag)
        psuag <- psuag[, .N, by = names(psuag)][, N := NULL]
        psuag <- rbindlist(list(psuag[, .N, by = c(names(periods), names(PSUs))],
                                psuag[, .N, by = c(names(periods), names(vars))]), use.names = TRUE, fill = TRUE)
        if (nrow(psuag[N > 1]) > 0) stop("'PSU_sort' must be equal for each 'PSU'", call. = FALSE)}

      if (varn == "gender") {
         if (length(unique(vars)) != 2) stop("'gender' must be exactly two values", call. = FALSE)
         if (!all(vars %in% 1:2)) stop("'gender' must be value 1 for male, 2 for females", call. = FALSE) }

      if (varn %in% c("countryX", "periodX", "yearsX", "subperiodsX", "X_ID_level1")) {
               if (names(vars) != varname) stop(paste0("'", varn, "' must be equal with '", varnout,"' names"))
               ncolvars <- ifelse(is.null(vars),0,ncol(vars))
               if (ncolvars != length(varname)) stop(paste0("'", varn, "' length must be equal with '",varnout,"' row count"), call. = FALSE)
      }

      if (varn == "countryX") {
               varsX <- vars[, .N, keyby = names(vars)][, N := NULL]
               country <- country[, .N, keyby = names(country)][, N := NULL]
               if (!identical(country, varsX)) stop("'unique(country)' and 'unique(countryX)' records have different")
       }

      print(vars)

      if (varn %in% c("periodX", "yearsX", "subperiodsX")) {
               peri <- periods
               periX <- copy(vars)
               if (!is.null(country)) peri <- data.table(country, peri)
               if (!is.null(countryX)) periX <- data.table(countryX, periX)
               periX <- periX[, .N, keyby = names(periX)][, N := NULL]
               peri <- peri[, .N, keyby = names(peri)][, N := NULL]
               if (!identical(peri, periX)) {
                   if (is.null(country)) { stop(paste0("'unique(", periods_varn, ")' and 'unique(", periods_varnX, ")' records have different"))
                                } else stop("'unique(country, period)' and 'unique(countryX, periodX)' records have different")
                 }
               peri <- periX <- NULL
      }

      if (varn == "X_ID_level1") {
             ID_level1h <- copy(ID_level1)
             X_ID_level1h <- copy(vars)
             if (!is.null(countryX)) {X_ID_level1h <- data.table(countryX, X_ID_level1h)
             ID_level1h <- data.table(country, ID_level1h)}
             if (!is.null(periodsX)) {X_ID_level1h <- data.table(periodsX, X_ID_level1h)
             ID_level1h <- data.table(periods, ID_level1h)}

             ID_level1h <- ID_level1h[, .N, keyby = names(ID_level1h)][, N := NULL]
             X_ID_level1h <- X_ID_level1h[, .N, keyby = names(X_ID_level1h)]
             if (nrow(X_ID_level1h[N > 1]) > 0) stop("'X_ID_level1' have duplicates")
             X_ID_level1h[, .N := NULL]

             if (!identical(ID_level1h, X_ID_level1h)) {
               if (!is.null(periodsX)) {
                 if (!is.null(country)) {
                       if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' have different row count")
                       if (any(ID_level1h != X_ID_level1h)) stop("''unique(countryX, periodX, X_ID_level1)' and 'unique(country, period, ID_level1)' records have different")
                   } else {
                       if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(periodX, X_ID_level1)' and 'unique(period, ID_level1)' have different row count")
                       if (any(ID_level1h != X_ID_level1h)) stop("''unique(periodX, X_ID_level1)' and 'unique(period, ID_level1)' records have different")  }
                 } else {
                    if (!is.null(country)) {
                        if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(countryX, X_ID_level1)' and 'unique(country, ID_level1)' have different row count")
                        if (any(ID_level1h != X_ID_level1h)) stop("''unique(countryX, X_ID_level1)' and 'unique(country, ID_level1)' records have different")
                     } else {
                        if (nrow(ID_level1h) != nrow(X_ID_level1h)) stop("'unique(X_ID_level1)' and 'unique(ID_level1)' have different row count")
                        if (any(ID_level1h != X_ID_level1h)) stop("''unique(X_ID_level1)' and 'unique(ID_level1)' records have different") }}
               }
             ID_level1h <- X_ID_level1h <- NULL
         }
    } else if (mustbedefined) stop(paste0("'", varn, "' must be defined!"), call. = FALSE)

    return(vars[])
}


namesD <- function(Y, D) {
  Dom_agg <- unique(D)
  h <- vector(mode = "character", length = nrow(Dom_agg))
  for (i in 1:nrow(Dom_agg)) {
    cc <- paste(names(D), Dom_agg[i, ], sep = ".")
    h[i] <- paste(cc, collapse = "__")
  }
  foreach(i = 1 : ncol(Y), .combine = c) %do% paste(names(Y)[i], h, sep="__")
}


domain <- function(Y, D, dataset = NULL) {
  Y <- check_var(vars = Y, varn = "Y", dataset = dataset, check.names = TRUE,
                 ncols = 0, Yncol = 0, Ynrow = 0, isnumeric = TRUE, grepls = "__")
  Ynrow <- nrow(Y)
  D <- check_var(vars = D, varn = "Dom", dataset = dataset, check.names = TRUE,
                 ncols = 0, Yncol = 0, Ynrow = Ynrow, isnumeric = FALSE,
                 ascharacter = TRUE, dif_name = "percoun", grepls = "__")

  Dom_agg <- unique(D)
  setkeyv(Dom_agg, names(Dom_agg))

  i <- k <- NULL
  domen <- foreach(i = 1 : ncol(Y), .combine = data.table) %:%
    foreach(k = 1:nrow(Dom_agg), .combine = data.table) %do%
    ifelse(rowSums(D == Dom_agg[k, ][rep(1, n), ]) == ncol(D), Y[[i]], 0)

  if (!is.data.table(domen)) domen <- data.table(domen)

  setnames(domen, names(Y), namesD(Y, D))
  domen <- data.table(domen, check.names = TRUE)
  return(domen)
}





