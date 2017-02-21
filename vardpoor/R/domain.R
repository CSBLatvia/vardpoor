
check_var <- function(vars, varn, dataset, check.names = FALSE,
                      ncols = NULL, Yncol = 0, Ynrow = 0, Xnrow = 0,
                      isnumeric = FALSE, ascharacter = FALSE,
                      asvector = FALSE, grepls = NULL, dif_name = "",
                      namesID1 = "id", duplicatednames = FALSE,
                      withperiod = TRUE, PSUs = NULL, periods = NULL,
                      periods_varn  = NULL, country = NULL,
                      periodsX = NULL){

  if (varn %in%  c("g", "q") & (is.null(class(vars)) | any(class(vars) == "function"))) stop("'g' must be numeric", call. = FALSE)
  if (is.null(vars)) {
    if (Xnrow > 0 & varn %in% c("q", "ind_gr", "id")) { vars <- rep(1, Xnrow)
                                                        dataset <- NULL}
    if (Ynrow > 0 & varn == "id") { vars <- rep(1, Ynrow)
                                    dataset <- NULL}}

  if (!is.null(vars)) {
      if (withperiod == FALSE & varn == "period") stop(paste0("'period' must be NULL for those data"), call. = FALSE)
      if(!is.null(dataset)) {
        dataset <- data.table(dataset)
        if (min(vars %in% names(dataset)) != 1) stop(paste0("'", varn, "' does not exist in 'dataset'!"), call. = FALSE)
        if (min(vars %in% names(dataset)) == 1)  vars <- dataset[, vars, with = FALSE]}
      if (!(data_type %in% c("data.frame", "data.table")))  stop("'data_type' please check again!", call. = FALSE)

      vars <- data.table(vars, check.names = check.names)
      if (ascharacter) vars[, (names(vars)) := lapply(.SD, as.character)]
      if (anyNA(vars)) stop(paste0("'", varn, "' has missing values"), call. = FALSE)
      if (Ynrow > 0) if (nrow(vars) != Ynrow) stop(paste0("'", varn, "' length must be equal with 'Y' row count"), call. = FALSE)
      if (Xnrow > 0) if (nrow(vars) != Xnrow) stop(paste0("'", varn, "' length must be equal with 'X' row count"), call. = FALSE)
      if (Yncol > 0) if (ncol(vars) != Yncol) stop(paste0("'", varn, "' length must be equal with 'Y' column count"), call. = FALSE)
      if (ncols > 0) if (ncol(vars) != ncols) stop(paste0("'", varn, "' must be ", ncols, " column data.frame, matrix, data.table"), call. = FALSE)
      if (isnumeric) if(!all(sapply(vars, is.numeric))) stop(paste0("'", varn, "' must be numeric"), call. = FALSE)
      if (!is.null(grepls)) if (any(grepl(grepls, names(vars)))) stop(paste0("'", varn, "' is not allowed column names with '", grepls, "'"), call. = FALSE)
      if (names(vars) == dif_name) stop(paste0("'", dif_name, "' must be different name"), call. = FALSE)
      if (names(vars) == namesID1) setnames(vars, names(vars), paste0(names(vars), "_", varn))
      if (duplicatednames == TRUE & !is.null(vars)) {
        if (any(duplicated(names(vars))))
          stop(paste0("'", varn, "' are duplicate column names: "),
               paste(names(vars)[duplicated(names(vars))], collapse = ","), call. = FALSE) }
      if (ncols == 1 & asvector) vars <- vars[[names(vars)]]
      if (any(vars == 0) & varn == "g") stop("'g' value can not be 0", call. = FALSE)
      if (varn == "q") if (any(is.infinite(vars))) stop("'q' value can not be infinite", call. = FALSE)

      if (varn == "id") {
             if (is.null(periods)){ if (any(duplicated(vars))) stop("'id' are duplicate values", call. = FALSE)
                                 } else { dd <- data.table(period, vars)
                                          if (any(duplicated(dd, by = names(dd)))) stop("'id' by period are duplicate values", call. = FALSE)
                                          dd <- NULL}}
      if (varn == "PSU_sort") {
                psuagg <- data.table(vars, PSUs)
                if (!is.null(period)) psuag <- data.table(periods, psuag)
                psuag <- psuag[, .N, by = names(psuag)][,N := NULL]
                psuag <- psuag[, .N, by = c(names(periods), names(vars))]
                if (nrow(psuag[N > 1]) > 0) stop(paste0("'PSU_sort' must be equal for each 'PSU'", call. = FALSE)
      }
      if (varn %in% c("year1", "year2")) {
        setnames(vars, names(vars), names(periods))
        if (anyNA(merge(vars, periods, all.x = TRUE,
                        by = names(periods), allow.cartesian = TRUE)))
                               stop(paste0("'", varn, "' row must be exist in '", periods_varn, "'"), call. = FALSE)
      }

      if (varn == "gender") {
         if (length(unique(vars)) != 2) stop("'gender' must be exactly two values", call. = FALSE)
         if (!all(vars %in% 1:2)) stop("'gender' must be value 1 for male, 2 for females", call. = FALSE) }
      } else if (!(varn %in% c("country", "sort", "Dom"))) stop(paste0("'", varn, "' must be defined!"), call. = FALSE)
  return(vars)
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

  setnames(domen, namesD(Y, D))
  domen <- data.table(domen, check.names=TRUE)
  return(domen)
}
