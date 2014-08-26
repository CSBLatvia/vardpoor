domain <- function(Y, D) {
  
  name.Y <- substitute(Y)
  name.D <- substitute(D)
  
  # Y
  Y <- data.table(Y, check.names = T)
  if (!all(sapply(Y, is.numeric))) stop(name.Y, " must be numerical")
  if (any(is.na(Y))) stop(name.Y, " has unknown values")
  n <- nrow(Y)

  # D
  D <- data.table(D, check.names = F)
  if (any(duplicated(names(D))))
    stop(name.D, " has duplicate column names: ",
         paste(names(D)[duplicated(names(D))], collapse = ", "))
  if (nrow(D) != n) stop(name.Y, " and ", name.D ," have different row count")
  
  Dom_agg <- unique(D)
  setkeyv(Dom_agg, names(Dom_agg))
  
  i <- k <- NULL 	
  domen <- foreach(i = 1:ncol(Y), .combine = data.table) %:%
    foreach(k = 1:nrow(Dom_agg), .combine = data.table) %do%
      ifelse(rowSums(D == Dom_agg[k, ][rep(1, n), ]) == ncol(D), Y[[i]], 0)

  namesD <- function(Y, D) {
    h <- vector(mode = "character", length = nrow(Dom_agg))
    for (i in 1:nrow(Dom_agg)) {
      cc <- paste(names(D), Dom_agg[i, ], sep = ".")
      h[i] <- paste(cc, collapse = "__")
    }
    foreach(i = 1:ncol(Y), .combine = c) %do% paste(names(Y)[i], h, sep="__")
  }
  
  if (!is.data.table(domen)) domen <- data.table(domen)
  setnames(domen, namesD(Y, D))
  return(domen)
}
