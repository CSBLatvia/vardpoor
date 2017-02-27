
lin.ratio <- function(Y, Z, weight, Dom = NULL, dataset = NULL, percentratio = 1, checking = TRUE) {

  if (length(percentratio) != 1 | !any(is.integer(percentratio) | percentratio > 0)) stop("'percentratio' must be a positive integer")

  if (checking) {
       Y <- check_var(vars = Y, varn = "Y", dataset = dataset,
                      check.names = TRUE, isnumeric = TRUE, grepls = "__")
       Ynrow <- nrow(Y)
       Yncol <- ncol(Y)

       Z <- check_var(vars = Z, varn = "Z", dataset = dataset,
                      check.names = TRUE, Yncol = Yncol, Ynrow = Ynrow,
                      isnumeric = TRUE, mustbedefined = FALSE)

       weight <- check_var(vars = weight, varn = "weight",
                           dataset = dataset, ncols = 1,
                           Ynrow = Ynrow, isnumeric = TRUE, isvector = TRUE)
   }

  if (!is.null(Dom)) Yd <- domain(Y, Dom) else Yd <- Y
  if (!is.null(Dom)) Zd <- domain(Z, Dom) else Zd <- Z

  Y_est <- colSums(Yd * weight)
  Z_est <- colSums(Zd * weight)
  R_est <- Y_est / Z_est

  percentratio <- as.integer(percentratio)
  U <- percentratio * t((1 / Z_est) * t(Yd - t(R_est * t(Zd))))
  return(data.table(U))
}
