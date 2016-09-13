# Unit test 001
# Pārbauda vai vardomh aprēķina pareizi atlikumus, ja dati
# ir sakārtoti gadījuma secībā

# Atgriež TRUE vai FALSE

unit.test001 <- function() {
  require(vardpoor)

  n <- 600
  m <- n / 3

  datY <- data.table(IDp = 1:n,
                     IDh = rep(1:m, each = 3),
                     y = sample(0:1, n, replace = T),
                     strata = 1L,
                     rnd = runif(n))
  datY[, psu := floor((IDh - 1) / 10) + 1L]
  datY

  datX <- data.table(IDh = 1:m,
                     x0 = 1L,
                     x1 = sample(0:3, m, replace = T),
                     x2 = sample(0:3, m, replace = T),
                     wd = sample(1:5, m, replace = T),
                     g = rnorm(m, 1, 0.1),
                     q = runif(m),
                     rnd = runif(m))
  datX[, wc := wd * g]
  datX

  datY <- merge(datY, datX[, .(IDh, wc)], by = "IDh")
  datY

  totY <- datY[, .(y = sum(y)), keyby = IDh]
  totY

  # Sakārtojam gadījuma secībā
  setorder(datX, rnd)
  setorder(datY, rnd)

  tmpX <- merge(datX, totY, by = "IDh")
  tmpX

  tmpX[, yres := residual_est(Y = y,
                              X = .SD[, .(x0, x1, x2)],
                              weight = wd, q = q)]
  tmpX

  res <- vardomh(Y = "y",
                 H = "strata",
                 PSU = "psu",
                 w_final = "wc",
                 ID_household = "IDh",
                 dataset = datY,
                 X = paste0("x", 0:2),
                 X_ID_household = "IDh",
                 g = "g",
                 q = "q",
                 datasetX = datX,
                 outp_res = T)
  res$res_out

  tmp <- merge(tmpX, res$res_out[, .(IDh, yres2 = y)], by = "IDh")
  tmp

  is.logical(tmp[, all.equal(yres, yres2)])
}

unit.test001()
