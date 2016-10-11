# Pārbauda vai vardomh aprēķina pareizi atlikumus, ja dati
# ir sakārtoti gadījuma secībā

require(vardpoor)

test_unordered_residuals <- function(n = 600, l = 3) {

  # n - personu skaits izlasē
  # l - personu skaits mājsaimniecībā
  # m - mājsaimniecību skaits izlasē

  m <- n / l

  # Personu izlases dati
  datY <- data.table(IDp = 1:n,
                     IDh = rep(1:m, each = l),
                     y = sample(0:1, n, replace = T),
                     strata = 1L,
                     rnd = runif(n))

  # PSU
  datY[, psu := floor((IDh - 1) / 10) + 1L]

  # Mājsaimniecību dati izlasē
  datX <- data.table(IDh = 1:m,
                     x0 = 1L,
                     x1 = sample(0:3, m, replace = T),
                     x2 = sample(0:3, m, replace = T),
                     wd = sample(1:5, m, replace = T),
                     g = rnorm(m, 1, 0.1),
                     q = runif(m),
                     rnd = runif(m))

  # Kalibrētais svars
  datX[, wc := wd * g]

  # Pievieno kalibrēto svaru personām
  datY <- merge(datY, datX[, .(IDh, wc)], by = "IDh")

  # Y summārās vērtības pa majsaimniecībām
  totY <- datY[, .(y = sum(y)), keyby = IDh]

  # Sakārtojam gadījuma secībā
  setorder(datX, rnd)
  setorder(datY, rnd)

  tmpX <- merge(datX, totY, by = "IDh")

  tmpX[, yres := residual_est(Y = y, X = .SD[, .(x0, x1, x2)],
                              weight = wd, q = q)]

  res <- vardomh(Y = "y",
                 H = "strata",
                 PSU = "psu",
                 w_final = "wc",
                 ID_level1 = "IDh",
                 dataset = datY,
                 X = paste0("x", 0:2),
                 X_ID_level1 = "IDh",
                 g = "g",
                 q = "q",
                 datasetX = datX,
                 outp_res = T)

  tmp <- merge(tmpX, res$res_out[, .(IDh, yres2 = y)], by = "IDh")
  
  return(list(tmp[, yres], tmp[, yres2]))
}

test_that("Parbaude vai nejausa datu seciba nerada kludu vardomh atlikumu aprekina",{
  results <- test_unordered_residuals()
  expect_equal(results[[1]], results[[2]])
})
