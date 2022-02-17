# Test if vardom and vardomh return the same result with the same information
# where:
#   - vardom use the data at level 1 (for example households)
#   - vardomh use the data at level 2 (for example persons)
#
# test_fun - test for data without period
# test_fun2 - test for data with period

# library(testthat)
# devtools::load_all(path = "vardpoor")
# library(data.table)
# library(vardpoor)

#### test without period ----

test_fun <- function(n1 = 3000) {
  # data generation
  # g-matrix
  g_dat <- data.table(id_m = 1:n1,
                      x0 = 1L,
                      x1 = sample(0:2, n1, replace = T),
                      x2 = sample(0:3, n1, replace = T),
                      x3 = sample(0:3, n1, replace = T),
                      x4 = sample(0:3, n1, replace = T),
                      x5 = sample(0:3, n1, replace = T),
                      g = rnorm(n1, 1, 0.1),
                      q = runif(n1))
  
  # vardom data
  nn <- round(n1 / 300, 0) # number of PSUs
  # dat_x <- data.table()
  strata <- sample(1:4, n1, replace = T,)
  dat_x <- data.table(
    id_m = 1:n1,
    b06 = sample(1:11, n1, replace = T,
                 prob = c(0.1922021, 0.1302067, 0.0641481, 0.0471424,
                          0.01797438, 0.007695619, 0.002179529, 0.00131848,
                          0.0003498009, 0.000107631, 2.690776e-05)),
    wd = sample(1:5, n1, replace = T), 
    strata = strata,
    survey = rep(1:4, each = n1/4), 
    iec = sample(1:nn, n1, replace = T)
  )
  
  # vardomh data
  dat_y <- data.table()
  l <- 1
  for (i in 1:n1) {
    if (dat_x[i, b06] == 1) {
      rinda <- dat_x[i]
      rinda <- rinda[, id_p := l]
      dat_y <- rbind(dat_y, rinda)
      l <- l + 1
    } else {
      j <- dat_x[i, b06]
      for (k in 1:j) {
        rinda <- dat_x[i]
        rinda <- rinda[, b06 := k]
        rinda <- rinda[, id_p := l]
        dat_y <- rbind(dat_y, rinda)
        l <- l + 1
      }
    }
  }
  rm(i, j, k, l, rinda)
  
  n2 <- nrow(dat_y)
  if ((nrow(dat_y) - sum(dat_x$b06)) != 0) stop("wrong number of records")
  
  dat_y[, y := sample(0:1, n2, replace = T)] # generate y variable
  gg <- g_dat[, c("id_m", "g")]
  
  ycol <- dat_y[, lapply(.SD, sum),
                keyby = id_m,
                .SDcols = "y"]
  dat_yy <- merge(dat_y, gg, by = "id_m")
  dat_x$y <- ycol$y # add to vardom data
  dat_xx <- merge(dat_x, g_dat, by = c("id_m"))
  dat_xx[, wc := wd * g]
  dat_yy[, wc := wd * g]
  rm(dat_x, dat_y, gg, ycol)
  
  # calculations ----
  
  n_h <- data.table(strata = 1:4, pop = sample(500:1500, 4))
  res_1 <- vardom(Y = "y",
                  H = "strata",
                  PSU = "iec",
                  w_final = "wc",
                  fh_zero = TRUE,
                  N_h = n_h,
                  X = paste0("x", 0:5),
                  g = "g",
                  q = "q",
                  dataset = dat_xx)$all_result
  
  res_2 <- vardomh(Y = "y",
                   H = "strata",
                   PSU = "iec",
                   w_final = "wc",
                   ID_level1 = "id_m",
                   ID_level2 = "id_p",
                   N_h = n_h,
                   fh_zero = TRUE,
                   X = paste0("x", 0:5),
                   X_ID_level1 = "id_m",
                   g = "g",
                   q = "q",
                   dataset = dat_yy,
                   datasetX = g_dat)$all_result
  
  # Those variables differ if calculated from different levels
  res_1 <- res_1[, -c("respondent_count", "n_nonzero","pop_size")]
  res_2 <- res_2[, -c("respondent_count", "n_nonzero","pop_size")]
  
  names_a <- names(res_2)
  return(list(res_2, res_1[, .SD, .SDcols = names_a]))
}

test_that("test equal for one period, diferent levels", {
  results <- test_fun()
  expect_equal(results[[1]], results[[2]])
})


#### 2 periods, with calibration, without Z ----

test_fun2 <- function(n1){
  # g-matrix
  g_dat <- data.table(id_m = 1:n1,
                      x0 = 1L,
                      x1 = sample(0:2, n1, replace = T),
                      x2 = sample(0:3, n1, replace = T),
                      x3 = sample(0:3, n1, replace = T),
                      x4 = sample(0:3, n1, replace = T),
                      x5 = sample(0:3, n1, replace = T),
                      g = rnorm(n1, 1, 0.1),
                      q = runif(n1),
                      period = rep(1:2, each = n1 / 2))
  
  # vardom data
  nn <- round(n1 / 300, 0) # number of PSUs
  # dat_x <- data.table()
  strata <- sample(1:4, n1, replace = T,)
  dat_x <- data.table(
    id_m = 1:n1,
    b06 = sample(1:11, n1, replace = T,
                 prob = c(0.1922021, 0.1302067, 0.0641481, 0.0471424,
                          0.01797438, 0.007695619, 0.002179529, 0.00131848,
                          0.0003498009, 0.000107631, 2.690776e-05)),
    wd = sample(1:5, n1, replace = T), 
    strata = strata,
    survey = rep(1:4, each = n1/4), 
    iec = sample(1:nn, n1, replace = T)
  )
  
  # vardomh data
  dat_y <- data.table()
  l <- 1
  for (i in 1:n1) {
    if (dat_x[i, b06] == 1) {
      rinda <- dat_x[i]
      rinda <- rinda[, id_p := l]
      dat_y <- rbind(dat_y, rinda)
      l <- l + 1
    } else {
      j <- dat_x[i, b06]
      for (k in 1:j) {
        rinda <- dat_x[i]
        rinda <- rinda[, b06 := k]
        rinda <- rinda[, id_p := l]
        dat_y <- rbind(dat_y, rinda)
        l <- l + 1
      }
    }
  }
  rm(i, j, k, l, rinda)
  
  n2 <- nrow(dat_y)
  if ((nrow(dat_y) - sum(dat_x$b06)) != 0) stop("wrong number of records")
  
  dat_y[, y := sample(0:1, n2, replace = T)] # add y variable
  gg <- g_dat[, c("id_m", "g", "period")]
  
  ycol <- dat_y[, lapply(.SD, sum),
                keyby = id_m,
                .SDcols = "y"]
  dat_yy <- merge(dat_y, gg, by = "id_m")
  dat_x$y <- ycol$y # add to vardom data
  dat_xx <- merge(dat_x, g_dat, by = c("id_m"))
  dat_xx[, wc := wd * g]
  dat_yy[, wc := wd * g]
  
  n_h <- data.table(period = rep(1:2, each = 4),
                    strata = rep(1:4, 2),
                    pop = rep(sample(500:1500, 4),2))
  
  res_1 <- vardom(Y = "y",
                  H = "strata",
                  PSU = "iec",
                  w_final = "wc",
                  fh_zero = TRUE,
                  N_h = n_h,
                  X = paste0("x", 0:5),
                  g = "g",
                  q = "q",
                  dataset = dat_xx, 
                  period = "period")$all_result
  
  res_2 <- vardomh(Y = "y",
                   H = "strata",
                   PSU = "iec",
                   w_final = "wc",
                   ID_level1 = "id_m",
                   ID_level2 = "id_p",
                   N_h = n_h,
                   fh_zero = TRUE,
                   X = paste0("x", 0:5),
                   X_ID_level1 = "id_m",
                   g = "g",
                   q = "q",
                   dataset = dat_yy,
                   datasetX = g_dat,
                   period = "period",
                   periodX = "period")$all_result
  
  # Those variables differ if calculated from different levels
  res_1 <- res_1[, -c("respondent_count", "n_nonzero","pop_size")]
  res_2 <- res_2[, -c("respondent_count", "n_nonzero","pop_size")]
  
  names_a <- names(res_2)
  return(list(res_2, res_1[, .SD, .SDcols = names_a]))
}

test_that("test equal for one period, diferent levels", {
  results <- test_fun2(6000)
  expect_equal(results[[1]], results[[2]])
})
