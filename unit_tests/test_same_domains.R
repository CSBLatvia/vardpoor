# Nodarbināto darbavietas NACE 2 nozares var aprēķināt gan A*21 (burta), gan A*88 (2-zīmju) līmenī
# Dažās nozarēs (D, L, O, U) domēns ir gan vienā, gan otrā līmenī, jo sīkāk netiek sadalīts.
# Piemēram, A sadalās A.01, A.02, A.03. D kļūst par D.36.
# Tests pārbauda, vai šādā gadījumā attiecīgajam domēnam ar vardomh tiek iegūti tie paši ticamības intervāli.

require(vardpoor)

test_domains <- function(n = 600, l = 3) {
  
  # n - personu skaits izlasē
  # l - personu skaits mājsaimniecībā
  # m - mājsaimniecību skaits izlasē
  
  m <- n / l
  
  # Personu izlases dati
  dat_y <- data.table(IDp = 1:n,
                     IDh = rep(1:m, each = l),
                     y = 1L,
                     strata = sample(1:4, n, replace = T),
                    domain_a = sample(c("AA", "AB", "AC", "BA"), n, replace = T))
  
  # domēns B. BA == B
  dat_y[, domain_b := substring(domain_a, 0, 1)]
  
  # PSU
  dat_y[, psu := floor((IDh - 1) / 10) + 1L]
  
  # Mājsaimniecību dati izlasē
  # 4 apsekojumi
  dat_x <- data.table(IDh = 1:m,
                     x0 = 1L,
                     x1 = sample(0:3, m, replace = T),
                     x2 = sample(0:3, m, replace = T),
                     wd = sample(1:5, m, replace = T),
                     g = rnorm(m, 1, 0.1),
                     q = runif(m),
                     apsek = rep(1:4, each = m/4))
  
  # Kalibrētais svars
  dat_x[, wc := wd * g]
  
  # Pievieno kalibrēto svaru un apsekojuma numuru personām
  dat_y <- merge(dat_y, dat_x[, .(IDh, wc, apsek)], by = "IDh")
  
  n_h <- data.table(strata = 1:4, pop = sample(500:1500,4))
  
  result_a <- vardomh(Y = "y",
                    H = "strata", 
                    PSU = "psu", 
                    w_final = "wc",
                    ID_level1 = "IDh",
                    Dom = "domain_a",
                    ind_gr = "apsek",
                    N_h = n_h,
                    X = paste0("x", 0:2),
                    X_ID_level1 = "IDh",
                    g = "g", 
                    q = "q", 
                    dataset = dat_y,
                    datasetX = dat_x)
  
  result_b <- vardomh(Y = "y",
                      H = "strata", 
                      PSU = "psu", 
                      w_final = "wc",
                      ID_level1 = "IDh",
                      Dom = "domain_b",
                      ind_gr = "apsek",
                      N_h = n_h,
                      X = paste0("x", 0:2),
                      X_ID_level1 = "IDh",
                      g = "g", 
                      q = "q", 
                      dataset = dat_y,
                      datasetX = dat_x)  
  
  ci_a1 <- result_a$all_result[domain_a == "BA", CI_lower]
  ci_a2 <- result_a$all_result[domain_a == "BA", CI_upper]
  
  ci_b1 <- result_b$all_result[domain_b == "B", CI_lower]
  ci_b2 <- result_b$all_result[domain_b == "B", CI_upper]
  return(list(ci_a1, ci_a2, ci_b1, ci_b2))
}

test_that("Parbaude vai identiskiem domeniem dazadas domenu kopas tiek aprekinati identiski ticamibas intervali",{
  results <- test_domains()
  expect_equal(results[[1]], results[[3]])
  expect_equal(results[[2]], results[[4]])
})