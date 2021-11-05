# Example for vardcrospoor

devtools::load_all("vardpoor")

t1 <- proc.time()

library("data.table")
data("eusilc", package = "laeken")
setDT(eusilc)

set.seed(1)
eusilc <- eusilc[sample(x = .N, size = 3000)]

dataset1 <- data.table(rbindlist(list(eusilc, eusilc)),
                       year = c(rep(2010, nrow(eusilc)),
                                rep(2011, nrow(eusilc))))
dataset1[age < 0, age := 0]

PSU <- dataset1[, .N, keyby = "db030"][, N := NULL]
PSU[, PSU := trunc(runif(nrow(PSU), 0, 100))]
PSU[, inc := runif(.N, 20, 100000)]

dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")
dataset1[, strata := "XXXX"]
dataset1[, pl085 := 12 * trunc(runif(.N, 0, 2))]
dataset1[, month_at_work := 12 * trunc(runif(.N, 0, 2))]
dataset1[, id_l2 := paste0("V", .I)]

vardcrospoor(Y = "inc", age = "age",
             pl085 = "pl085", 
             month_at_work = "month_at_work",
             Y_den = "inc", Y_thres = "inc",
             wght_thres = "rb050",
             H = "strata", PSU = "PSU", 
             w_final = "rb050", ID_level1 = "db030",
             ID_level2 = "id_l2",
             Dom = c("rb090", "db040"),
             country = NULL, period = "year",
             sort = NULL, gender = NULL,
             dataset = dataset1,
             percentage = 60,
             order_quant = 50L,
             alpha = 20,
             confidence = 0.95,
             type = "linrmpg")

data.table::timetaken(t1)
