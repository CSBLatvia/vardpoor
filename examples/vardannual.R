# Examples for vardannual

# devtools::load_all("vardpoor")


### Example
library("data.table")

t1 <- proc.time()

set.seed(1)
data("eusilc", package = "laeken")
eusilc1 <- eusilc[1:20, ]

dataset1 <- data.table(rbind(eusilc1, eusilc1),
                       year = c(rep(2010, nrow(eusilc1)),
                                rep(2011, nrow(eusilc1))))

dataset1[, country := "AT"]
dataset1[, half := .I - 2 * trunc((.I - 1) / 2)]
dataset1[, quarter := .I - 4 * trunc((.I - 1) / 4)]
dataset1[age < 0, age := 0]

PSU <- dataset1[, .N, keyby = "db030"][, N := NULL]
PSU[, PSU := trunc(runif(.N, 0, 5))]

dataset1 <- merge(dataset1, PSU, all = TRUE, by = "db030")

dataset1[, strata := "XXXX"]
dataset1[, employed := trunc(runif(.N, 0, 2))]
dataset1[, unemployed := trunc(runif(.N, 0, 2))]
dataset1[, labour_force := employed + unemployed]
dataset1[, id_lv2 := paste0("V", .I)]

vardannual(Y = "employed", H = "strata",
           PSU = "PSU", w_final = "rb050",
           ID_level1 = "db030", ID_level2 = "id_lv2",
           Dom = NULL, Z = NULL, years = "year",
           subperiods = "half", dataset = dataset1,
           percentratio = 100, confidence = 0.95,
           method = "cros")
  
vardannual(Y = "employed", H = "strata",
           PSU = "PSU", w_final = "rb050",
           ID_level1 = "db030", ID_level2 = "id_lv2",
           Dom = NULL, Z = NULL, country = "country",
           years = "year", subperiods = "quarter",
           dataset = dataset1, year1 = 2010, year2 = 2011,
           percentratio = 100, confidence = 0.95,
           method = "netchanges")
    
vardannual(Y = "unemployed", H = "strata",
           PSU = "PSU", w_final = "rb050",
           ID_level1 = "db030", ID_level2 = "id_lv2", 
           Dom = NULL, Z = "labour_force",
           country = "country", years = "year",
           subperiods = "quarter", dataset = dataset1,
           year1 = 2010, year2 = 2011,
           percentratio = 100, confidence = 0.95,
           method = "netchanges")

data.table::timetaken(t1)
