# Universidade Federal Fluminense
# Instituto de matemática e estatística
# Departamento de estatística
# Iniciação científica
# Orientadora: Núbia Karla de Oliveira Almeida
# Bolsista faperj: Pedro Frazão Dutra

library(foreign)
library(tidyverse)
library(lubridate)

col_datas <- c("DATAINITRT", "DATAOBITO", 
               "DATAPRICON", "DTDIAGNO", 
               "DTTRIAGE")

dados13 <- read.dbf("rhc13.dbf") |> 
  as_tibble() |> 
  mutate(across(all_of(col_datas), as.character))

falhasDatas <- function(data_col) {
  dados13 %>%
    filter(is.na(dmy(.data[[data_col]]))) %>%
    count(.data[[data_col]])
}

falhas_initrt <- falhasDatas("DATAINITRT")
falhas_obito <- falhasDatas("DATAOBITO")
falhas_pricon <- falhasDatas("DATAPRICON")
falhas_diagno <- falhasDatas("DTDIAGNO")
falhas_triage <- falhasDatas("DTTRIAGE")

cat("Contagem de falhas em DATAINITRT:\n")
print(falhas_initrt)
cat("\n")

cat("Contagem de falhas em DATAOBITO:\n")
print(falhas_obito)
cat("\n")

cat("Contagem de falhas em DATAPRICON:\n")
print(falhas_pricon)
cat("\n")

cat("Contagem de falhas em DTDIAGNO:\n")
print(falhas_diagno)
cat("\n")

cat("Contagem de falhas em DTTRIAGE:\n")
print(falhas_triage)
cat("\n")

dep13 <- dados13 |>
  mutate(
    DataInitrt = dmy(DATAINITRT),
    DataObito = dmy(DATAOBITO),
    DataPricon = dmy(DATAPRICON),
    DtDiagno = dmy(DTDIAGNO),
    DtTriage = dmy(DTTRIAGE)
  ) |> select(-c(DATAINITRT,DATAOBITO,DATAPRICON,DTDIAGNO,DTTRIAGE))

dep13F <- dados13 |> filter(TPCASO == 1,
                          SEXO == 2,
                          LOCTUDET == "C50")

write.csv2(dep13F, "rhc13F.csv", row.names = FALSE)

# Terminação
