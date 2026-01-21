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

dados12 <- read.dbf("rhc12.dbf") |> 
  as_tibble() |> 
  mutate(across(all_of(col_datas), as.character))

falhasDatas <- function(data_col) {
  dados12 %>%
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

dep12 <- dados12 |>
  mutate(
    DataInitrt = dmy(DATAINITRT),
    DataObito = dmy(DATAOBITO),
    DataPricon = dmy(DATAPRICON),
    DtDiagno = dmy(DTDIAGNO),
    DtTriage = dmy(DTTRIAGE)
  ) |> select(-c(DATAINITRT,DATAOBITO,DATAPRICON,DTDIAGNO,DTTRIAGE))

dep12F <- dados12 |> filter(SEXO == 2,
                          LOCTUDET == "C50")

write.csv2(dep12F, "rhc12F.csv", row.names = FALSE)

# Terminação
