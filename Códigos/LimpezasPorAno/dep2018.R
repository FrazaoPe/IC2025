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

dados18 <- read.dbf("rhc18.dbf") |> 
  as_tibble() |> 
  mutate(across(all_of(col_datas), as.character))

falhasDatas <- function(data_col) {
  dados18 %>%
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

dados18C <- dados18 %>%
  mutate(
    DATAINITRT = ifelse(DATAINITRT == "1 /04/2019", "01/04/2019", DATAINITRT),
    DTDIAGNO = ifelse(DTDIAGNO == "22/7 /2018", "22/07/2018", DTDIAGNO),
    DTDIAGNO = ifelse(DTDIAGNO == "28/1 /2018","28/01/2018", DTDIAGNO),
    DTDIAGNO = ifelse(DTDIAGNO == "30/4 /2018","30/04/2018",DTDIAGNO),
    DTDIAGNO = ifelse(DTDIAGNO == "5 /10/2018","05/10/2018",DTDIAGNO),
  )

dep18 <- dados18C |>
  mutate(
    DataInitrt = dmy(DATAINITRT),
    DataObito = dmy(DATAOBITO),
    DataPricon = dmy(DATAPRICON),
    DtDiagno = dmy(DTDIAGNO),
    DtTriage = dmy(DTTRIAGE)
  ) |> select(-c(DATAINITRT,DATAOBITO,DATAPRICON,DTDIAGNO,DTTRIAGE))

dep18F <- dados18C |> filter(SEXO == 2,
                          LOCTUDET == "C50")

write.csv2(dep18F, "rhc18F.csv", row.names = FALSE)

# Terminação
