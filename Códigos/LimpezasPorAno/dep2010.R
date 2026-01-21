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

dados10 <- read.dbf("rhc10.dbf") |> 
  as_tibble() |> 
  mutate(across(all_of(col_datas), as.character))

falhasDatas <- function(data_col) {
  dados10 %>%
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

dados10C <- dados10 %>%
   mutate(
     DATAINITRT = ifelse(DATAINITRT == "20/5 /2010", "20/05/2010", DATAINITRT),
     DTDIAGNO = ifelse(DTDIAGNO == "01/8 /2010", "01/08/2010", DTDIAGNO),
     DTDIAGNO = ifelse(DTDIAGNO == "06/5 /2010","06/05/2010", DTDIAGNO),
     DTDIAGNO = ifelse(DTDIAGNO == "2 /01/2011","02/01/2011",DTDIAGNO),
     DTTRIAGE = ifelse(DTTRIAGE == "1 /02/2010","01/02/2010",DTTRIAGE),
     DTTRIAGE = ifelse(DTTRIAGE == "3 /05/2010","03/05/2010",DTTRIAGE)
   )

dep10 <- dados10C |>
   mutate(
     DataInitrt = dmy(DATAINITRT),
     DataObito = dmy(DATAOBITO),
     DataPricon = dmy(DATAPRICON),
     DtDiagno = dmy(DTDIAGNO),
     DtTriage = dmy(DTTRIAGE)
   ) |> select(-c(DATAINITRT,DATAOBITO,DATAPRICON,DTDIAGNO,DTTRIAGE))

dep10F <- dados10C |> filter(SEXO == 2,
                         LOCTUDET == "C50")

write.csv2(dep10F, "rhc10F.csv", row.names = FALSE)

# Terminação
