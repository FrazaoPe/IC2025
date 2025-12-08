library(survival)
library(tidyverse)

base <- read_csv2("baseDashboard.csv")
base <- base |> 
  mutate(escolaridade = as.factor(escolaridade),
         racaCor = as.factor(racaCor),
         faixaEtaria = as.factor(faixaEtaria),
         companheiro = as.factor(companheiro),
         estadiamento = as.factor(estadiamento),
         lateralidade = as.factor(lateralidade),
         maisumtu = as.factor(maisumtu),
         fezQuimio = as.factor(fezQuimio),
         fezRadio = as.factor(fezRadio),
         fezCirurgia = as.factor(fezCirurgia),
         historico = as.factor(historico),
         hiato = as.factor(hiato)
         )

# Terminação
