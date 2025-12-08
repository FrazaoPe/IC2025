# Universidade Federal Fluminense
# Instituto de matemática e estatística
# Departamento de estatística
# Iniciação científica
# Orientadora: Núbia Karla de Oliveira Almeida
# Bolsista FAPERJ: Pedro Frazão Dutra

## Carregamento dos pacotes necessários
library(tidyverse)
library(lubridate)
library(stringr)
library(geobr)

# rhc10F <- read_csv2("rhc10F.csv")
# rhc11F <- read_csv2("rhc11F.csv")
# rhc12F <- read_csv2("rhc12F.csv")
# rhc13F <- read_csv2("rhc13F.csv")
# rhc14F <- read_csv2("rhc14F.csv")
# rhc15F <- read_csv2("rhc15F.csv")
# rhc16F <- read_csv2("rhc16F.csv")
# rhc17F <- read_csv2("rhc17F.csv")
# rhc18F <- read_csv2("rhc18F.csv")
# rhc19F <- read_csv2("rhc19F.csv")
# 
# empilhado <- bind_rows(rhc10F, rhc11F, rhc12F, rhc13F, rhc14F,
#                        rhc15F, rhc16F, rhc17F, rhc18F, rhc19F)
# 
# write.csv2(empilhado,"empilhado.csv",row.names = FALSE)

## Carregamento dos dados
empilhado <- read.csv2("empilhado.csv")

## Filtros iniciais.
empilhado1 <- empilhado |> 
  filter(TPCASO == 1,
         DIAGANT == 1,
         RZNTR == 8,
         !ESTDFIMT %in% c(8, 9))

## Código que calcula a quantidade de datas que não são datas
falhasDatas <- function(data_col) {
  empilhado1 %>%
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

## Criando colunas de classe data para poder manipulá-las.
empilhado2 <- empilhado1 |>
  mutate(
    DataInitrt = dmy(DATAINITRT),
    DataObito = dmy(DATAOBITO),
    DataPricon = dmy(DATAPRICON),
    DtDiagno = dmy(DTDIAGNO),
    DtTriage = dmy(DTTRIAGE)
  )

## Criando colunas de diferença entre datas
empilhado3 <- empilhado2 |> 
  mutate(
    diff_Diagno_Pricon = as.numeric(DtDiagno - DataPricon),
    diff_Diagno_Tratamento = as.numeric(DataInitrt - DtDiagno),
    diff_Diagno_Obito = as.numeric(DataObito - DtDiagno),
    diff_Pricon_Tratamento = as.numeric(DataInitrt - DataPricon),
    diff_Pricon_Obito = as.numeric(DataObito - DataPricon),
    diff_Tratamento_Obito = as.numeric(DataObito - DataInitrt),
    
    check_Diagno_Pricon = is.na(DtDiagno) | is.na(DataPricon) | diff_Diagno_Pricon >= 0,
    check_Diagno_Tratamento = is.na(DtDiagno) | is.na(DataInitrt) | diff_Diagno_Tratamento >= 0,
    check_Diagno_Obito = is.na(DtDiagno) | is.na(DataObito) | diff_Diagno_Obito >= 0,
    check_Pricon_Tratamento = is.na(DataPricon) | is.na(DataInitrt) | diff_Pricon_Tratamento >= 0,
    check_Pricon_Obito = is.na(DataPricon) | is.na(DataObito) | diff_Pricon_Obito >= 0,
    check_Tratamento_Obito = is.na(DataInitrt) | is.na(DataObito) | diff_Tratamento_Obito >= 0
  )

## Filtrando linhas que seguem o critério: 
# DataDiagno < DataPricon < DataInitrt < DataObito
empilhado4 <- empilhado3 |>
  filter(
    check_Diagno_Pricon,
    check_Diagno_Tratamento,
    check_Diagno_Obito,
    check_Pricon_Tratamento,
    check_Pricon_Obito,
    check_Tratamento_Obito 
  ) |> 
  select(-c(starts_with("diff"), starts_with("check")))

## Selecionando pessoas com DataInitrt <= 31/12/2019
empilhado5 <- empilhado4 |> 
  filter(DataInitrt <= ymd("2019/12/31"))

## Retirando pessoas com ESTDFIMT == 6 mas sem data de óbito
empilhado6 <- empilhado5 |> 
  filter(!(ESTDFIMT == 6 & ((DATAOBITO %in% c("99/99/9999", "/  /")))))

## Retirando pessoas com ESTDFIMT entre 1 e 5 e com data de óbito 
# igual a 99/99/9999
empilhado7 <- empilhado6 |> 
  filter(!(ESTDFIMT %in% 1:5 & (DATAOBITO == "99/99/9999")))

## Lendo os códigos de municípios oficiais do IBGE
codigos_oficiais <- read_municipality(code_muni = "all", year = 2024) %>%
  distinct(code_muni) %>%
  pull(code_muni)

unicosMUUH <- unique(empilhado7$MUUH)
unicosPROCEDEN <- unique(empilhado7$PROCEDEN)

invalidosMUUH <- setdiff(unicosMUUH, codigos_oficiais)
invalidosPROCEDEN <- setdiff(unicosPROCEDEN,codigos_oficiais)

## Imprimindo, se houver, códigos que não são códigos de municípios 
print(invalidosMUUH)
print(invalidosPROCEDEN)

## Imprimindo a quantidade de 9999999
sum(empilhado7$PROCEDEN == "9999999", na.rm = TRUE)

## Imprimindo a quantidade de 7777777
sum(empilhado7$PROCEDEN == "7777777", na.rm = TRUE)

## Retirando linhas com códigos iguais a 9999999 ou 7777777;
# Retirando linhas com LOCTUPRO igual a C50.9.
# Criando variável PEREGRINACAO.
empilhado8 <- empilhado7 |> 
  filter(!PROCEDEN %in% c("9999999", "7777777"),
         !LOCTUPRO %in% c("C50.9")) |> 
  mutate(PEREGRINACAO = ifelse(MUUH != PROCEDEN,1,0))

## Contando quantas pessoas tem pelo menos um 9 em PRITRATH
empilhado8$PRITRATH <- as.character(empilhado8$PRITRATH)
tem_nove <- grepl("9", empilhado8$PRITRATH)
sum(tem_nove)

## Contando quantas pessoas tem pelo menos um 1 em PRITRATH
tem_um <- grepl("1", empilhado8$PRITRATH)
sum(tem_um)

## Retirando as pessoas que apresentaram 9 em PRITRATH
empilhado9 <- empilhado8 |> 
  filter(PRITRATH != "9")

## Criando variável COMPBIO
empilhado10 <- empilhado9 |> 
  mutate(COMPBIO = str_extract(TIPOHIST, "(?<=/)\\d+"))

## Olhando as variáveis TIPOHIST e COMPBIO lado a lado
a <- empilhado10 |> 
  select(TIPOHIST,COMPBIO)

## Verificando a quantidade de cada tipo de comportamento biológico
table(empilhado10$COMPBIO)

## Criando tabela que contém o nome da variável e a sua % de valores Não-NA's
completude <- empilhado10 %>%
  summarise(across(everything(), ~mean(!is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "variavel",
               values_to = "completude") 

### Reconhecimento das informações faltantes

## O que está ao lado da hashtag será tido como informação faltante.
table(empilhado10$ALCOOLIS) #9; #4; #0; #8
table(empilhado10$BASMAIMP) #9
table(empilhado10$ESTADIAM) #99; #B2; #88
table(empilhado10$ESTCONJ) #9; #0
table(empilhado10$HISTFAMC) #9; #0; Mas, não vamos usar essa v.a
table(empilhado10$INSTRUC) #9
table(empilhado10$MAISUMTU) #0
table(empilhado10$RACACOR) #9
table(empilhado10$TABAGISM) #9; #4; #0; #8
table(empilhado10$TIPOHIST) #Ignore
table(empilhado10$COMPBIO) #7
table(empilhado10$EXDIAG) #9; #8
table(empilhado10$ORIENC) #0; #9

## Lista das variáveis de interesse
vars <- c("ALCOOLIS", "BASMAIMP", "ESTADIAM", "ESTCONJ", 
          "INSTRUC", "MAISUMTU", "RACACOR", 
          "TABAGISM", "EXDIAG","COMPBIO",
          "ORIENC")

## Lista das variáveis e seus respectivos indicadores de informação faltante
na_codes <- list(
  ALCOOLIS = c("9","4","0","8"),
  BASMAIMP = c("9"),
  ESTADIAM = c("99", "B2", "88"),
  ESTCONJ = c("9", "0"),
  INSTRUC = c("9"),
  MAISUMTU = c("0"),
  RACACOR = c("9"),
  TABAGISM = c("9", "4", "0", "8"),
  EXDIAG = c("9", "8"),
  COMPBIO = c("7"),
  ORIENC = c("0","9")
)

## Criando tabela com o nome da variável e o seu grau de preenchimento
df_completude <- map_dfr(names(na_codes), function(var) {
  cod_na <- na_codes[[var]]
  preenchido_pct <- mean(!(as.character(empilhado10[[var]]) %in% cod_na),
                         na.rm = TRUE) * 100
  tibble(variavel = var, preenchido_pct = preenchido_pct)
})

## Plotando o gráfico
ggplot(df_completude, aes(x = reorder(variavel, preenchido_pct), 
                          y = preenchido_pct)) +
  geom_col(fill = "#0072B2") +
  geom_text(aes(label = sprintf("%.1f%%", preenchido_pct)),
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Porcentagem de Preenchimento das Variáveis",
    x = " ",
    y = " "
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), 
                     limits = c(0, 110), breaks = seq(0,100,25)) 

#rm(empilhado,empilhado1,empilhado2,empilhado3,empilhado4,empilhado5,empilhado6,
#   empilhado7,empilhado8,empilhado9)

### CONSTRUÇÃO DE REPRESENTAÇÕES PARA OS DADOS DE SOBREVIVÊNCIA

## A partir deste momento, só temos dois valores possíveis para a variável
# DATAOBITO, são eles: "/ /" ou uma data qualquer.

falhasDatas <- function(data_col) {
  empilhado10 %>%
    filter(is.na(dmy(.data[[data_col]]))) %>%
    count(.data[[data_col]])
}

falhas_initrt <- falhasDatas("DATAINITRT")
falhas_obito <- falhasDatas("DATAOBITO")

cat("Contagem de falhas em DATAINITRT:\n")
print(falhas_initrt)
cat("\n")

cat("Contagem de falhas em DATAOBITO:\n")
print(falhas_obito)
cat("\n")

# Supondo que ESTDFIMT = 6 é óbito por câncer, teremos 
# o seguinte esquema de classificação de censuras e falhas:

## ESTDFIMT = 6 e DataObito > 31/12/2019 <- CENSURA
# TEMPO DE SOBREVIDA <- 31/12/2019 - DATAINITRT

## ESTDFIMT = 6 e DataObito < 31/12/2019 <- FALHA
# TEMPO DE SOBREVIDA <- DataObito - DATAINITRT

## ESTDFIMT !=6 e DataObito < 31/12/2019 <- CENSURA
# TEMPO DE SOBREVIDA <- DataObito - DATAINITRT

## ESTDFIMT !=6 e DataObito > 31/12/2019 <- CENSURA
# TEMPO DE SOBREVIDA <- 31/12/2019 - DATAINITRT

## ESTDFIMT !=6 e DataObito = NA <- CENSURA
# TEMPO DE SOBREVIDA <- 31/12/2019 - DATAINITRT

# Supomos que ESTDFIMT igual a 6 é óbito por câncer

dataCorte <- as.Date("2019-12-31")

# Construindo a variável status
empilhado11 <- empilhado10 |> 
  mutate(status = case_when(
    ESTDFIMT != 6 ~ 0,
    ESTDFIMT == 6 & DataObito > dataCorte ~ 0,
    ESTDFIMT == 6 & DataObito <= dataCorte ~ 1
  ))

# Trocando as datas vazias pelo final da COORTE (Janela temporal)
empilhado11$DataObito[is.na(empilhado11$DataObito) == TRUE] = as.Date("2019-12-31")

# Calculando tempo de sobrevida
empilhado12 <- empilhado11 |> 
  mutate(sobrevida = case_when(
    DataObito > dataCorte ~ as.numeric(dataCorte - DataInitrt),
    DataObito <= dataCorte ~ as.numeric(DataObito - DataInitrt)))

# Calculando tempo de atraso até o início do tratamento
empilhado13 <- empilhado12 |> 
  mutate(atraso = DataInitrt - DtDiagno)

# Gravando dataframe no arquivo
write.csv2(empilhado13,file = "inca.csv")

# Terminação
