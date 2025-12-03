# Universidade Federal Fluminense
# Instituto de matemática e estatística
# Departamento de estatística
# Iniciação científica
# Orientadora: Núbia Karla de Oliveira Almeida
# Bolsista FAPERJ: Pedro Frazão Dutra

## Carregamento dos pacotes necessários
library(tidyverse)
library(survival)
library(survminer)
library(gridExtra)

## Carregamentos dos dados
inca <- read.csv2("inca.csv")

# Filtros necessários
inca2 <- inca |>
  filter(
    COMPBIO %in% c("2", "3"),
    ESTCONJ %in% c("1", "2", "3", "4", "5"),
    INSTRUC %in% c("1", "2", "3", "4", "5", "6"),
    RACACOR %in% c("1", "2", "3", "4"),
    !ESTADIAM %in% c("99", "B2", "88"),
    !LATERALI %in% c("0","8","9"),
    !MAISUMTU %in% c("0","3"),
    !PRITRATH %in% c("9"),
    as.numeric(IDADE) >= 20
  ) |>
  mutate(IDADE = as.numeric(IDADE))

# Criando novas variáveis
inca3 <- inca2 |> 
  mutate(
    faixaEtaria = case_when(
      IDADE < 40 ~ "[20,40)",
      IDADE >= 40 & IDADE < 60 ~ "[40,60)",
      IDADE >= 60 ~ "60+"
    ),
    tempo = sobrevida / 30.4375,
    companheiro = case_when(
      ESTCONJ %in% c("1","3","4") ~ "Sem companheiro",
      ESTCONJ %in% c("2","5") ~ "Com companheiro"
    ),
    ESCOL = case_when(INSTRUC ==  1 ~ 1,
                      INSTRUC == 2 ~ 2,
                      INSTRUC == 3 | INSTRUC == 4 ~ 3,
                      INSTRUC == 5 | INSTRUC == 6 ~ 4),
    estadiamento = case_when(
      ESTADIAM == "0" ~ 0,
      ESTADIAM %in% c("1","1A","1B") ~ 1,
      ESTADIAM %in% c("2","2A","2B") ~ 2,
      ESTADIAM %in% c("3","3A","3B","3C") ~ 3,
      ESTADIAM %in% c("4","4A","4B") ~ 4))

# Criando as legendas
inca4 <- inca3 |> 
  mutate(
    escolaridade = case_when(
      ESCOL == "1" ~ "Nenhum",
      ESCOL == "2" ~ "Fund.Incompleto",
      ESCOL == "3" ~ "Fund. / Médio",
      ESCOL == "4" ~ "Sup. / Sup.Incompleto"
    ),
    racaCor = case_when(
      RACACOR == "1" ~ "Branca",
      RACACOR == "2" ~ "Preta",
      RACACOR == "3" ~ "Amarela",
      RACACOR == "4" ~ "Parda",
      RACACOR == "5" ~ "Indígena"
    ),
    estadoConjugal = case_when(
      ESTCONJ == "1" ~ "Solteiro",
      ESTCONJ == "2" ~ "Casado",
      ESTCONJ == "3" ~ "Viúvo",
      ESTCONJ == "4" ~ "Separado judicialmente",
      ESTCONJ == "5" ~ "União consensual"
    ),
    lateralidade = case_when(
      LATERALI == "1" ~ "Direita",
      LATERALI == "2" ~ "Esquerda",
      LATERALI == "3" ~ "Bilateral"
    ),
    maisumtu = case_when(
      MAISUMTU == "1" ~ "Não",
      MAISUMTU == "2" ~ "Sim"
    )
  ) 

inca4$escolaridade <- as.factor(inca4$escolaridade)

inca4$escolaridade <- factor(
  inca4$escolaridade,
  levels = c(
    "Nenhum",
    "Fund.Incompleto",
    "Fund. / Médio",
    "Sup. / Sup.Incompleto"
  ),
  ordered = TRUE
)

inca4 <- inca4 |> 
  filter(racaCor != "Amarela",
         racaCor != "Indígena")

inca5 <- inca4 |>
  dplyr::mutate(
    # Cria a nova coluna 'fezCirurgia'
    fezCirurgia = ifelse(
      # Condição: Se o caractere "2" for encontrado na coluna 'PRITRATH'
      # grepl("2", PRITRATH): retorna TRUE se "2" for encontrado
      grepl("2", PRITRATH),
      # Se TRUE (fez cirurgia)
      "Sim",
      # Se FALSE (não fez cirurgia)
      "Não"
    )
  )

inca5 <- inca5 |>
  dplyr::mutate(
    # Indicadora de Radioterapia: "3" em PRITRATH
    fezRadio = ifelse(
      grepl("3", PRITRATH),
      "Sim",
      "Não"
    ),
    # Indicadora de Quimioterapia: "4" em PRITRATH
    fezQuimio = ifelse(
      grepl("4", PRITRATH),
      "Sim",
      "Não"
    )
  )

# Sobrevivência geral
y <- Surv(inca5$tempo / 12, inca5$status)

km <- survfit(y ~ 1, data = inca5)

graf <- ggsurvplot(km, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = " ",
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30, "black"),
                    font.y = c(30, "black"),
                    lwd = 1.2,
                   legend = "none")

graf <- graf$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.94, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf

# Gráfico onde obtenho a sobrevivência em 5 anos.
km_summary <- summary(km, times = 5)

S_hat_5 <- km_summary$surv

S_hat_5_label <- round(S_hat_5, 3)

graf <- ggsurvplot(km, censor = FALSE,
                   xlab = "Anos", ylab = expression(hat(S)(t)),
                   ggtheme = theme_minimal(),
                   title = " ",
                   font.tickslab = c(11, "plain", "black"),
                   font.x = c(30, "black"),
                   font.y = c(30, "black"),
                   lwd = 1.2,
                   legend = "none",
                   conf.int = F)

graf <- graf$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.94, 1)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf <- graf +
  geom_segment(aes(x = 5, y = 0.94, xend = 5, yend = S_hat_5),
               linetype = "dashed", color = "blue", linewidth = 1)
graf <- graf +
  geom_segment(aes(x = 0, y = S_hat_5, xend = 5, yend = S_hat_5),
               linetype = "dashed", color = "blue", linewidth = 1)
graf <- graf +
  geom_point(aes(x = 5, y = S_hat_5), 
             color = "blue", 
             fill = "blue", 
             shape = 21,    
             size = 6)      
graf <- graf +
  annotate("text",
           x = 5.2, 
           y = S_hat_5 + 0.003, 
           label = sprintf("(5, %s)", S_hat_5_label),
           parse = FALSE, 
           color = "blue",
           size = 10,
           hjust = 0.05, 
           vjust = 0) 

graf

#################################
# # Gráfico segundo estadiamento
km1 <- survfit(y~inca5$estadiamento, data = inca5)

graf1 <- ggsurvplot(km1, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Estadiamento",
                    legend.labs = c("0", "1","2","3","4"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30, "black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf1 <- graf1$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.05),
                     limits = c(0.75, 1)) +
  scale_color_manual(values = c(
    "0" = "#fedc97",
    "1" = "#b5b682",
    "2" = "#7c9885",
    "3" = "#28666e",
    "4" = "#033f63"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 35, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
    )

graf1

logrank1 <- survdiff(y~inca5$estadiamento, data = inca5)
logrank1[["pvalue"]]
# p-valor < 0.01

#################################

# Gráfico segundo a ocorrência de mais um tumor
km2 <- survfit(y~inca5$maisumtu, data = inca5)

graf2 <- ggsurvplot(km2, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Múltiplos tumores",
                    legend.labs = c("Não","Sim"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30,"black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf2 <- graf2$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "Não" = "#0d1b2a",
    "Sim" = "#778da9"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 30, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5,size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
    )

graf2

logrank2 <- survdiff(y~inca5$maisumtu, data = inca5)
logrank2[["pvalue"]]
# p-valor < 0.05

#################################

# Gráfico segundo a lateralidade do tumor
km3 <- survfit(y~inca5$lateralidade, data = inca5)

graf3 <- ggsurvplot(km3, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Lateralidade",
                    legend.labs = c("Bilateral","Direita","Esquerda"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30, "black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf3 <- graf3$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.88, 1)) +
  scale_color_manual(values = c(
    "Bilateral" = "#EF476F",
    "Direita" = "#FFD166",
    "Esquerda" = "#06D6A0"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 30, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black"))

graf3

logrank3 <- survdiff(y~inca5$lateralidade, data = inca5)
logrank3[["pvalue"]]
# p-valor < 0.01

################################# 
# Gráfico segundo faixa etária
km4 <- survfit(y~inca5$faixaEtaria, data = inca5)

graf4 <- ggsurvplot(km4, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Faixa Etária",
                    legend.labs = c("[20,40)", "[40,60)","60+"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30, "black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf4 <- graf4$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "[20,40)" = "#EF476F",
    "[40,60)" = "#FFD166",
    "60+" = "#06D6A0"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 25, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf4

logrank4 <- survdiff(y~inca5$faixaEtaria, data = inca5)
logrank4[["pvalue"]]
# p-valor < 0.01

#################################

# Gráfico segundo a escolaridade
km5 <- survfit(y ~ escolaridade, data = inca5)

graf5 <- ggsurvplot(
  km5,
  censor = FALSE,
  xlab = "Anos",
  ylab = expression(hat(S)(t)),
  ggtheme = theme_minimal(),
  title = "Escolaridade",
  legend.labs = levels(inca5$escolaridade),
  font.tickslab = c(11, "plain", "black"),
  font.x = c(30, "black"),
  font.y = c(30, "black"),
  lwd = 1.2
)

graf5 <- graf5$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02), limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "Nenhum" = "#fedc97",
    "Fund.Incompleto" = "#b5b682",
    "Fund. / Médio" = "#7c9885",
    "Sup. / Sup.Incompleto" = "#28666e"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 25, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )
graf5

logrank5 <- survdiff(y~inca5$escolaridade, data = inca5)
logrank5[["pvalue"]]
# p-valor < 0.01

#################################
# Gráfico segundo a raça/cor
km6 <- survfit(y~inca5$racaCor, data = inca5)

graf6 <- ggsurvplot(km6, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Raça/Cor ",
                    legend.labs = c("Branca","Parda","Preta"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30, "black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf6 <- graf6$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "Branca" = "#c1c1c1",
    "Parda" = "#d16666",
    "Preta" = "#2c4251"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 25, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf6

logrank6 <- survdiff(y~inca5$racaCor, data = inca5)
logrank6[["pvalue"]]
# p-valor < 0.01

#################################
# Gráfico segundo o relacionamento romântico
km7 <- survfit(y~inca5$companheiro, data = inca5)

graf7 <- ggsurvplot(km7, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Estado conjugal",
                    legend.labs = c("Com companheiro","Sem companheiro"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30, "black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf7 <- graf7$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "Com companheiro" = "#fac05e",
    "Sem companheiro" = "#ee6352"
  )) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 25, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf7

logrank7 <- survdiff(y~inca5$companheiro, data = inca5)
logrank7[["pvalue"]]
# p-valor < 0.01

#################################

# Sobrevivência segundo a realização de cirurgia
km8 <- survfit(y~inca5$fezCirurgia, data = inca5)

graf8 <- ggsurvplot(km8, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Cirurgia",
                    legend.labs = c("Não","Sim"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30,"black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf8 <- graf8$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.84, 1)) +
  scale_color_manual(values = c(
    "Não" = "#0d1b2a",
    "Sim" = "#778da9"
  ))  +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 30, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
    )

graf8

logrank8 <- survdiff(y~inca5$fezCirurgia, data = inca5)
logrank8[["pvalue"]]
# p-valor < 0.01

# Sobrevivência segundo a realização de quimioterapia
km9 <- survfit(y~inca5$fezQuimio, data = inca5)

graf9 <- ggsurvplot(km9, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Quimioterapia",
                    legend.labs = c("Não","Sim"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30,"black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf9 <- graf9$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "Não" = "#0d1b2a",
    "Sim" = "#778da9"
  ))  +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 30, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf9

logrank9 <- survdiff(y~inca5$fezQuimio, data = inca5)
logrank9[["pvalue"]]
# p-valor < 0.01

# Sobrevivência segundo a realização de radioterapia
km10 <- survfit(y~inca5$fezRadio, data = inca5)

graf10 <- ggsurvplot(km10, censor = FALSE,
                    xlab = "Anos", ylab = expression(hat(S)(t)),
                    ggtheme = theme_minimal(),
                    title = "Radioterapia",
                    legend.labs = c("Não","Sim"),
                    font.tickslab = c(11, "plain", "black"),
                    font.x = c(30,"black"),
                    font.y = c(30, "black"),
                    lwd = 1.2)

graf10 <- graf10$plot +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_y_continuous(breaks = seq(0, 1, 0.02),
                     limits = c(0.92, 1)) +
  scale_color_manual(values = c(
    "Não" = "#0d1b2a",
    "Sim" = "#778da9"
  ))  +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 30, color = "black"),
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 30),
    axis.text.x = element_text(size = 30, color = "black"),
    axis.text.y = element_text(size = 30, color = "black")
  )

graf10

logrank10 <- survdiff(y~inca5$fezRadio, data = inca5)
logrank10[["pvalue"]]
# p-valor < 0.01

# Terminação
