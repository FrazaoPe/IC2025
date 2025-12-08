library(shiny)
library(DT)

ui <- navbarPage(
  "Análise de sobrevivência",
  
  tabPanel(
    "Estimador de Kaplan-Meier",
    
    fluidRow(
      column(
        width = 3,
        wellPanel(
          selectInput(
            inputId = "sur_var",
            label = "Selecione a variável:",
            choices = c(
              "Escolaridade"       = "escolaridade",
              "Situação romântica" = "companheiro",
              "Raça/Cor"           = "racaCor",
              "Faixa Etária"       = "faixaEtaria",
              "Estadiamento Clínico" = "estadiamento",
              "Lateralidade do tumor" = "lateralidade",
              "Ocorrência de mais um tumor" = "maisumtu",
              "Fez cirurgia?" = "fezCirurgia",
              "Fez radioterapia?" = "fezRadio",
              "Fez quimioterapia?" = "fezQuimio",
              "Atraso para início do tratamento" = "hiato",
              "Histórico familiar de câncer" = "historico"
            )
          ),
          sliderInput(
            'xvalue',
            'Meses de sobrevivência = ',
            value = 100,
            min = 1,
            max = round(max(base$tempo))
          )
        ),
        div(style = "font-size: 15px",
            tableOutput("center")),
        div(style = "font-size: 18px",
            textOutput("LogR")
        )
      ),
      
      column(
        width = 9,
        plotOutput("plot1", height = "650px", width = "100%")
      )
    )
  )
)