library(shiny)
library(bslib)
library(survival)
library(tidyverse)
library(plotly)
  
if (file.exists("baseDashboard.csv")) {
  base <- read_csv2("baseDashboard.csv")
  
  base$escolaridade <- factor(base$escolaridade, levels = c(
    "Nenhum", 
    "Fund.Incompleto", 
    "Fund. / Médio", 
    "Sup. / Sup.Incompleto"
  ))
}

ui <- page_navbar(
  title = "DashSurvivalBoard",
  theme = bs_theme(version = 5, bootswatch = "minty"), 
  
  fillable = TRUE, 
  
  # --- PÁGINA 1: ANÁLISE KM ---
  nav_panel(
    title = "Método de Kaplan-Meier",
    icon = icon("laptop-medical"),
    navset_card_tab(
      height = "100%",
      full_screen = TRUE, 
      
      sidebar = sidebar(
        width = 300,
        title = "Configurações",
        
        selectInput(
          inputId = "sur_var",
          label = "Variável de estratificação",
          choices = c(
            "Escolaridade"                   = "escolaridade",
            "Estadiamento clínico"           = "estadiamento",
            "Faixa Etária"                   = "faixaEtaria",
            "Fez cirurgia?"                  = "fezCirurgia",
            "Fez quimioterapia?"             = "fezQuimio",
            "Fez radioterapia?"              = "fezRadio",
            "Lateralidade do tumor"          = "lateralidade",
            "Ocorrência de mais um tumor"    = "maisumtu",
            "Raça/Cor"                       = "racaCor",
            "Situação romântica"             = "companheiro",
            "Tempo até início do tratamento" = "atrasoTrat",
            "Tempo até o diagnóstico"        = "atrasoDiag"
          ),
          selected = "escolaridade"
        ),
        
        sliderInput(
          inputId = "xvalue",
          label = "Ano de acompanhamento (t)",
          value = 2,
          min = 0,
          max = 10, 
          step = 0.1
        ),
        
        hr(), 
        
        # Resultado do Log-Rank
        div(
          class = "alert alert-light", 
          style = "border: 1px solid #ddd; padding: 10px;",
          h6("Teste Log-Rank", style = "margin-top: 0; font-weight: bold;"),
          uiOutput("LogR")
        )
      ),
      
      # Aba 1: Gráfico
      nav_panel(
        title = "Curva",
        plotOutput("plot1", height = "100%") 
      ),
      
      # Aba 2: Tabela
      nav_panel(
        title = "Valores",
        h4("Estimativas da função de sobrevivência"),
        tableOutput("center")
      )
    )
  ),
  
  # --- PÁGINA 2: ANÁLISE DESCRITIVA ---
  nav_panel(
    title = "Análise Descritiva",
    icon = icon("chart-pie"), 
    
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        title = "Configurações",
        selectInput(
          inputId = "sur_var2", 
          label = "Variável:", 
          choices = c(
            "Escolaridade"                   = "escolaridade",
            "Estadiamento clínico"           = "estadiamento",
            "Faixa Etária"                   = "faixaEtaria",
            "Fez cirurgia?"                  = "fezCirurgia",
            "Fez quimioterapia?"             = "fezQuimio",
            "Fez radioterapia?"              = "fezRadio",
            "Lateralidade do tumor"          = "lateralidade",
            "Ocorrência de mais um tumor"    = "maisumtu",
            "Raça/Cor"                       = "racaCor",
            "Situação romântica"             = "companheiro",
            "Tempo até início do tratamento" = "atrasoTrat",
            "Tempo até o diagnóstico"        = "atrasoDiag"
          ), 
          selected = "escolaridade"
        ),
      ),
      
      
      # 1. LINHA DAS VALUE BOXES
      layout_columns(
        fill = FALSE, 
        height = "120px",
        
        value_box(
          title = "Total de mulheres no estudo",
          value = textOutput("vb_total"),
          showcase = icon("person-dress"),
          theme = "primary"
        ),
        
        value_box(
          title = "Quantidade de óbitos ocorridos no estudo",
          value = textOutput("vb_obitos"),
          showcase = icon("heart-pulse"),
          theme = "danger"
        ),
      ),
      
      br(), 
      
      # 2. LINHA DO GRÁFICO E TABELA
      layout_columns(
        col_widths = c(8, 4), 
        
        card(
          full_screen = TRUE, 
          card_header("Taxa de mortalidade por estrato"),
          plotlyOutput("plot2")
        ),
        
        card(
          full_screen = TRUE,
          card_header("Distribuição de frequências da variável"),
          div(style = "overflow-y: auto; max-height: 400px;", 
              tableOutput("table_desc")
          )
        )
      )
    )
  ),
  
  nav_spacer(),
  
  # --- PÁGINA 3: SOBRE / CONTATO ---
  nav_panel(
    title = "Sobre",
    icon = icon("circle-info"), 
    
    fluidRow(
      column(
        width = 6, offset = 3, 
        
        card(
          class = "text-center", 
          style = "margin-top: 50px; margin-bottom: 50px;", 
          card_header("Sobre este Dashboard", class = "bg-secondary text-white"),
          
          card_body(
            h4("Análise de Sobrevivência de Mulheres em Tratamento contra Câncer de Mama"),
            p("Este painel interativo foi desenvolvido para visualizar o comportamento da função de sobrevivência de mulheres de todo o Brasil em seu primeiro tratamento contra câncer de mama, de forma geral e por estratos. O período de análise considerado foram os anos de 2010 a 2019. 
              Os dados utilizados estão disponíveis de forma pública no site Integrador RHC, do INCA. Para maiores informações sobre a metodologia utilizada e a manipulação realizada no banco de dados, confira o repositório deste projeto no github do autor.", 
              style = "color: #6c757d; text-align: justify;"),
            
            hr(),
            
            # --- BLOCO DO DESENVOLVEDOR ---
            h5("Autor"),
            strong("Pedro Frazão Dutra", style = "font-size: 1.2rem;"),
            br(),
            div(
              style = "display: flex; justify-content: center; gap: 20px;",
              tags$a(
                href = "https://github.com/FrazaoPe/IC2025.git", 
                target = "_blank",
                class = "btn btn-outline-dark btn-sm", 
                icon("github"), " GitHub"
              ),
              tags$a(
                href = "mailto:dutra.frazao.pedro@gmail.com", 
                class = "btn btn-outline-dark btn-sm",
                icon("envelope"), " Contato"
              )
            ),
            
            br(), 
            
            # --- BLOCO DA ORIENTADORA ---
            h5("Supervisão"),
            tags$a(
              href = "http://lattes.cnpq.br/7953196264864761", 
              target = "_blank",
              style = "font-size: 1.2rem; font-weight: bold; color: #2c3e50; text-decoration: none;",
              "Núbia Almeida",
              icon("external-link-alt", style = "font-size: 0.8rem; margin-left: 5px;")
            ),
            
            # --- BLOCO DA FAPERJ ---
            br(), hr(), 
            
            h6("Apoio Financeiro", style = "color: #6c757d; margin-bottom: 15px;"),
            
            tags$a(
              href = "https://www.faperj.br/", 
              target = "_blank",
              tags$img(
                src = "logo_faperj.jpg", 
                width = "180px",
                alt = "Logo FAPERJ"
              )
            ),
            
            br(), br(),
            p("Qualquer dúvida, sugestão ou feedback, entre em contato com o autor.", 
              style = "font-size: 0.9rem; font-style: italic; color: #adb5bd;")
          )
        )
      )
    )
  )
)

# ==============================================================================
# 3. SERVIDOR (SERVER)
# ==============================================================================

server <- function(input, output, session) {
  
  # --- Reativos ---

  selectedData <- reactive({
    req(input$sur_var %in% names(base))
    
    var_col <- base[[input$sur_var]]
    if (!is.factor(var_col)) {
      var_col <- factor(var_col) 
    }
    var_col
  })
  
  t_meses <- reactive({
    input$xvalue * 12
  })
  
  runSur <- reactive({
    req(input$sur_var)
    survfit(as.formula(paste("Surv(tempo,status) ~ ", paste(input$sur_var))),
            data = base)
  })
  
  runLogR <- reactive({
    req(input$sur_var)
    survdiff(as.formula(paste("Surv(tempo,status) ~ ", paste(input$sur_var))),
             data = base)
  })
  
  # --- Value Boxes ---
  
  output$vb_total <- renderText({
    format(nrow(base), big.mark = ".", decimal.mark = ",")
  })
  
  output$vb_obitos <- renderText({
    n_obitos <- sum(base$status == 1, na.rm = TRUE)
    format(n_obitos, big.mark = ".", decimal.mark = ",")
  })
  
  output$table_desc <- renderTable({
    req(input$sur_var2)
    total_pacientes <- nrow(base)
    
    base |> 
      group_by(.data[[input$sur_var2]]) |> 
      summarise(
        `N` = n(),
        `%` = (n() / total_pacientes) * 100
      ) |> 
      mutate(
        `%` = paste0(format(round(`%`, 1), decimal.mark = ","), "%"),
        `N` = as.integer(`N`)
      ) |>  
      rename("Estrato" = input$sur_var2)
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # --- Outputs ---
  
  output$LogR <- renderUI({
    tryCatch({
      pvalor <- runLogR()[["pvalue"]]
      p_fmt <- format.pval(pvalor, digits = 3, eps = 0.001)
      
      div(
        style = "font-size: 20px; font-weight: bold; color: #2c3e50;",
        paste0("p-valor ", p_fmt)
      )
    }, error = function(e) {
      "Não calculável"
    })
  })
  
  output$plot1 <- renderPlot({
    fit <- runSur()
    
    surv_df <- data.frame(
      time   = fit$time / 12,   
      surv   = fit$surv,
      lower  = fit$lower,
      upper  = fit$upper,
      strata = rep(names(fit$strata), fit$strata)
    )
    
    surv_df$strata <- gsub(paste0(input$sur_var, "="), "", surv_df$strata)
    
    y_lim <- if (input$sur_var == "estadiamento") {
      c(0.75, 1)
    } else if (input$sur_var == "fezCirurgia") {
      c(0.84, 1)
    } else if (input$sur_var == "lateralidade") {
      c(0.85, 1)
    } else {
      c(0.9, 1) 
    }
    
    ggplot(surv_df, aes(x = time, y = surv, color = strata, fill = strata)) +
      geom_ribbon(
        aes(ymin = lower, ymax = upper),
        alpha = 0.20,
        linewidth = 0
      ) +
      geom_step(linewidth = 1.2) +
      geom_vline(
        xintercept = input$xvalue,
        linetype = "longdash", 
        color = "#555555",     
        alpha = 0.6,           
        linewidth = 0.8        
      ) +
      scale_x_continuous(
        name = "t (ano)",
        breaks = scales::pretty_breaks(10)
      ) +
      scale_y_continuous(
        name = expression(hat(S)(t)),
        limits = y_lim
      ) +
      theme_minimal() +
      scale_color_manual(
        values = c("#20C997", "#FF8882", "#5E81AC", "#B48EAD", "#F39C12")
      ) +
      
      scale_fill_manual(
        values = c("#20C997", "#FF8882", "#5E81AC", "#B48EAD", "#F39C12")
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        legend.text = element_text(size = 16)
      )
  })
  
  output$center <- renderTable({
    tryCatch({
      s <- summary(runSur(), times = t_meses())
      
      tab <- data.frame(
        Estrato = gsub(paste0(input$sur_var, "="), "", s$strata),
        `S(t)` = round(s$surv, 3),
        `IC 95%` = paste0("(", round(s$lower, 3), " ; ", round(s$upper, 3), ")"),
        check.names = F
      )
      tab
    }, error = function(e) {
      data.frame(Erro = "Tempo selecionado fora do intervalo ou dados insuficientes.")
    })
  })
  
  output$plot2 <- renderPlotly({
    req(input$sur_var2)
    
    limite_max <- 10 
    
    if (input$sur_var2 == "fezCirurgia") {
      limite_max <- 15
    } else if (input$sur_var2 == "estadiamento") {
      limite_max <- 20
    }
    
    resumo_risco <- base |> 
      group_by(.data[[input$sur_var2]]) |> 
      summarise(
        total_grupo = n(),                            
        obitos = sum(status == 1, na.rm = TRUE)       
      ) |> 
      mutate(
        taxa_obito = (obitos / total_grupo) * 100     
      ) 
    
    p <- resumo_risco |> 
      ggplot(aes(x = .data[[input$sur_var2]], 
                 y = taxa_obito,
                 text = paste0("<b>Estrato: ", .data[[input$sur_var2]], "</b><br>",
                               "Óbitos ocorridos: ", obitos, "<br>",
                               "Percentual: ", round(taxa_obito, 2), "%"))) +
      geom_col(fill = "#FF8882", color = "white") + 
      scale_y_continuous(limits = c(0,limite_max)) + 
      labs(x = NULL, y = "Porcentagem (%)") +
      
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(hjust = 1, face = "bold", color = "black", size = 11),
        axis.text.y = element_text(face = "bold", color = "black")
      )
    
    ggplotly(p, tooltip = "text") |> 
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)

