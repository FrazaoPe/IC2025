function(input, output, session) {
  
  # A. Reativa que seleciona e garante que a variável seja um FATOR
  selectedData <- reactive({
    var_col <- base[[input$sur_var]]
    if (!is.factor(var_col)) {
      # Converte para fator. Isso é importante para survfit e para os levels().
      var_col <- factor(var_col) 
    }
    var_col
  })
  
  runSur <- reactive({
    survfit(as.formula(paste("Surv(tempo,status) ~ ",paste(input$sur_var))),
            data=base)
  })
  
  runLogR <- reactive({
    survdiff(as.formula(paste("Surv(tempo,status) ~ ",paste(input$sur_var))),
             data=base)
  })
  
  output$LogR <- renderText({
    pvalor <- runLogR()[["pvalue"]]
    paste("Valor-p do teste de logrank:", signif(pvalor,4))
  })
  
  
  output$plot1 <- renderPlot({
    
    y_lim <- if (input$sur_var == "estadiamento") {
      c(0.75, 1)
    } else if (input$sur_var == "fezCirurgia") {
      c(0.84, 1)
    } else if (input$sur_var == "lateralidade") {
      c(0.88, 1)
    } else {
      c(0.9, 1)
    }
    
    # -----------------------------------------------------------------
    # CORREÇÃO DA LEGEND: Extrai os níveis (categorias) para a legenda.
    # -----------------------------------------------------------------
    legenda_textos <- levels(selectedData())
    
    # Fallback: Se não for um fator com níveis definidos, usa valores únicos.
    if (is.null(legenda_textos)) {
      legenda_textos <- sort(unique(selectedData()))
    }
    
    # Garante que o número de cores seja igual ao número de categorias
    cores_disponiveis <- c("red","skyblue","green","purple","orange","yellow")
    cores_plot <- cores_disponiveis[1:length(legenda_textos)]
    # -----------------------------------------------------------------
    
    plot(runSur(),
         col = cores_plot, # Usa as cores ajustadas
         xlab="Meses", 
         ylab="S(t)",
         ylim = y_lim,
         cex.axis = 1.2,
         cex.lab = 1.2,
         bty = "n",
         lwd = "3")
    
    legend("bottomleft",cex=1.2,
           legend = legenda_textos, # Usa os nomes das categorias
           fill= cores_plot) # Usa as cores ajustadas
    
    abline(v=input$xvalue,col=1,lty=2)
  })
  
  
  output$center <- renderTable({
    as.data.frame(summary(runSur(), 
                          times=input$xvalue )[c("surv", "time", "strata")])
  })
  
  output$plot2 <- renderPlot({
    total <- nrow(base)
    
    resumo <- base |> 
      group_by(.data[[input$sur_var2]]) |> 
      summarise(freq = n() / total) |>
      mutate(freq = freq * 100) |> 
      arrange(freq)
    
    resumo |> 
      ggplot(aes(x = reorder(.data[[input$sur_var2]], +freq), 
                 y = freq)) +
      geom_bar(fill = "#2BF0AD",
               color = "black",
               stat = "identity") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      geom_text(aes(label = paste0(round(freq, 0), "%")),
                hjust = 1.2 ,
                fontface = "bold",
                size = 5) +
      labs(x = " ", y = " ") +
      theme_classic() +
      theme(
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)
      ) +
      coord_flip()
  })
}