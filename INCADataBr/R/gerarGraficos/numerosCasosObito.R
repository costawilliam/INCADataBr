numerosCasosObito <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)
  numeroMortos <- length(which(validarData(dfDados$DATAOBITO) == T))
  numeroVivos  <- length(which(validarData(dfDados$DATAOBITO) == F))

  if (params$type == "bar") {
    p <- plot_ly(
      x = c("Mortos", "Vivos"),
      y = c(numeroMortos, numeroVivos),
      type = params$type
    ) %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY)
      )

    p

  } else if (params$type == "pie") {
    PercentualMorte <- calcularPercentual(nrow(dfDados), numeroMortos)
    PercentualViva  <- calcularPercentual(nrow(dfDados), numeroVivos)


    params$colors <-
      c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)',
        'rgb(0,0,0)'
      )
    p <-
      plot_ly(
        df,
        labels = ~ c("Mortos", "Vivos"),
        values = ~ c(PercentualMorte, PercentualViva),
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        #text = ~ paste(df$TABAGISM, ' - ', df$FREQUENCIA, '%'),
        marker = list(
          colors = params$colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
        showlegend = TRUE
      ) %>%
      layout(
        title = 'Percentual de casos por tipo de consumo de tabaco',
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    p

  } else {
    message("Tipo de gráfico não indicado não é suportado por esta função")
    message("Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }

}
