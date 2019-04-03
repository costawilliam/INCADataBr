numeroCasosPorRacaCor <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$RACACOR),
              list(RACACOR = dfDados$RACACOR),
              length)

  df$RACACOR <- converterFatorParaInteiro(df$RACACOR)
  df$RACACOR[df$RACACOR == 1] <- "Branca"
  df$RACACOR[df$RACACOR == 2] <- "Preta"
  df$RACACOR[df$RACACOR == 3] <- "Amarela"
  df$RACACOR[df$RACACOR == 4] <- "Parda"
  df$RACACOR[df$RACACOR == 5] <- "Indígena"
  df$RACACOR[df$RACACOR == 9] <- "Sem informação"

  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$RACACOR,
        y = ~ df$NroCasos,
        type = params$type
      ) %>%
      layout(
        title = params$titleGraphic,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY)
      )

    p

  } else if (params$type == "pie") {
    df["FREQUENCIA"] <- NA

    for (i in c(1:nrow(df))) {
      df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
    }

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
        labels = ~ df$RACACOR,
        values = ~ df$FREQUENCIA,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$RACACOR, ' - ', df$FREQUENCIA, '%'),
        marker = list(
          colors = params$colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = TRUE
      ) %>%
      layout(
        title = 'Percentual de casos por Raça/Cor',
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
