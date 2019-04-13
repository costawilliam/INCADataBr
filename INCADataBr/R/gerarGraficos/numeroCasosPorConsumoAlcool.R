numeroCasosPorConsumoAlcool <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$ALCOOLIS),
              list(ALCOOLIS = dfDados$ALCOOLIS),
              length)

  df <- subset(df, df$ALCOOLIS != 0)

  df$ALCOOLIS <- converterFatorParaInteiro(df$ALCOOLIS)

  df$ALCOOLIS[df$ALCOOLIS == 1] <- "Nunca"
  df$ALCOOLIS[df$ALCOOLIS == 2] <- "Ex-consumidor"
  df$ALCOOLIS[df$ALCOOLIS == 3] <- "Sim"
  df$ALCOOLIS[df$ALCOOLIS == 4] <- "Não avaliado"
  df$ALCOOLIS[df$ALCOOLIS == 8] <- "Não se aplica"
  df$ALCOOLIS[df$ALCOOLIS == 9] <- "Sem informação"


  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$ALCOOLIS,
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

    colors <-
      c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)'
      )

    p <-
      plot_ly(
        df,
        labels = ~ df$ALCOOLIS,
        values = ~ df$FREQUENCIA,
        type = params$type,
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$ALCOOLIS, ' - ', df$FREQUENCIA, '%'),
        marker = list(
          colors = colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = TRUE
      ) %>%
      layout(
        title = params$title,
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
