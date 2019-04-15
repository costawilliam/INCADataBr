numeroCasosPorConsumoTabaco <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$TABAGISM),
              list(TABAGISM = dfDados$TABAGISM),
              length)

  df <- subset(df, df$TABAGISM != 0)

  df$TABAGISM <- converterFatorParaInteiro(df$TABAGISM)

  df$TABAGISM[df$TABAGISM == 1] <- "Nunca"
  df$TABAGISM[df$TABAGISM == 2] <- "Ex-consumidor"
  df$TABAGISM[df$TABAGISM == 3] <- "Sim"
  df$TABAGISM[df$TABAGISM == 4] <- "Não avaliado"
  df$TABAGISM[df$TABAGISM == 8] <- "Não se aplica"
  df$TABAGISM[df$TABAGISM == 9] <- "Sem informação"

  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$TABAGISM,
        y = ~ df$NroCasos,
        type = params$type
      ) %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY)
      )

    p
  }  else if (params$type == "pie") {
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
        labels = ~ df$TABAGISM,
        values = ~ df$FREQUENCIA,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$TABAGISM, ' - ', df$FREQUENCIA, '%'),
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
