numeroCasosPorIdade <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$IDADE),
              list(Idade = dfDados$IDADE),
              length)

  df <- subset(df, converterFatorParaInteiro(df$Idade) < 150)

  df$Idade <- converterFatorParaInteiro(df$Idade)

  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$Idade,
        y = ~ df$NroCasos,
        type = params$type
      ) %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),

        yaxis = list(title = params$titleY)
      )
    p
  } else if (params$type == "pie") {
    df["FREQUENCIA"] <- NA

    for (i in c(1:nrow(df))) {
      df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
    }

    p <-
      plot_ly(
        df,
        labels = ~ df$Idade,
        values = ~ df$FREQUENCIA,
        type = params$type,
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$Idade, ' - ', df$NroCasos, 'casos'),
        marker = list(
          colors = colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
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
