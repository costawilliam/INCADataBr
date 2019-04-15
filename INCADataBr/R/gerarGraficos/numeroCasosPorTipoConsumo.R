numeroCasosPorTipoConsumo <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  dftabaco <-
    subset(
      dfDados,
      converterFatorParaInteiro(dfDados$TABAGISM) == 3 &
        converterFatorParaInteiro(dfDados$ALCOOLIS) != 3
    )
  dfAlcool <-
    subset(
      dfDados,
      converterFatorParaInteiro(dfDados$TABAGISM) != 3 &
        converterFatorParaInteiro(dfDados$ALCOOLIS) == 3
    )
  dfAlcoolTabaco <-
    subset(
      dfDados,
      converterFatorParaInteiro(dfDados$TABAGISM) == 3 &
        converterFatorParaInteiro(dfDados$ALCOOLIS) == 3
    )
  dfOutros <-
    subset(
      dfDados,
      converterFatorParaInteiro(dfDados$TABAGISM) != 3 &
        converterFatorParaInteiro(dfDados$ALCOOLIS) != 3
    )

  dftabaco$tipoConsumo <- 1 #somente tabaco
  dfAlcool$tipoConsumo <- 2 #somente Alcool
  dfAlcoolTabaco$tipoConsumo <- 3 #Alcool e tabaco
  dfOutros$tipoConsumo <- 4 #outros


  dfTemp <-
    rbind.data.frame(dftabaco, dfAlcool, dfAlcoolTabaco, dfOutros)

  df <-
    aggregate(
      data.frame(NroCasos = dfTemp$tipoConsumo),
      list(tipoConsumo = dfTemp$tipoConsumo),
      length
    )

  df$tipoConsumo[df$tipoConsumo == 1] <- "Somente Tabaco"
  df$tipoConsumo[df$tipoConsumo == 2] <- "Somente Alcool"
  df$tipoConsumo[df$tipoConsumo == 3] <- "Alcool e Tabaco"
  df$tipoConsumo[df$tipoConsumo == 4] <- "Outros"

  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$tipoConsumo,
        y = ~ df$NroCasos,
        type = params$type
      ) %>%
      layout(
        title = params$title,
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
        labels = ~ df$tipoConsumo,
        values = ~ df$FREQUENCIA,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$tipoConsumo, ' - ', df$FREQUENCIA, '%'),
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
