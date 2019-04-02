numeroCasosPorSexoPizza <- function(dfDados, ...) {
  params = list(...)

  if (is.null(params$titleGraphic)) {
    params$titleGraphic <- "Número de casos por sexo"
  }

  if (is.null(params$titleX)) {
    params$titleX <- "Idade"
  }

  if (is.null(params$titleY)) {
    params$titleY <- "Número de casos"
  }

  if (is.null(params$type)) {
    params$type <- "bar"
  }

  if (is.null(params$colors)) {
    params$colors <-
      colors <-
      c(
        'rgb(0,0,0)',
        'rgb(128,0,0)',
        'rgb(255,0,0)',
        'rgb(255,255,0)',
        'rgb(128,128,0)',
        'rgb(255,0,255)',
        'rgb(128,255,128)',
        'rgb(255,255,128)',
        'rgb(0,0,255)',
        'rgb(0,128,0)',
        'rgb(0,255,255)',
        'rgb(0,255,0)',
        'rgb(0,255,128)',
        'rgb(128,255,255)',
        'rgb(0,0,128)',
        'rgb(128,0,128)',
        'rgb(128,0,255)',
        'rgb(128,128,128)',
        'rgb(128,128,255)',
        'rgb(0,128,128)',
        'rgb(255,128,128)',
        'rgb(255,0,128)',
        'rgb(128,255,0)',
        'rgb(0,128,255)',
        'rgb(255,128,0)',
        'rgb(255,128,255)'
      )
  }

  library(plotly)

  df <-
    aggregate(data.frame(Quantidade = dfDados$SEXO),
              list(SEXO = dfDados$SEXO),
              length)

  df$SEXO <- converterFatorParaInteiro(df$SEXO)
  df$SEXO[df$SEXO == 1] <- "Masculino"
  df$SEXO[df$SEXO == 2] <- "Feminino"

  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$SEXO,
        y = ~ df$Quantidade,
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
      df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$Quantidade[i])
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
        labels = ~ df$SEXO,
        values = ~ df$FREQUENCIA,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$SEXO, ' - ', df$FREQUENCIA, '%'),
        marker = list(
          colors = colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
        showlegend = TRUE
      ) %>%
      layout(
        title = 'Percentual de casos por Sexo',
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
