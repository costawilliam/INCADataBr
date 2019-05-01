plotGraficoPizza <- function(df, params) {
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
      labels = ~ df$VAR,
      values = ~ df$FREQUENCIA,
      type = "pie",
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~ paste(df$VAR, ' - ', df$FREQUENCIA, '%'),
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
}
