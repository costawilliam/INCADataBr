plotGraficoPizza <- function(df, params) {
  library(plotly)

  df["FREQUENCIA"] <- NA

  for (i in c(1:nrow(df))) {
    df$FREQUENCIA[i] = calcularPercentual(sum(df$nrocasos), df$nrocasos[i])
  }

  if ("desc" %in% colnames(df))
  {
    p <-
      plot_ly(
        df,
        labels = ~ df$var,
        values = ~ df$FREQUENCIA,
        type = "pie",
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$var, ' - ', df$FREQUENCIA, '% - ', df$desc),
        marker = list(
          colors = params$colors,
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
  } else {
    p <-
      plot_ly(
        df,
        labels = ~ df$var,
        values = ~ df$FREQUENCIA,
        type = "pie",
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$var, ' - ', df$FREQUENCIA, '%'),
        marker = list(
          colors = params$colors,
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
  }
  p
}
