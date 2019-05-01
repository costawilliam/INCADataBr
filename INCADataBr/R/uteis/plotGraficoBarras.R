plotGraficoBarras <- function(df, params) {
  p <-
    plot_ly(
      df,
      x = ~ df$VAR,
      y = ~ df$NroCasos,
      type = "bar"
    ) %>%
    layout(
      title = params$title,
      xaxis = list(title = params$titleX),
      yaxis = list(title =  params$titleY)
    )

  p
}
