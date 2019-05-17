plotGraficoBarras <- function(df, params) {
  library(plotly)

    p <-
      plot_ly(
        df,
        x = ~ df$var,
        y = ~ df$nrocasos,
        type = "bar",
        text = df$desc,
        showlegend = F
      ) %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY)
      )

  p
}
