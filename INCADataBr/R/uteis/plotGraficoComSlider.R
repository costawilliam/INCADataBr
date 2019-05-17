plotGraficoComSlider <- function(df, params) {
  library(plotly)
  p <- plot_ly(df, x = ~ df$var) %>%
    add_lines(y = ~ df$nrocasos) %>%
    layout(
      title = params$title,
      xaxis = list(
        title = params$titleX,
        rangeslider = list(type = "date")
      ),

      yaxis = list(title =  params$titleY)
    )

  p
}
