#' Title
#'
#' @param df
#' @param params
#'
#' @return
#' @export
#'
#' @examples
plotGraficoBarras <- function(df, params) {
  if (!require("plotly", character.only = TRUE)) {
    install.packages("plotly", dep = TRUE)
  }

  library(plotly)

    p <-
      plotly::plot_ly(
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
