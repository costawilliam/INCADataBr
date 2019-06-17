#' Title
#'
#' @param df
#' @param params
#'
#' @return
#' @export
#'
#' @examples
plotGraficoComSlider <- function(df, params) {
  if (!require("plotly", character.only = TRUE)) {
    install.packages("plotly", dep = TRUE)
  }

  library(plotly)

  p <- plotly::plot_ly(df, x = ~ df$var) %>%
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
