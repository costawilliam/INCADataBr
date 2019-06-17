#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeroCasosPorIdade <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT IDADE as VAR, count(*) AS NroCasos from tb_inca group by IDADE order by IDADE"

  df <- obterDados(query)
  df <- subset(df, df$var < 150)

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else if (params$type == "slider") {
    plotGraficoComSlider(df, params)
  } else {
    message(
      "Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parÃ¢metro type como \"bar\" ou \"pie\"."
    )
  }
}
