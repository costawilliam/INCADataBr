#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeroCasosPorAno <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT dtpricon as var, count(*) AS nrocasos from tb_inca group by dtpricon order by dtpricon"

  df <- obterDados(query)

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  }
  else if (params$type == "slider") {
    plotGraficoComSlider(df, params)

  } else {
    message(
      "Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parÃ¢metro type como \"bar\", \"pie\" ou \"slider\"."
    )
  }
}
