#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeroCasosPorGrupoEstagiamento <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT ESTADIAG as VAR, count(*) AS NroCasos from tb_inca group by ESTADIAG order by ESTADIAG"

  df <- obterDados(query)

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message(
      "Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parÃ¢metro type como \"bar\" ou \"pie\"."
    )
  }

}
