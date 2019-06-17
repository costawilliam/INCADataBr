#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeroCasosPorRacaCor <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT RACACOR as VAR, count(*) AS NroCasos from tb_inca group by RACACOR order by RACACOR"

  df <- obterDados(query)


  df$var[df$var == 1] <- "Branca"
  df$var[df$var == 2] <- "Preta"
  df$var[df$var == 3] <- "Amarela"
  df$var[df$var == 4] <- "Parda"
  df$var[df$var == 5] <- "Indígena"
  df$var[df$var == 9] <- "Sem informação"

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
