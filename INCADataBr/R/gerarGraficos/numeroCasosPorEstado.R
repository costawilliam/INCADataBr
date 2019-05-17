numeroCasosPorEstado <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT UFUH as VAR, count(*) AS NroCasos from tb_inca group by UFUH order by UFUH"

  df <- obterDados(query)

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message(
      "Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parâmetro type como \"bar\" ou \"pie\"."
    )
  }

}
