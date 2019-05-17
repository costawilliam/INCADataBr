numeroCasosPorConsumoAlcool <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT alcoolis as VAR, count(*) AS NroCasos from tb_inca group by alcoolis order by alcoolis"

  df <- obterDados(query)


  df <- subset(df, df$var != 0)

  df$var[df$var == 1] <- "Nunca"
  df$var[df$var == 2] <- "Ex-consumidor"
  df$var[df$var == 3] <- "Sim"
  df$var[df$var == 4] <- "Não avaliado"
  df$var[df$var == 8] <- "Não se aplica"
  df$var[df$var == 9] <- "Sem informação"

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
