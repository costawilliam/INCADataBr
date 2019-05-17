numeroCasosPorSexo <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT SEXO as VAR, count(*) AS NroCasos from tb_inca group by SEXO order by SEXO"

  df <- obterDados(query)

  df <- subset(df, df$var == 1 | df$var == 2)

  df$var[df$var == 1] <- "Masculino"
  df$var[df$var == 2] <- "Feminino"

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
