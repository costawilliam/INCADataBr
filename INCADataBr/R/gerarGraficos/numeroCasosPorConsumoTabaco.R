numeroCasosPorConsumoTabaco <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$TABAGISM),
              list(VAR = dfDados$TABAGISM),
              length)

  df <- subset(df, df$VAR != 0)

  df$VAR[df$VAR == 1] <- "Nunca"
  df$VAR[df$VAR == 2] <- "Ex-consumidor"
  df$VAR[df$VAR == 3] <- "Sim"
  df$VAR[df$VAR == 4] <- "Não avaliado"
  df$VAR[df$VAR == 8] <- "Não se aplica"
  df$VAR[df$VAR == 9] <- "Sem informação"

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
