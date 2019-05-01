numeroCasosPorIdade <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$IDADE),
              list(VAR = dfDados$IDADE),
              length)

  df <- subset(df,df$VAR < 150)

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message("Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }

}
