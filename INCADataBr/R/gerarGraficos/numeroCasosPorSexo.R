numeroCasosPorSexo <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$SEXO),
              list(VAR = dfDados$SEXO),
              length)

  df$VAR[df$VAR == 1] <- "Masculino"
  df$VAR[df$VAR == 2] <- "Feminino"

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message("Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }
}
