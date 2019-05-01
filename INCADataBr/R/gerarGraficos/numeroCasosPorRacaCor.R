numeroCasosPorRacaCor <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$RACACOR),
              list(VAR = dfDados$RACACOR),
              length)

  df$VAR[df$VAR == 1] <- "Branca"
  df$VAR[df$VAR == 2] <- "Preta"
  df$VAR[df$VAR == 3] <- "Amarela"
  df$VAR[df$VAR == 4] <- "Parda"
  df$VAR[df$VAR == 5] <- "Indígena"
  df$VAR[df$VAR == 9] <- "Sem informação"

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message("Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }
}
