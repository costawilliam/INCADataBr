numeroCasosPorGrupoEstagiamento <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$ESTADIAG),
              list(VAR = dfDados$ESTADIAG),
              length)

  df$VAR[df$VAR == "0"] <- "0"
  df$VAR[df$VAR == "1"] <- "1"
  df$VAR[df$VAR == "2"] <- "2"
  df$VAR[df$VAR == "3"] <- "3"
  df$VAR[df$VAR == "4"] <- "4"
  df$VAR[df$VAR == "8"] <- "8"
  df$VAR[df$VAR == "9"] <- "9"
  df$VAR[df$VAR == "A"] <- "A"
  df$VAR[df$VAR == "B"] <- "B"
  df$VAR[df$VAR == "C"] <- "C"
  df$VAR[df$VAR == "D"] <- "D"
  df$VAR[df$VAR == "Z"] <- "Z"

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message("Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }

}
