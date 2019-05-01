numeroCasosPorTipoConsumo <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  dftabaco <- subset(dfDados, dfDados$TABAGISM == 3 & dfDados$ALCOOLIS != 3)
  dfAlcool <- subset(dfDados, dfDados$TABAGISM != 3 & dfDados$ALCOOLIS == 3)
  dfAlcoolTabaco <- subset(dfDados, dfDados$TABAGISM == 3 & dfDados$ALCOOLIS == 3)
  dfOutros <- subset(dfDados, dfDados$TABAGISM != 3 & dfDados$ALCOOLIS != 3)

  dftabaco$tipoConsumo <- "Somente tabaco" #somente tabaco
  dfAlcool$tipoConsumo <- "Somente Alcool" #somente Alcool
  dfAlcoolTabaco$tipoConsumo <- "Alcool e tabaco" #Alcool e tabaco
  dfOutros$tipoConsumo <- "Outros" #outros

  dfTemp <-
    rbind.data.frame(dftabaco, dfAlcool, dfAlcoolTabaco, dfOutros)

  df <-
    aggregate(data.frame(NroCasos = dfTemp$tipoConsumo),
              list(VAR = dfTemp$tipoConsumo),
              length)

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
