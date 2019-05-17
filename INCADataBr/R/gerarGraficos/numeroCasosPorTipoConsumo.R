numeroCasosPorTipoConsumo <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT tabagism, alcoolis , count(*) AS NroCasos from tb_inca group by tabagism, alcoolis  order by tabagism, alcoolis "

  df <- obterDados(query)

  dftabaco <-
    subset(df, df$tabagism == 3 & df$alcoolis != 3)

  dfAlcool <-
    subset(df, df$tabagism != 3 & df$alcoolis == 3)

  dfAlcoolTabaco <-
    subset(df, df$tabagism == 3 & df$alcoolis == 3)

  dfOutros <-
    subset(df, df$tabagism != 3 & df$alcoolis != 3)

  dftabaco$tipoConsumo <- 1 #somente tabaco
  dfAlcool$tipoConsumo <- 2 #somente Alcool
  dfAlcoolTabaco$tipoConsumo <- 3 #Alcool e tabaco
  dfOutros$tipoConsumo <- 4 #outros

  dfTemp <-
    rbind.data.frame(dftabaco, dfAlcool, dfAlcoolTabaco, dfOutros)

  nrocasos <- c(
    sum(dfTemp$nrocasos[dfTemp$tipoConsumo == 1]),
    sum(dfTemp$nrocasos[dfTemp$tipoConsumo == 2]),
    sum(dfTemp$nrocasos[dfTemp$tipoConsumo == 3]),
    sum(dfTemp$nrocasos[dfTemp$tipoConsumo == 4])
  )

  var <- c("Somente Tabaco",
           "Somente Alcool",
           "Alcool e Tabaco",
           "Outros")

  df <- data.frame(nrocasos, var)

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
