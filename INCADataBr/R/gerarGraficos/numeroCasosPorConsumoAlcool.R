numeroCasosPorConsumoAlcool <- function(dfDados){
  library(ggplot2)

  df<-aggregate(data.frame(NroCasos = dfDados$ALCOOLIS), list(ALCOOLIS = dfDados$ALCOOLIS), length)

  df$ALCOOLIS <- converterFatorParaInteiro(df$ALCOOLIS)

  df$ALCOOLIS[df$ALCOOLIS == 1] <- "Nunca"
  df$ALCOOLIS[df$ALCOOLIS == 2] <- "Ex-consumidor"
  df$ALCOOLIS[df$ALCOOLIS == 3] <- "Sim"
  df$ALCOOLIS[df$ALCOOLIS == 4] <- "Não avaliado"
  df$ALCOOLIS[df$ALCOOLIS == 8] <- ".Não se aplica"
  df$ALCOOLIS[df$ALCOOLIS == 9] <- "Sem informação"

  df$ALCOOLIS <- as.factor(df$ALCOOLIS)

  pnumeroCasosPorAlcoolismo <- ggplot(data=df, aes(x=ALCOOLIS, y=NroCasos)) +
    geom_bar(stat="identity")

  pnumeroCasosPorAlcoolismo
}
