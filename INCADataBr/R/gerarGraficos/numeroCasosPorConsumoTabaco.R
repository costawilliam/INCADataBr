numeroCasosPorConsumoTabaco <- function(dfDados){
  library(ggplot2)

  df<-aggregate(data.frame(NroCasos = dfDados$TABAGISM), list(TABAGISM = dfDados$TABAGISM), length)

  df$TABAGISM <- converterFatorParaInteiro(df$TABAGISM)

  df$TABAGISM[df$TABAGISM == 1] <- "Nunca"
  df$TABAGISM[df$TABAGISM == 2] <- "Ex-consumidor"
  df$TABAGISM[df$TABAGISM == 3] <- "Sim"
  df$TABAGISM[df$TABAGISM == 4] <- "Não avaliado"
  df$TABAGISM[df$TABAGISM == 8] <- ".Não se aplica"
  df$TABAGISM[df$TABAGISM == 9] <- "Sem informação"

  df$TABAGISM <- as.factor(df$TABAGISM)

  pnumeroCasosPorTabagismo <- ggplot(data=df, aes(x=TABAGISM, y=NroCasos)) +
    geom_bar(stat="identity")

  pnumeroCasosPorTabagismo
}

numeroCasosPorConsumoTabaco(dfDados)
