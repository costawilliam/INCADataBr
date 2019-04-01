numeroCasosPorRacaCor <- function(dfDados){
  library(ggplot2)

  df<-aggregate(data.frame(NroCasos = dfDados$RACACOR), list(RACACOR = dfDados$RACACOR), length)

  df$RACACOR <- converterFatorParaInteiro(df$RACACOR)
  df$RACACOR[df$RACACOR == 1] <- "Branca"
  df$RACACOR[df$RACACOR == 2] <- "Preta"
  df$RACACOR[df$RACACOR == 3] <- "Amarela"
  df$RACACOR[df$RACACOR == 4] <- "Parda"
  df$RACACOR[df$RACACOR == 5] <- "Indígena"
  df$RACACOR[df$RACACOR == 9] <- "Sem informação"

  df$RACACOR <- as.factor(df$RACACOR)

  plotNumeroCasosPorRacaCor <- ggplot(data=df, aes(x=RACACOR, y=NroCasos)) +
    geom_bar(stat="identity")

  plotNumeroCasosPorRacaCor
}

numeroCasosPorRacaCor(dfDados)
