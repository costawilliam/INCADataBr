numeroCasosPorEstado <- function(dfDados){
  library(ggplot2)

  df<-aggregate(data.frame(NroCasos = dfDados$UFUH), list(Estados = dfDados$UFUH), length)

  p <- ggplot(data=df, aes(x=Estados, y=NroCasos)) +
    geom_bar(stat="identity")

  p
}
