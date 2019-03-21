numeroCasosPorIdade <- function(dfDados){
  library(ggplot2)

  df-aggregate(data.frame(NroCasos = dfDados$IDADE), list(Idade = dfDados$IDADE), length)

  p - ggplot(data=df, aes(x=Idade, y=NroCasos)) +
    geom_bar(stat=identity)

  p
}