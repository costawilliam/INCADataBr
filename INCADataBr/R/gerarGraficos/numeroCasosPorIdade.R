numeroCasosPorIdade <- function(dfDados){
  library(plotly)

  df<-aggregate(data.frame(NroCasos = dfDados$IDADE), list(Idade = dfDados$IDADE), length)

  p <- plot_ly(df, x = ~df$Idade, y = ~df$NroCasos, type = "bar", color = I("black")) %>%
    layout(title = "Número de casos de Cancer em 2017 por idade",
           xaxis = list(title = "Idade"),
           yaxis = list(title = "Número de casos"))
  p
}
