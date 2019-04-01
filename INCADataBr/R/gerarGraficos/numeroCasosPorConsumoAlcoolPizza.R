numeroCasosPorConsumoAlcoolPizza <- function(dfDados){

  df<-aggregate(data.frame(NroCasos = dfDados$ALCOOLIS), list(ALCOOLIS = dfDados$ALCOOLIS), length)

  df$ALCOOLIS <- converterFatorParaInteiro(df$ALCOOLIS)
  df$ALCOOLIS[df$ALCOOLIS == 1] <- "Nunca"
  df$ALCOOLIS[df$ALCOOLIS == 2] <- "Ex-consumidor"
  df$ALCOOLIS[df$ALCOOLIS == 3] <- "Sim"
  df$ALCOOLIS[df$ALCOOLIS == 4] <- "Não avaliado"
  df$ALCOOLIS[df$ALCOOLIS == 8] <- "Não se aplica"
  df$ALCOOLIS[df$ALCOOLIS == 9] <- "Sem informação"

  df["FREQUENCIA"] <- NA

  for(i in c(1:nrow(df))){
    df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
  }

  library(plotly)

  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

  p <- plot_ly(df, labels = ~df$ALCOOLIS, values = ~df$FREQUENCIA, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(df$ALCOOLIS, ' - ', df$FREQUENCIA, '%'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = TRUE) %>%
    layout(title = 'Número de casos por consumo de Alcool',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p

}
