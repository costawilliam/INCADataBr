numeroCasosPorRacaCorPizza <- function(dfDados){

  df<-aggregate(data.frame(NroCasos = dfDados$RACACOR), list(RACACOR = dfDados$RACACOR), length)

  df$RACACOR <- converterFatorParaInteiro(df$RACACOR)
  df$RACACOR[df$RACACOR == 1] <- "Branca"
  df$RACACOR[df$RACACOR == 2] <- "Preta"
  df$RACACOR[df$RACACOR == 3] <- "Amarela"
  df$RACACOR[df$RACACOR == 4] <- "Parda"
  df$RACACOR[df$RACACOR == 5] <- "Indígena"
  df$RACACOR[df$RACACOR == 9] <- "Sem informação"

  df["FREQUENCIA"] <- NA

  for(i in c(1:nrow(df))){
    df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
  }

  library(plotly)

  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

  p <- plot_ly(df, labels = ~df$RACACOR, values = ~df$FREQUENCIA, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(df$RACACOR, ' - ', df$FREQUENCIA, '%'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE) %>%
    layout(title = 'Percentual de casos por Raça/Cor',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p

}
