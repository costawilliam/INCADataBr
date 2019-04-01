numeroCasosPorConsumoTabacoPizza <- function(dfDados){

  df<-aggregate(data.frame(NroCasos = dfDados$TABAGISM), list(TABAGISM = dfDados$TABAGISM), length)

  df$TABAGISM <- converterFatorParaInteiro(df$TABAGISM)
  df$TABAGISM[df$TABAGISM == 1] <- "Nunca"
  df$TABAGISM[df$TABAGISM == 2] <- "Ex-consumidor"
  df$TABAGISM[df$TABAGISM == 3] <- "Sim"
  df$TABAGISM[df$TABAGISM == 4] <- "Não avaliado"
  df$TABAGISM[df$TABAGISM == 8] <- "Não se aplica"
  df$TABAGISM[df$TABAGISM == 9] <- "Sem informação"

  df["FREQUENCIA"] <- NA

  for(i in c(1:nrow(df))){
    df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
  }

  library(plotly)

  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

  p <- plot_ly(df, labels = ~df$TABAGISM, values = ~df$FREQUENCIA, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(df$TABAGISM, ' - ', df$FREQUENCIA, '%'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = TRUE) %>%
    layout(title = 'Número de casos por consumo de Tabaco',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p

}
