numeroCasosPorSexoPizza <- function(dfDados){

  df<-aggregate(data.frame(NroCasos = dfDados$SEXO), list(SEXO = dfDados$SEXO), length)

  df$SEXO <- converterFatorParaInteiro(df$SEXO)
  df$SEXO[df$SEXO == 1] <- "Masculino"
  df$SEXO[df$SEXO == 2] <- "Feminino"

  df["FREQUENCIA"] <- NA

  for(i in c(1:nrow(df))){
    df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
  }

  library(plotly)

  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

  p <- plot_ly(df, labels = ~df$SEXO, values = ~df$FREQUENCIA, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste(df$SEXO, ' - ', df$FREQUENCIA, '%'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = TRUE) %>%
    layout(title = 'Percentual de casos por Sexo',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  p

}
