numeroCasosPorGrupoEstagiamento <- function(dfDados, ...){
  library(plotly)
  
  params <- tratarParametros(...)
  
  df<-aggregate(data.frame(NroCasos = dfDados$ESTADIAG), list(ESTADIAG = dfDados$ESTADIAG), length)
  
  df$ESTADIAG <- converterFatorParaCaracter(df$ESTADIAG)
  
  df$ESTADIAG[df$ESTADIAG == "0"] <- "0"
  df$ESTADIAG[df$ESTADIAG == "1"] <- "1"
  df$ESTADIAG[df$ESTADIAG == "2"] <- "2"
  df$ESTADIAG[df$ESTADIAG == "3"] <- "3"
  df$ESTADIAG[df$ESTADIAG == "4"] <- "4"
  df$ESTADIAG[df$ESTADIAG == "8"] <- "8"
  df$ESTADIAG[df$ESTADIAG == "9"] <- "9"
  df$ESTADIAG[df$ESTADIAG == "A"] <- "A"
  df$ESTADIAG[df$ESTADIAG == "B"] <- "B"
  df$ESTADIAG[df$ESTADIAG == "C"] <- "C"
  df$ESTADIAG[df$ESTADIAG == "D"] <- "D"
  df$ESTADIAG[df$ESTADIAG == "Z"] <- "Z"
  
  if (params$type == "bar") {
    p <- plot_ly(df, x = ~df$ESTADIAG, y = ~df$NroCasos, type = 'bar', color = I("black")) %>%
      layout(title = "título",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
    p
    
  } else if(params$type == "pie") {
    df["FREQUENCIA"] <- NA
    
    for (i in c(1:nrow(df))) {
      df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
    }
    
    params$colors <-
      c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)',
        'rgb(0,0,0)'
      )
    p <-
      plot_ly(
        df,
        labels = ~ df$ESTADIAG,
        values = ~ df$FREQUENCIA,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$ESTADIAG, ' - ', df$NroCasos, ' casos'),
        marker = list(
          colors = params$colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
        #The 'pull' attribute can also be used to create space between the sectors
        showlegend = FALSE
      ) %>%
      layout(
        title = params$titleGraphic,
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      )
    p
  } else {
    message("Tipo de gráfico não indicado não é suportado por esta função")
    message("Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }
  
}
