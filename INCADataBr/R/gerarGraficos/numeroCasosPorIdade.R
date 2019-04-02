numeroCasosPorIdade <- function(dfDados, ...) {
  params=list(...)

  if(is.null(params$titleGraphic)){
    params$titleGraphic <- "Número de casos por idade"
  }

  if (is.null(params$titleX)) {
    params$titleX <- "Idade"
  }

  if (is.null(params$titleY)) {
    params$titleY <- "Número de casos"
  }

  if (is.null(params$type)) {
    params$type <- "bar"
  }

  if(is.null(params$colors)){
    params$colors <- colors <- c('rgb(0,0,0)', 'rgb(128,0,0)', 'rgb(255,0,0)', 'rgb(255,255,0)', 'rgb(128,128,0)', 'rgb(255,0,255)', 'rgb(128,255,128)', 'rgb(255,255,128)', 'rgb(0,0,255)', 'rgb(0,128,0)', 'rgb(0,255,255)', 'rgb(0,255,0)', 'rgb(0,255,128)', 'rgb(128,255,255)', 'rgb(0,0,128)', 'rgb(128,0,128)', 'rgb(128,0,255)', 'rgb(128,128,128)', 'rgb(128,128,255)', 'rgb(0,128,128)', 'rgb(255,128,128)', 'rgb(255,0,128)', 'rgb(128,255,0)', 'rgb(0,128,255)', 'rgb(255,128,0)', 'rgb(255,128,255)')
  }

  library(plotly)

  df <-
    aggregate(data.frame(NroCasos = dfDados$IDADE),
              list(Idade = dfDados$IDADE),
              length)

  #quantidadeNA <- df$NroCasos[ df$Idade ==  888] + df$NroCasos[ df$Idade ==  999]

  df <- df[c(1:113),c(1,2)] #to do: remover registros com idade > X
  #df <- rbind(df, c(NA, quantidadeNA)) #To do

 df$Idade <- converterFatorParaInteiro(df$Idade)
 if (params$type == "bar") {
  p <-
    plot_ly(df,
            x = ~ df$Idade,
            y = ~ df$NroCasos,
            type = params$type) %>%
    layout(
      title = params$titleGraphic,
      xaxis = list(title = params$titleX),

      yaxis = list(title = params$titleY)
    )
  p
 } else if(params$type == "pie") {
   df["FREQUENCIA"] <- NA

   for (i in c(1:nrow(df))) {
     df$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), df$NroCasos[i])
   }

   p <-
     plot_ly(
       df,
       labels = ~ df$Idade,
       values = ~ df$FREQUENCIA,
       type = params$type,
       textposition = 'inside',
       textinfo = 'label+percent',
       insidetextfont = list(color = '#FFFFFF'),
       hoverinfo = 'text',
       text = ~ paste(df$Idade, ' - ', df$NroCasos, 'casos'),
       marker = list(
         colors = colors,
         line = list(color = '#FFFFFF', width = 1)
       ),
       #The 'pull' attribute can also be used to create space between the sectors
       showlegend = TRUE
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

numeroCasosPorIdade(dfDados, type = "bar")
