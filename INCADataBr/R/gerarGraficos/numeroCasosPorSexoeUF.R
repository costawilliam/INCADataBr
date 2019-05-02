numeroCasosPorSexoeUF <- function(dfDados, ...) {
  library(plyr)

  params <- tratarParametros(...)


  df <- count(dfDados, vars = c("SEXO", "UFUH"))

  df$SEXO[df$SEXO == 1] <- "Masculino"
  df$SEXO[df$SEXO == 2] <- "Feminino"

  casosMasculinos <-  subset(df, df$SEXO == "Masculino")
  casosFemininos  <-  subset(df, df$SEXO == "Feminino")

  if (nrow(casosMasculinos) != nrow(casosFemininos) &&
      nrow(casosMasculinos) > nrow(casosFemininos)) {
    for (i in c(1:nrow(casosMasculinos))) {
      if (!casosMasculinos$UFUH[i] %in% casosFemininos$UFUH) {
        casosFemininos <-
          rbind(casosFemininos,
                c("Feminino", casosMasculinos$UFUH[i], 0))
      }
    }
  } else if (nrow(casosMasculinos) != nrow(casosFemininos) &&
             nrow(casosMasculinos) < nrow(casosFemininos)) {
    for (i in c(1:nrow(casosFemininos))) {
      if (!casosFemininos$UFUH[i] %in% casosMasculinos$UFUH) {
        casosMasculinos <-
          rbind(casosMasculinos,
                c("Masculino", casosFemininos$UFUH[i], 0))
      }
    }
  }

  casosFemininos <- casosFemininos[order(casosFemininos$UFUH), ]
  casosMasculinos <- casosMasculinos[order(casosMasculinos$UFUH), ]

  if (params$type == "bar") {
    UFUH <- casosMasculinos$UFUH
    nroHomens <- as.numeric(casosMasculinos$freq)
    nroMulheres <- as.numeric(casosFemininos$freq)

    df <- data.frame(UFUH, nroHomens, nroMulheres)

    library(plotly)
    p <-
      plot_ly(
        df,
        x = ~ UFUH,
        y = ~ nroHomens,
        type = 'bar',
        name = 'Homens'
      ) %>%
      add_trace(y = ~ nroMulheres, name = 'Mulheres') %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY),
        barmode = 'stack'
      )

    p

  } else if (params$type == "pie") {
    dfDadosHomens <- subset(dfDados, dfDados$SEXO == 1)
    dfDadosMulheres <- subset(dfDados, dfDados$SEXO == 2)

    casosMasculinos["FREQUENCIA"] <- NA
    casosFemininos["FREQUENCIA"] <- NA

    #calcular percentual casos homens
    for (i in c(1:nrow(casosMasculinos))) {
      casosMasculinos$FREQUENCIA[i] = calcularPercentual(nrow(dfDadosHomens), casosMasculinos$freq[i])
    }

    #calcular percentual casos mulheres
    for (i in c(1:nrow(casosFemininos))) {
      casosFemininos$FREQUENCIA[i] = calcularPercentual(nrow(dfDadosMulheres), casosFemininos$freq[i])
    }

    colors <-
      c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)'
      )

    library(plotly)
    p <- plot_ly() %>%
      add_pie(
        data = casosMasculinos,
        labels = ~ casosMasculinos$UFUH,
        values = casosMasculinos$freq,
        name = "Homens",
        domain = list(row = 0, column = 0)
      ) %>%
      add_pie(
        data = casosFemininos,
        labels = ~ UFUH,
        values = ~ freq,
        name = "Mulheres",
        domain = list(row = 0, column = 1)
      ) %>%
      layout(
        title = "Title",
        showlegend = T,
        grid = list(rows = 1, columns = 2),
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
