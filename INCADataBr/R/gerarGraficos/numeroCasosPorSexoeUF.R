numeroCasosPorsexoeUF <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT sexo as sexo, ufuh as ufuh, count(*) AS NroCasos from tb_inca group by sexo, ufuh order by sexo, ufuh "

  df <- obterDados(query)

  df <- subset(df, df$sexo  == 1 | df$sexo == 2)

  df$sexo[df$sexo == 1] <- "Masculino"
  df$sexo[df$sexo == 2] <- "Feminino"

  casosMasculinos <-  subset(df, df$sexo == "Masculino")
  casosFemininos  <-  subset(df, df$sexo == "Feminino")

  if (nrow(casosMasculinos) != nrow(casosFemininos) &&
      nrow(casosMasculinos) > nrow(casosFemininos)) {
    for (i in c(1:nrow(casosMasculinos))) {
      if (!casosMasculinos$ufuh[i] %in% casosFemininos$ufuh) {
        casosFemininos <-
          rbind(casosFemininos,
                c("Feminino", casosMasculinos$ufuh[i], 0))
      }
    }
  } else if (nrow(casosMasculinos) != nrow(casosFemininos) &&
             nrow(casosMasculinos) < nrow(casosFemininos)) {
    for (i in c(1:nrow(casosFemininos))) {
      if (!casosFemininos$ufuh[i] %in% casosMasculinos$ufuh) {
        casosMasculinos <-
          rbind(casosMasculinos,
                c("Masculino", casosFemininos$ufuh[i], 0))
      }
    }
  }

  casosFemininos <- casosFemininos[order(casosFemininos$ufuh),]
  casosMasculinos <- casosMasculinos[order(casosMasculinos$ufuh),]

  if (params$type == "bar") {
    ufuh <- casosMasculinos$ufuh
    nroHomens <- as.numeric(casosMasculinos$nrocasos)
    nroMulheres <- as.numeric(casosFemininos$nrocasos)

    df <- data.frame(ufuh, nroHomens, nroMulheres)

    library(plotly)
    p <-
      plot_ly(
        df,
        x = ~ ufuh,
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
    dfDadosHomens <- subset(df, df$sexo == "Masculino")
    dfDadosMulheres <- subset(df, df$sexo == "Feminino")

    casosMasculinos["nrocasosUENCIA"] <- NA
    casosFemininos["nrocasosUENCIA"] <- NA

    #calcular percentual casos homens
    for (i in c(1:nrow(casosMasculinos))) {
      casosMasculinos$nrocasosUENCIA[i] = calcularPercentual(sum(dfDadosHomens$nrocasos),
                                                             casosMasculinos$nrocasos[i])
    }

    #calcular percentual casos mulheres
    for (i in c(1:nrow(casosFemininos))) {
      casosFemininos$nrocasosUENCIA[i] = calcularPercentual(sum(dfDadosMulheres$nrocasos),
                                                            casosFemininos$nrocasos[i])
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
        labels = ~ casosMasculinos$ufuh,
        values = casosMasculinos$nrocasos,
        name = "Homens",
        domain = list(row = 0, column = 0)
      ) %>%
      add_pie(
        data = casosFemininos,
        labels = ~ ufuh,
        values = ~ nrocasos,
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
