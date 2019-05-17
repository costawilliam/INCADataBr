numerosCasosObito <- function(...) {
  library(plotly)
  params <- tratarParametros(...)

  query <-
    "SELECT Extract(year FROM To_date(dataobito, 'DD/MM/YYYY')) AS var,
       Count(*) as nrocasos
    FROM   tb_inca
    WHERE  Length(dataobito) = 10
       AND Extract(year FROM To_date(dataobito, 'DD/MM/YYYY')) BETWEEN Extract(
           year FROM timestamp '1985-01-01 00:00:00') AND Extract(
           year FROM Now())
       AND Extract(year FROM To_date(dataobito, 'DD/MM/YYYY')) < 2017
    GROUP  BY var"

  dfNumeroMortos <- obterDados(query)

  query <-
    "SELECT dtpricon as var,
       Count(*) as nrocasos
    FROM   tb_inca
    GROUP  BY var"

  dfNumeroCasosPorAno <- obterDados(query)

  dfNumeroMortos$var <- as.character(dfNumeroMortos$var)
  dfNumeroCasosPorAno$var <- as.character(dfNumeroCasosPorAno$var)
  dfNumeroMortos$var <- as.numeric(dfNumeroMortos$nrocasos)
  dfNumeroCasosPorAno$var <- as.character(dfNumeroCasosPorAno$nrocasos)

  if (nrow(dfNumeroMortos) != nrow(dfNumeroCasosPorAno) &&
      nrow(dfNumeroMortos) < nrow(dfNumeroCasosPorAno)) {
    for (i in c(1:nrow(dfNumeroCasosPorAno))) {
      if (!dfNumeroCasosPorAno$var[i] %in% dfNumeroMortos$var) {
        dfNumeroMortos <-
          rbind(dfNumeroMortos,
                c(dfNumeroCasosPorAno$var[i], 0))
      }
    }
  }

  dfNumeroMortos <- dfNumeroMortos[order(dfNumeroMortos$var), ]
  dfNumeroCasosPorAno <-
    dfNumeroCasosPorAno[order(dfNumeroCasosPorAno$var), ]

  if (params$type == "bar") {
    casos  <- dfNumeroCasosPorAno$nrocasos


    df <-
      data.frame(
        var = dfNumeroMortos$var,
        vivos = dfNumeroCasosPorAno$nrocasos - dfNumeroMortos$nrocasos,
        mortos = dfNumeroMortos$nrocasos
      )

    p <-
      plot_ly(
        df,
        x = ~ var,
        y = ~ vivos,
        type = 'bar',
        name = 'Vivos'
      ) %>%
      add_trace(y = ~ mortos, name = 'Mortos') %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY),
        barmode = 'stack'
      )

    p
  } else if (params$type == "pie") {
    dfMortos <- data.frame(
      ano = dfNumeroMortos$var ,
      numeroMortes = dfNumeroMortos$nrocasos,
      totalCasosAno =  dfNumeroCasosPorAno$nrocasos
    )


    dfVivos  <- data.frame(
      ano = dfNumeroMortos$var ,
      numeroVivos  = dfNumeroCasosPorAno$nrocasos - dfNumeroMortos$nrocasos,
      totalCasosAno =  dfNumeroCasosPorAno$nrocasos
    )


    #calcular percentual MORTES
    for (i in c(1:nrow(dfMortos))) {
      dfMortos$PercentualMortesAno[i] = calcularPercentual(dfMortos$totalCasosAno[i],
                                                           dfMortos$numeroMortes[i])
    }

    #calcular percentual casos VIVOS
    for (i in c(1:nrow(dfVivos))) {
      dfVivos$PercentualVivosAno[i] = calcularPercentual(dfVivos$totalCasosAno[i],
                                                         dfVivos$numeroVivos[i])
    }


    colors <-
      c(
        'rgb(211,94,96)',
        'rgb(128,133,133)',
        'rgb(144,103,167)',
        'rgb(171,104,87)',
        'rgb(114,147,203)'
      )

    p <- plot_ly() %>%
      add_pie(
        data = dfVivos,
        labels = ~ dfVivos$ano,
        values = dfVivos$PercentualVivosAno,
        name = "Vivos",
        domain = list(row = 0, column = 0)
      ) %>%
      add_pie(
        data = dfMortos,
        labels = ~ dfMortos$ano,
        values = ~ dfMortos$PercentualMortesAno,
        name = "Mortes",
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

numerosCasosObito(type="pie")
