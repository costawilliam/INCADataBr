numeroCasosPorLocalizacaoAgrupada <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <- dfDados
  df$GrupoLocalizacao <- NULL
  df$LOCTUDET <- converterFatorParaCaracter(df$LOCTUDET)

  df$GrupoLocalizacao[df$LOCTUDET == "C00"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C01"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C02"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C03"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C04"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C05"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C06"] <-
    "GA"
  df$GrupoLocalizacao[df$LOCTUDET == "C07"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C08"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C09"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C10"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C11"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C12"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C13"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C14"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C15"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C16"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C17"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C18"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C19"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C20"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C21"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C22"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C23"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C24"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C25"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C26"] <-
    "GC"
  df$GrupoLocalizacao[df$LOCTUDET == "C30"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C31"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C32"] <-
    "GB"
  df$GrupoLocalizacao[df$LOCTUDET == "C33"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C34"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C37"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C38"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C39"] <-
    "GD"
  df$GrupoLocalizacao[df$LOCTUDET == "C40"] <-
    "GE"
  df$GrupoLocalizacao[df$LOCTUDET == "C41"] <-
    "GE"
  df$GrupoLocalizacao[df$LOCTUDET == "C43"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C44"] <-
    "GG"
  df$GrupoLocalizacao[df$LOCTUDET == "C45"] <-
    "G"
  df$GrupoLocalizacao[df$LOCTUDET == "C46"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C47"] <-
    "GH"
  df$GrupoLocalizacao[df$LOCTUDET == "C48"] <-
    "GI"
  df$GrupoLocalizacao[df$LOCTUDET == "C49"] <-
    "GJ"
  df$GrupoLocalizacao[df$LOCTUDET == "C50"] <-
    "GK"
  df$GrupoLocalizacao[df$LOCTUDET == "C51"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C52"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C53"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C54"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C55"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C56"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C57"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C58"] <-
    "GL"
  df$GrupoLocalizacao[df$LOCTUDET == "C60"] <-
    "GM"
  df$GrupoLocalizacao[df$LOCTUDET == "C61"] <-
    "GM"
  df$GrupoLocalizacao[df$LOCTUDET == "C62"] <-
    "GM"
  df$GrupoLocalizacao[df$LOCTUDET == "C63"] <-
    "GM"
  df$GrupoLocalizacao[df$LOCTUDET == "C64"] <-
    "GN"
  df$GrupoLocalizacao[df$LOCTUDET == "C65"] <-
    "GN"
  df$GrupoLocalizacao[df$LOCTUDET == "C66"] <-
    "GN"
  df$GrupoLocalizacao[df$LOCTUDET == "C67"] <-
    "GN"
  df$GrupoLocalizacao[df$LOCTUDET == "C68"] <-
    "GN"
  df$GrupoLocalizacao[df$LOCTUDET == "C69"] <-
    "GO"
  df$GrupoLocalizacao[df$LOCTUDET == "C70"] <-
    "GO"
  df$GrupoLocalizacao[df$LOCTUDET == "C71"] <-
    "GO"
  df$GrupoLocalizacao[df$LOCTUDET == "C72"] <-
    "GO"
  df$GrupoLocalizacao[df$LOCTUDET == "C73"] <-
    "GP"
  df$GrupoLocalizacao[df$LOCTUDET == "C74"] <-
    "GP"
  df$GrupoLocalizacao[df$LOCTUDET == "C75"] <-
    "GP"
  df$GrupoLocalizacao[df$LOCTUDET == "C76"] <-
    "GQ"
  df$GrupoLocalizacao[df$LOCTUDET == "C77"] <-
    "GR"
  df$GrupoLocalizacao[df$LOCTUDET == "C78"] <-
    "GS"
  df$GrupoLocalizacao[df$LOCTUDET == "C79"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C80"] <-
    "GS"
  df$GrupoLocalizacao[df$LOCTUDET == "C81"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C82"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C83"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C84"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C85"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C88"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C90"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C91"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C92"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C93"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C94"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C95"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C96"] <- NA
  #ver com Juliano
  df$GrupoLocalizacao[df$LOCTUDET == "C97"] <- NA
  #ver com Juliano

  df <- subset(df, is.na(df$GrupoLocalizacao) == 0)


  df <-
    aggregate(
      data.frame(NroCasos = df$GrupoLocalizacao),
      list(GrupoLocalizacao = df$GrupoLocalizacao),
      length
    )



  if (params$type == "bar") {
    p <-
      plot_ly(
        df,
        x = ~ df$GrupoLocalizacao,
        y = ~ df$NroCasos,
        type =  params$type
      ) %>%
      layout(
        title = params$title,
        xaxis = list(title = params$titleX),
        yaxis = list(title =  params$titleY)
      )

    p

  } else if (params$type == "pie") {
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
        labels = ~ df$GrupoLocalizacao,
        values = ~ df$FREQUENCIA,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~ paste(df$GrupoLocalizacao, ' - ', df$NroCasos, ' casos'),
        marker = list(
          colors = params$colors,
          line = list(color = '#FFFFFF', width = 1)
        ),
        showlegend = FALSE
      ) %>%
      layout(
        title = params$title,
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
