numeroCasosPorFaixasIdade <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)
  numeroGrupos <- length(params$groups)
  if (numeroGrupos < 2) {
    message("Para gerar este gráfico é necessário indicar ao menos dois grupos.")
  } else {
    df <-
      aggregate(data.frame(NroCasos = dfDados$IDADE),
                list(Idade = dfDados$IDADE),
                length)

    df <- subset(df, df$Idade < 150)

    idadeMax <-  max(df$Idade)

    #Exibe ao usuário os grupos que serão desconsiderados
    for (i in c(1:numeroGrupos)) {
      if (params$groups[i] > idadeMax) {
        message(
          paste(
            "O valor",
            params$groups[i],
            "é superior a maior idade registrada nos dados que estão sendo utilizados para geração deste gráfico e será desconsiderado."
          )
        )
      }
    }

    #remove os grupos inválidos (maiores que a idade máxima)
    params$groups <-
      subset(params$groups, params$groups <= idadeMax)

    #Atribui a nova quantidade de grupos que deve ser considerada
    numeroGrupos <- length(params$groups)


    #cria datasets de acordo os os grupos informados pelo usuário
    dfGrupos <- c()
    for (i in c(1:(numeroGrupos - 1))) {
      dfGrupos[[i]] <-
        subset(
          df,
          df$Idade >= params$groups[i] &
            df$Idade < params$groups[i + 1]
        )
    }
    dfGrupos[[numeroGrupos]] <-
      subset(df, df$Idade >= params$groups[numeroGrupos])

    #atribui os valores iniciais das faixas
    inicio <- params$groups

    #Atribui os valores finais das faixas
    fim <- c()
    for (i in c(1:(numeroGrupos - 1))) {
      fim[i] <- params$groups[i + 1] - 1
    }
    fim[numeroGrupos] <- idadeMax

    #Atribui a quantidade de casos por grupo
    quantidadePorGrupo <- c()
    for (i in c(1:length(dfGrupos))) {
      x <- data.frame(dfGrupos[i])
      quantidadePorGrupo[i] <- sum(x$NroCaso)
    }

    data <- data.frame(inicio, fim, quantidadePorGrupo)

    #Adiciona coluna para os rótulos do gráfico
    data$grupo <- paste("De", data$inicio, "até", data$fim, "anos")

    #data <- subset(data, data$quantidadePorGrupo > 0) #ver com Juliano se devo exibir colunas

    print(data)

    if (params$type == "bar") {
      p <-
        plot_ly(
          df,
          x = ~ data$grupo,
          y = ~ data$quantidadePorGrupo,
          type = params$type
        ) %>%
        layout(
          title = params$title,
          xaxis = list(title = params$titleX),

          yaxis = list(title = params$titleY)
        )
      p
    } else if (params$type == "pie") {
      df["FREQUENCIA"] <- NA

      for (i in c(1:nrow(data))) {
        data$FREQUENCIA[i] = calcularPercentual(nrow(dfDados), data$quantidadePorGrupo[i])
      }

      p <-
        plot_ly(
          data,
          labels = ~ data$grupo,
          values = ~ data$FREQUENCIA,
          type = params$type,
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text',
          text = ~ paste(data$grupo, ' - ', data$quantidadePorGrupo, 'casos'),
          marker = list(
            colors = colors,
            line = list(color = '#FFFFFF', width = 1)
          ),
          showlegend = TRUE
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
}
