numeroCasosPorFaixasidade <- function(...) {
  params <- tratarParametros(...)

  numeroGrupos <- length(params$groups)

  if (numeroGrupos < 2) {
    message("Para gerar este gráfico é necessário indicar ao menos dois grupos.")
  } else {
    query <-
      "SELECT idade as idade, count(*) AS NroCasos from tb_inca group by idade order by idade"

    df <- obterDados(query)

    df$idade <- as.numeric(df$idade)

    df <- subset(df, df$idade < 150)

    idadeMax <-  max(df$idade)

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


    #cria lista de acordo os os grupos informados pelo usuário
    dfGrupos <- c()
    for (i in c(1:(numeroGrupos - 1))) {
      dfGrupos[[i]] <-
        subset(df,
               df$idade >= params$groups[i] &
                 df$idade < params$groups[i + 1])
    }

    dfGrupos[[numeroGrupos]] <-
      subset(df, df$idade >= params$groups[numeroGrupos])

    #atribui os valores iniciais das faixas
    inicio <- params$groups

    #Atribui os valores finais das faixas
    fim <- c()
    for (i in c(1:(numeroGrupos - 1))) {
      fim[i] <- params$groups[i + 1] - 1
    }
    fim[numeroGrupos] <- idadeMax

    #Atribui a quantidade de casos por grupo
    nrocasos <- c()
    for (i in c(1:length(dfGrupos))) {
      nrocasos[i] <- sum(data.frame(dfGrupos[i])$nrocasos)
    }

    #cria um dataset com o inicio, fim e número de casos de cada grupo
    data <- data.frame(inicio, fim, nrocasos)

    #Adiciona coluna para os rótulos do gráfico
    data$var <- paste("De", data$inicio, "até", data$fim, "anos")

    #remove as colunas que não serão utilizadas
    df <- data.frame(nrocasos = data$nrocasos, var = data$var)

    if (params$type == "bar") {
      plotGraficoBarras(df, params)
    } else if (params$type == "pie") {
      plotGraficoPizza(df, params)
    } else {
      message(
        "Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parÃ¢metro type como \"bar\" ou \"pie\"."
      )
    }
  }
}
