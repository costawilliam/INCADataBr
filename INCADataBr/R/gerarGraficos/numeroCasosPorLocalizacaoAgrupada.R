numeroCasosPorLocalizacaoAgrupada <- function(...) {
  params <- tratarParametros(...)

  query <-
    "SELECT LOCTUDET as var, count(*) AS nrocasos from tb_inca group by LOCTUDET order by LOCTUDET"

  df <- obterDados(query)

  df$GrupoLocalizacao <- NULL

  df$GrupoLocalizacao[df$var == "C00"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C01"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C02"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C03"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C04"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C05"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C06"] <- "Grupo A"
  df$GrupoLocalizacao[df$var == "C07"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C08"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C09"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C10"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C11"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C12"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C13"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C14"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C15"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C16"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C17"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C18"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C19"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C20"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C21"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C22"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C23"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C24"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C25"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C26"] <- "Grupo C"
  df$GrupoLocalizacao[df$var == "C30"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C31"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C32"] <- "Grupo B"
  df$GrupoLocalizacao[df$var == "C33"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C34"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C37"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C38"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C39"] <- "Grupo D"
  df$GrupoLocalizacao[df$var == "C40"] <- "Grupo E"
  df$GrupoLocalizacao[df$var == "C41"] <- "Grupo E"
  df$GrupoLocalizacao[df$var == "C42"] <- "Grupo F"
  df$GrupoLocalizacao[df$var == "C44"] <- "Grupo G"
  df$GrupoLocalizacao[df$var == "C47"] <- "Grupo H"
  df$GrupoLocalizacao[df$var == "C48"] <- "Grupo I"
  df$GrupoLocalizacao[df$var == "C49"] <- "Grupo J"
  df$GrupoLocalizacao[df$var == "C50"] <- "Grupo K"
  df$GrupoLocalizacao[df$var == "C51"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C52"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C53"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C54"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C55"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C56"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C57"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C58"] <- "Grupo L"
  df$GrupoLocalizacao[df$var == "C60"] <- "Grupo M"
  df$GrupoLocalizacao[df$var == "C61"] <- "Grupo M"
  df$GrupoLocalizacao[df$var == "C62"] <- "Grupo M"
  df$GrupoLocalizacao[df$var == "C63"] <- "Grupo M"
  df$GrupoLocalizacao[df$var == "C64"] <- "Grupo N"
  df$GrupoLocalizacao[df$var == "C65"] <- "Grupo N"
  df$GrupoLocalizacao[df$var == "C66"] <- "Grupo N"
  df$GrupoLocalizacao[df$var == "C67"] <- "Grupo N"
  df$GrupoLocalizacao[df$var == "C68"] <- "Grupo N"
  df$GrupoLocalizacao[df$var == "C69"] <- "Grupo O"
  df$GrupoLocalizacao[df$var == "C70"] <- "Grupo O"
  df$GrupoLocalizacao[df$var == "C71"] <- "Grupo O"
  df$GrupoLocalizacao[df$var == "C72"] <- "Grupo O"
  df$GrupoLocalizacao[df$var == "C73"] <- "Grupo P"
  df$GrupoLocalizacao[df$var == "C74"] <- "Grupo P"
  df$GrupoLocalizacao[df$var == "C75"] <- "Grupo P"
  df$GrupoLocalizacao[df$var == "C76"] <- "Grupo Q"
  df$GrupoLocalizacao[df$var == "C77"] <- "Grupo R"
  df$GrupoLocalizacao[df$var == "C80"] <- "Grupo S"

  df <- subset(df, is.na(df$GrupoLocalizacao) == 0)

  df <- aggregate(df$nrocasos, by=list(var=df$GrupoLocalizacao), FUN=sum)
  colnames(df) <- c("var", "nrocasos")

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

numeroCasosPorLocalizacaoAgrupada()
