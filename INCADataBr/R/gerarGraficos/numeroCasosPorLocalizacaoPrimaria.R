numeroCasosPorLocalizacaoPrimaria <- function(dfDados, ...) {
  library(plotly)

  params <- tratarParametros(...)

  df <-
    aggregate(data.frame(NroCasos = dfDados$LOCTUDET),
              list(VAR = dfDados$LOCTUDET),
              length)


  df$VAR[df$VAR == "C00"] <- "C00 - Neoplasia Maligna do Lábio"
  df$VAR[df$VAR == "C01"] <- "C01 - Neoplasia Maligna da Base da Língua"
  df$VAR[df$VAR == "C02"] <- "C02 - Neoplasia Maligna de Outras Partes e de Partes Não Especificadas da Língua"
  df$VAR[df$VAR == "C03"] <- "C03 - Neoplasia Maligna da Gengiva"
  df$VAR[df$VAR == "C04"] <- "C04 - Neoplasia Maligna do Assoalho da Boca"
  df$VAR[df$VAR == "C05"] <- "C05 - Neoplasia Maligna do Palato"
  df$VAR[df$VAR == "C06"] <- "C06 - Neoplasia Maligna de Outras Partes e de Partes Não Especificadas da Boca"
  df$VAR[df$VAR == "C07"] <- "C07 - Neoplasia Maligna da Glândula Parótida"
  df$VAR[df$VAR == "C08"] <- "C08 - Neoplasia Maligna de Outras Glândulas Salivares Maiores e as Não Especificadas"
  df$VAR[df$VAR == "C09"] <- "C09 - Neoplasia Maligna da Amígdala"
  df$VAR[df$VAR == "C10"] <- "C10 - Neoplasia Maligna da Orofaringe"
  df$VAR[df$VAR == "C11"] <- "C11 - Neoplasia Maligna da Nasofaringe"
  df$VAR[df$VAR == "C12"] <- "C12 - Neoplasia Maligna do Seio Piriforme"
  df$VAR[df$VAR == "C13"] <- "C13 - Neoplasia Maligna da Hipofaringe"
  df$VAR[df$VAR == "C14"] <- "C14 - Neoplasia Maligna de Outras Localizações e de Localizações Mal Definida, do Lábio, Cavidade Oral e Faringe"
  df$VAR[df$VAR == "C15"] <- "C15 - Neoplasia Maligna do Esôfago"
  df$VAR[df$VAR == "C16"] <- "C16 - Neoplasia Maligna do Estômago"
  df$VAR[df$VAR == "C17"] <- "C17 - Neoplasia Maligna do Intestino Delgado"
  df$VAR[df$VAR == "C18"] <- "C18 - Neoplasia Maligna do Cólon"
  df$VAR[df$VAR == "C19"] <- "C19 - Neoplasia Maligna da Junção Retossigmóide"
  df$VAR[df$VAR == "C20"] <- "C20 - Neoplasia Maligna do Reto"
  df$VAR[df$VAR == "C21"] <- "C21 - Neoplasia Maligna do Ânus e do Canal Anal"
  df$VAR[df$VAR == "C22"] <- "C22 - Neoplasia Maligna do Fígado e Das Vias Biliares Intra-hepáticas"
  df$VAR[df$VAR == "C23"] <- "C23 - Neoplasia Maligna da Vesícula Biliar"
  df$VAR[df$VAR == "C24"] <- "C24 - Neoplasia Maligna de Outras Partes, e de Partes Não Especificadas Das Vias Biliares"
  df$VAR[df$VAR == "C25"] <- "C25 - Neoplasia Maligna do Pâncreas"
  df$VAR[df$VAR == "C26"] <- "C26 - Neoplasia Maligna de Outros Órgãos Digestivos e de Localizações Mal Definidas no Aparelho Digestivo"
  df$VAR[df$VAR == "C30"] <- "C30 - Neoplasia Maligna da Cavidade Nasal e do Ouvido Médio"
  df$VAR[df$VAR == "C31"] <- "C31 - Neoplasia Maligna Dos Seios da Face"
  df$VAR[df$VAR == "C32"] <- "C32 - Neoplasia Maligna da Laringe"
  df$VAR[df$VAR == "C33"] <- "C33 - Neoplasia Maligna da Traquéia"
  df$VAR[df$VAR == "C34"] <- "C34 - Neoplasia Maligna Dos Brônquios e Dos Pulmões"
  df$VAR[df$VAR == "C37"] <- "C37 - Neoplasia Maligna do Timo"
  df$VAR[df$VAR == "C38"] <- "C38 - Neoplasia Maligna do Coração, Mediastino e Pleura"
  df$VAR[df$VAR == "C39"] <- "C39 - Neoplasia Maligna de Outras Localizações e de Localizações Mal Definidas do Aparelho Respiratório e Dos Órgãos Intratorácicos"
  df$VAR[df$VAR == "C40"] <- "C40 - Neoplasia Maligna Dos Ossos e Cartilagens Articulares Dos Membros"
  df$VAR[df$VAR == "C41"] <- "C41 - Neoplasia Maligna Dos Ossos e Das Cartilagens Articulares de Outras Localizações e de Localizações Não Especificadas"
  df$VAR[df$VAR == "C43"] <- "C43 - Melanoma Maligno da Pele"
  df$VAR[df$VAR == "C44"] <- "C44 - Outras Neoplasias Malignas da Pele"
  df$VAR[df$VAR == "C45"] <- "C45 - Mesotelioma"
  df$VAR[df$VAR == "C46"] <- "C46 - Sarcoma de Kaposi"
  df$VAR[df$VAR == "C47"] <- "C47 - Neoplasia Maligna Dos Nervos Periféricos e do Sistema Nervoso Autônomo"
  df$VAR[df$VAR == "C48"] <- "C48 - Neoplasia Maligna Dos Tecidos Moles do Retroperitônio e do Peritônio"
  df$VAR[df$VAR == "C49"] <- "C49 - Neoplasia Maligna do Tecido Conjuntivo e de Outros Tecidos Moles"
  df$VAR[df$VAR == "C50"] <- "C50 - Neoplasia Maligna da Mama"
  df$VAR[df$VAR == "C51"] <- "C51 - Neoplasia Maligna da Vulva"
  df$VAR[df$VAR == "C52"] <- "C52 - Neoplasia Maligna da Vagina"
  df$VAR[df$VAR == "C53"] <- "C53 - Neoplasia Maligna do Colo do Útero"
  df$VAR[df$VAR == "C54"] <- "C54 - Neoplasia Maligna do Corpo do Útero"
  df$VAR[df$VAR == "C55"] <- "C55 - Neoplasia Maligna do Útero, Porção Não Especificada"
  df$VAR[df$VAR == "C56"] <- "C56 - Neoplasia Maligna do Ovário"
  df$VAR[df$VAR == "C57"] <- "C57 - Neoplasia Maligna de Outros Órgãos Genitais Femininos e Dos Não Especificados"
  df$VAR[df$VAR == "C58"] <- "C58 - Neoplasia Maligna da Placenta"
  df$VAR[df$VAR == "C60"] <- "C60 - Neoplasia Maligna do Pênis"
  df$VAR[df$VAR == "C61"] <- "C61 - Neoplasia Maligna da Próstata"
  df$VAR[df$VAR == "C62"] <- "C62 - Neoplasia Maligna Dos Testículos"
  df$VAR[df$VAR == "C63"] <- "C63 - Neoplasia Maligna de Outros Órgãos Genitais Masculinos e Dos Não Especificados"
  df$VAR[df$VAR == "C64"] <- "C64 - Neoplasia Maligna do Rim, Exceto Pelve Renal"
  df$VAR[df$VAR == "C65"] <- "C65 - Neoplasia Maligna da Pelve Renal"
  df$VAR[df$VAR == "C66"] <- "C66 - Neoplasia Maligna Dos Ureteres"
  df$VAR[df$VAR == "C67"] <- "C67 - Neoplasia Maligna da Bexiga"
  df$VAR[df$VAR == "C68"] <- "C68 - Neoplasia Maligna de Outros Órgãos Urinários e Dos Não Especificados"
  df$VAR[df$VAR == "C69"] <- "C69 - Neoplasia Maligna do Olho e Anexos"
  df$VAR[df$VAR == "C70"] <- "C70 - Neoplasia Maligna Das Meninges"
  df$VAR[df$VAR == "C71"] <- "C71 - Neoplasia Maligna do Encéfalo"
  df$VAR[df$VAR == "C72"] <- "C72 - Neoplasia Maligna da Medula Espinhal, Dos Nervos Cranianos e de Outras Partes do Sistema Nervoso Central"
  df$VAR[df$VAR == "C73"] <- "C73 - Neoplasia Maligna da Glândula Tireóide"
  df$VAR[df$VAR == "C74"] <- "C74 - Neoplasia Maligna da Glândula Supra-renal (Glândula Adrenal)"
  df$VAR[df$VAR == "C75"] <- "C75 - Neoplasia Maligna de Outras Glândulas Endócrinas e de Estruturas Relacionadas"
  df$VAR[df$VAR == "C76"] <- "C76 - Neoplasia Maligna de Outras Localizações e de Localizações Mal Definidas"
  df$VAR[df$VAR == "C77"] <- "C77 - Neoplasia Maligna Secundária e Não Especificada Dos Gânglios Linfáticos"
  df$VAR[df$VAR == "C78"] <- "C78 - Neoplasia Maligna Secundária Dos Órgãos Respiratórios e Digestivos"
  df$VAR[df$VAR == "C79"] <- "C79 - Neoplasia Maligna Secundária de Outras Localizações"
  df$VAR[df$VAR == "C80"] <- "C80 - Neoplasia Maligna, Sem Especificação de Localização"
  df$VAR[df$VAR == "C81"] <- "C81 - Doença de Hodgkin"
  df$VAR[df$VAR == "C82"] <- "C82 - Linfoma Não-Hodgkin, Folicular (nodular)"
  df$VAR[df$VAR == "C83"] <- "C83 - Linfoma Não-Hodgkin Difuso"
  df$VAR[df$VAR == "C84"] <- "C84 - Linfomas de Células T Cutâneas e Periféricas"
  df$VAR[df$VAR == "C85"] <- "C85 - Linfoma Não-Hodgkin de Outros Tipos e de Tipo Não Especificado"
  df$VAR[df$VAR == "C88"] <- "C88 - Doenças Imunoproliferativas Malignas"
  df$VAR[df$VAR == "C90"] <- "C90 - Mieloma Múltiplo e Neoplasias Malignas de Plasmócitos"
  df$VAR[df$VAR == "C91"] <- "C91 - Leucemia Linfóide"
  df$VAR[df$VAR == "C92"] <- "C92 - Leucemia Mielóide"
  df$VAR[df$VAR == "C93"] <- "C93 - Leucemia Monocítica"
  df$VAR[df$VAR == "C94"] <- "C94 - Outras Leucemias de Células de Tipo Especificado"
  df$VAR[df$VAR == "C95"] <- "C95 - Leucemia de Tipo Celular Não Especificado"
  df$VAR[df$VAR == "C96"] <- "C96 - Outras Neoplasias Malignas e as Não Especificadas Dos Tecidos Linfático, Hematopoético e Tecidos Correlatos"
  df$VAR[df$VAR == "C97"] <- "C97 - Neoplasias Malignas de Localizações Múltiplas Independentes (primárias)"

  if (params$type == "bar") {
    plotGraficoBarras(df, params)
  } else if (params$type == "pie") {
    plotGraficoPizza(df, params)
  } else {
    message("Tipo de gráfico indicado não é suportado por esta função. Tente utilizar o parâmetro type como \"bar\" ou \"pie\".")
  }

}
