#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
numeroCasosPorLocalizacaoPrimaria <- function( ...) {
  params <- tratarParametros(...)

  query <-
    "SELECT LOCTUDET as VAR, count(*) AS NroCasos from tb_inca group by LOCTUDET order by LOCTUDET"

  df <- obterDados(query)

  df$desc[df$var == "C00"] <- "Labio                                      "
  df$desc[df$var == "C01"] <- "Base Da Lingua                             "
  df$desc[df$var == "C02"] <- "Outras Partes Partes Nao Especif. da Lingua"
  df$desc[df$var == "C03"] <- "Gengiva                                    "
  df$desc[df$var == "C04"] <- "Assoalho Da Boca                           "
  df$desc[df$var == "C05"] <- "Palato                                     "
  df$desc[df$var == "C06"] <- "Outras Partes Partes Nao Especif. da Boca  "
  df$desc[df$var == "C07"] <- "Glandula Parotida                          "
  df$desc[df$var == "C08"] <- "Outras Glandulas Salivares Maiores         "
  df$desc[df$var == "C09"] <- "Amigdala                                   "
  df$desc[df$var == "C10"] <- "Orofaringe                                 "
  df$desc[df$var == "C11"] <- "Nasofaringe                                "
  df$desc[df$var == "C12"] <- "Seio Periforme                             "
  df$desc[df$var == "C13"] <- "Hipofaringe                                "
  df$desc[df$var == "C14"] <- "Out.Loc. Loc.Mal Def.Labio,Cav.Oral,Faringe"
  df$desc[df$var == "C15"] <- "Esofago                                    "
  df$desc[df$var == "C16"] <- "Estomago                                   "
  df$desc[df$var == "C17"] <- "Intestino Delgado                          "
  df$desc[df$var == "C18"] <- "Colon                                      "
  df$desc[df$var == "C19"] <- "Juncao Retossigmoidiana                    "
  df$desc[df$var == "C20"] <- "Reto                                       "
  df$desc[df$var == "C21"] <- "Anus e Canal Anal                          "
  df$desc[df$var == "C22"] <- "Figado e Vias Biliares Intrahepaticas      "
  df$desc[df$var == "C23"] <- "Vesicula Biliar                            "
  df$desc[df$var == "C24"] <- "Outras Partes Partes Nao Espec.V.Biliares  "
  df$desc[df$var == "C25"] <- "Pancreas                                   "
  df$desc[df$var == "C26"] <- "Outros Org.Digest Loc.Mal Def.Ap.Digest    "
  df$desc[df$var == "C30"] <- "Cavidade Nasal e Ouvido Medio              "
  df$desc[df$var == "C31"] <- "Seios da Face                              "
  df$desc[df$var == "C32"] <- "Laringe                                    "
  df$desc[df$var == "C33"] <- "Traqueia                                   "
  df$desc[df$var == "C34"] <- "Bronquios e Pulmoes                        "
  df$desc[df$var == "C37"] <- "Timo                                       "
  df$desc[df$var == "C38"] <- "Coracao, Mediastino e Pleura               "
  df$desc[df$var == "C39"] <- "Outras Loc.Mal Definidas do Ap.Respirat    "
  df$desc[df$var == "C40"] <- "Ossos Articulacoes Cartil.Artic.dos Memb   "
  df$desc[df$var == "C41"] <- "Neop.Mal.Ossos Cart.Art.Outras Loc.E Loc.Ne"
  df$desc[df$var == "C42"] <- "Sistema Hematopoetico e Reticuloendotelial "
  df$desc[df$var == "C44"] <- "Pele                                       "
  df$desc[df$var == "C47"] <- "Nervos Perifericos SNA                     "
  df$desc[df$var == "C48"] <- "Retroperitonio e Peritonio                 "
  df$desc[df$var == "C49"] <- "Tec.Conjuntivo Subcutaneo Outros Tec.Moles "
  df$desc[df$var == "C50"] <- "Mama                                       "
  df$desc[df$var == "C51"] <- "Vulva                                      "
  df$desc[df$var == "C52"] <- "Vagina                                     "
  df$desc[df$var == "C53"] <- "Colo do Utero                              "
  df$desc[df$var == "C54"] <- "Corpo do Utero                             "
  df$desc[df$var == "C55"] <- "Utero, SOE                                 "
  df$desc[df$var == "C56"] <- "Ovario                                     "
  df$desc[df$var == "C57"] <- "Outros Org.Genit.Femininos e Nao Especif   "
  df$desc[df$var == "C58"] <- "Placenta                                   "
  df$desc[df$var == "C60"] <- "Penis                                      "
  df$desc[df$var == "C61"] <- "Prostata                                   "
  df$desc[df$var == "C62"] <- "Testiculo                                  "
  df$desc[df$var == "C63"] <- "Outros Orgaos Genitais Masculinos          "
  df$desc[df$var == "C64"] <- "Rim                                        "
  df$desc[df$var == "C65"] <- "Pelve Renal                                "
  df$desc[df$var == "C66"] <- "Ureter                                     "
  df$desc[df$var == "C67"] <- "Bexiga                                     "
  df$desc[df$var == "C68"] <- "Outros Org.Urinarios e Nao Especificados   "
  df$desc[df$var == "C69"] <- "Olho e Anexos                              "
  df$desc[df$var == "C70"] <- "Meninges                                   "
  df$desc[df$var == "C71"] <- "Encefalo                                   "
  df$desc[df$var == "C72"] <- "Med.Espin. Nerv.Cran Out.Partes S.N.Central"
  df$desc[df$var == "C73"] <- "Glandula Tiroide                           "
  df$desc[df$var == "C74"] <- "Glandula Supra Renal                       "
  df$desc[df$var == "C75"] <- "Outras Gland.Endocrinas e Estr.Relacionadas"
  df$desc[df$var == "C76"] <- "Outras Localizacoes Localiz. Mal Definidas "
  df$desc[df$var == "C77"] <- "Linfonodos                                 "
  df$desc[df$var == "C80"] <- "Localizacao Primaria Desconhecida          "

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
