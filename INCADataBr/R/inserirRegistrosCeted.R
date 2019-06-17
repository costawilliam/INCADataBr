#' Title
#'
#' @param pathFile
#'
#' @return
#' @export
#'
#' @examples
inserirRegistrosCeted <- function(pathFile) {
  library(RPostgreSQL)

  dfDados <- lerArquivoDBF(pathFile)

  anoRegistros <- dfDados$DTPRICON[1]

  query <-
    paste("select count(*) from anos_importados where ano = '",
          anoRegistros,"'", sep = "")

  df <- obterDados(query)
  print("Iniciando importaC'C#o dos dados...")
  if (as.numeric(df$count[1]) > 0) {
    print("   Dados jC! foram importados.")
    print("Processo concluC-do!")
  } else {

    names(dfDados) <-  tolower(names(dfDados))

    #atribui valor para variC!vel pw
    pw <- {
      "1234*ceted"
    }

    #instancia driver PostgreSQL

    drv <- DBI::dbDriver("PostgreSQL")

    #obtem instancia de conexC#o com banco de dados

    con <- DBI::dbConnect(
      drv,
      dbname = "pacoteinca",
      host = "ceted.feevale.br",
      port = 5432,
      user = "postgres",
      password = pw
    )

    print("processando...")

    #insere registros na tabela
    DBI::dbWriteTable(
      con,
      'tb_inca2',
      dfDados,
      row.names = FALSE,
      append = TRUE,
      overwrite = FALSE
    )

    print("InserC'C#o dos dados finalizada! Concluindo processo de importaC'C#o...")
    ano <- as.character(anoRegistros)

    #insere ano na tabela dos anos importados, para controle
    DBI::dbWriteTable(
      con,
      'anos_importados',
      as.data.frame(ano),
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE
    )
    print("Processo concluC-do!")

    #encerra conexC#o banco de dados
    DBI::dbDisconnect(con)

    #desaloca driver PostgreSQL
    DBI::dbUnloadDriver(drv)
  }
}
