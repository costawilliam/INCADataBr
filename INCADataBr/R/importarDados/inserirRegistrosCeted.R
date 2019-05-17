inserirRegistrosCeted <- function(pathFile) {
  library(RPostgreSQL)

  dfDados <- lerArquivoDBF(pathFile)

  anoRegistros <- dfDados$DTPRICON[1]

  query <-
    paste("select count(*) from anos_importados where ano = '",
          anoRegistros,"'", sep = "")

  df <- obterDados(query)
  print("Iniciando importação dos dados...")
  if (as.numeric(df$count[1]) > 0) {
    print("   Dados já foram importados.")
    print("Processo concluído!")
  } else {

    names(dfDados) <-  tolower(names(dfDados))

    #atribui valor para variável pw
    pw <- {
      "1234*ceted"
    }

    #instancia driver PostgreSQL
    drv <- dbDriver("PostgreSQL")

    #obtem instancia de conexão com banco de dados
    con <- dbConnect(
      drv,
      dbname = "pacoteinca",
      host = "ceted.feevale.br",
      port = 5432,
      user = "postgres",
      password = pw
    )
    print("processando...")
    #insere registros na tabela
    dbWriteTable(
      con,
      'tb_inca2',
      dfDados,
      row.names = FALSE,
      append = TRUE,
      overwrite = FALSE
    )
    print("Inserção dos dados finalizada! Concluindo processo de importação...")
    ano <- as.character(anoRegistros)

    #insere ano na tabela dos anos importados, para controle
    dbWriteTable(
      con,
      'anos_importados',
      as.data.frame(ano),
      row.names = FALSE,
      overwrite = FALSE,
      append = TRUE
    )
    print("Processo concluído!")

    #encerra conexão banco de dados
    dbDisconnect(con)

    #desaloca driver PostgreSQL
    dbUnloadDriver(drv)
  }
}

pathFile <- "C:/Users/WilliamCostaJacksond/Documents/William/coisas tc/TC II/Dados/2017/rhc17.dbf"
inserirRegistrosCeted(pathFile)
pathFile <- "C:/Users/WilliamCostaJacksond/Documents/William/coisas tc/TC II/Dados/2016/rhc16.dbf"
inserirRegistrosCeted(pathFile)
