lerDadosBancoCETED<- function() {
  out <- tryCatch(
    {
      message("Tentando ler o arquivo...")
      #print(paste("Hora inicio:", Sys.time()))

      pw <- {
        "1234*ceted"
      }

      drv <- dbDriver("PostgreSQL")

      con <- dbConnect(drv, dbname = "pacoteinca",
                       host = "ceted.feevale.br", port = 5432,
                       user = "postgres", password = pw)

      rm(pw)

      if(dbExistsTable(con, "tb_inca") == TRUE){
        print("Realizando a leitura dos dados, por favor aguarde...")
        dados <- dbGetQuery(con, "SELECT * from tb_inca where id < 500")
        print("Leitura concluída! Realizando ajuste dos dados...")
        names(dados) <- toupper(names(dados))
        print("Ajustes concluídos!")
      } else {
        message("   - Houve um erro ao ler os dados: Tabela não existe.")
      }
    },
    error=function(cond) {
      message(paste("   - Houve um erro ao ler o dados:", cond))
      return(NA)
    },
    finally={
      message("Tarefa finalizada!")
      #print(paste("Hora fim:", Sys.time()))
    }
  )
  return(dados)
}
