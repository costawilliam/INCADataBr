lerDadosBancoCETED<- function() {
  out <- tryCatch(
    {
      message("Tentando ler o arquivo...")

      pw <- {
        "password"
      }

      drv <- dbDriver("PostgreSQL")

      con <- dbConnect(drv, dbname = "pacoteinca",
                       host = "example.feevale.br", port = 5555,
                       user = "user", password = pw)

      rm(pw)

      if(dbExistsTable(con, "table") == TRUE){
        dados <- dbGetQuery(con, "SELECT * from table")
        names(dados) <- toupper(names(dados))

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
    }
  )
  return(dados)
}
