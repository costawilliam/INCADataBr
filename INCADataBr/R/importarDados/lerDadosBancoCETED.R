lerDadosBancoCETED <- function() {
  library(RPostgreSQL)

  out <- tryCatch({
    con <- obterConexao()
    if (dbExistsTable(con, "tb_inca") == TRUE) {
      print("Realizando a leitura dos dados, por favor aguarde...")

      dados <- dbGetQuery(con, "SELECT * from tb_inca")

      print("Leitura concluída! Realizando ajuste dos dados...")
      names(dados) <- toupper(names(dados))
      print("Ajustes concluídos!")
    } else {
      message("   - Houve um erro ao ler os dados: Tabela não existe.")
    }
  },
  error = function(cond) {
    message(paste("   - Houve um erro ao ler os dados:", cond))
    return(NA)
  })
  return(dados)
}
