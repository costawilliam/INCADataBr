#' Title
#'
#' @return
#' @export
#'
#' @examples
lerDadosBancoCETED <- function() {
  library(RPostgreSQL)

  out <- tryCatch({
    con <- obterConexao()
    if (DBI::dbExistsTable(con, "tb_inca") == TRUE) {
      print("Realizando a leitura dos dados, por favor aguarde...")

      dados <- dbGetQuery(con, "SELECT * from tb_inca")

      print("Leitura concluida! Realizando ajuste dos dados...")
      names(dados) <- toupper(names(dados))
      print("Ajustes concluidos!")
    } else {
      message("   - Houve um erro ao ler os dados: Tabela nao existe.")
    }
  },
  error = function(cond) {
    message(paste("   - Houve um erro ao ler os dados:", cond))
    return(NA)
  })
  return(dados)
}
