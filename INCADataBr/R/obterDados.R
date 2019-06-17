#' Title
#'
#' @param query
#'
#' @return
#' @export
#'
#' @examples
obterDados <- function(query) {
  if (!require("RPostgreSQL", character.only = TRUE)) {
    install.packages("RPostgreSQL", dep = TRUE)
  }

  if (!require("DBI", character.only = TRUE)) {
    install.packages("DBI", dep = TRUE)
  }

  library(RPostgreSQL)

  tryCatch({
    pw <- {
      "1234*ceted"
    }

    drv <- DBI::dbDriver("PostgreSQL")

    con <- DBI::dbConnect(
      drv,
      dbname = "pacoteinca",
      host = "ceted.feevale.br",
      port = 5432,
      user = "postgres",
      password = pw
    )

    dados <- DBI::dbGetQuery(con, query)

    DBI::dbDisconnect(con)
    DBI::dbUnloadDriver(drv)

    return(dados)
  },
  error = function(cond) {
    message(paste("   - Houve um erro ao criar conex??o:", cond))
    return(NA)
  })
}
