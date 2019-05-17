obterDados <- function(query) {
  if (!require("RPostgreSQL", character.only = TRUE)) {
    install.packages("RPostgreSQL", dep = TRUE)
  }

  library(RPostgreSQL)

  tryCatch({
    pw <- {
      "1234*ceted"
    }

    drv <- dbDriver("PostgreSQL")

    con <- dbConnect(
      drv,
      dbname = "pacoteinca",
      host = "ceted.feevale.br",
      port = 5432,
      user = "postgres",
      password = pw
    )

    dados <- dbGetQuery(con, query)

    dbDisconnect(con)
    dbUnloadDriver(drv)

    return(dados)
  },
  error = function(cond) {
    message(paste("   - Houve um erro ao criar conex??o:", cond))
    return(NA)
  })
}
