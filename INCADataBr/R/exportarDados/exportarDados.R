exportarDados <- function(dfDados, ...) {
  params <- tratarParametros(...)

  file <-
    paste(params$caminhoArquivo,
          params$nomeArquivo,
          ".",
          params$tipoArquivo,
          sep = "")

  if (params$tipoArquivo == "csv") {
    tryCatch({
      write.csv(dfDados, file)
    },
    error = function(cond) {
      out <- message("Houve um erro ao gravar o arquivo")
    }

  } else if (params$tipoArquivo == "xls") {
    #to do

  } else if (params$tipoArquivo == "sql") {
    #to do

  } else {
    message("Formato não suportado por esta função")
    message("Tente utilizar o parâmetro tipoArquivo como \"csv\",  \"xls\" ou \"sql\".")
  }
}
