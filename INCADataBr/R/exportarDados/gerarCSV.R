gerarCSV <- function(dfDados, caminho, nomeArquivo){
  tryCatch(
    {
      file <- paste(paste(caminho, nomeArquivo, sep = ""), ".CSV", sep="")
      write.csv(dfDados, file)
    },
    error=function(cond) {
      out<-message("Houve um erro ao gravar o arquivo")

    }
  )
}