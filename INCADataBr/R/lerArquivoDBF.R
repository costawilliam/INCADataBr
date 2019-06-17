#' Title
#'
#' @param pathFile
#'
#' @return
#' @export
#'
#' @examples
lerArquivoDBF<- function(pathFile) {
    print("Iniciando Tarefa...")
    out <- tryCatch(
        {
            print("   Iniciando leitura...")
            dados <- foreign::read.dbf(pathFile)
            dados$ALCOOLIS <- converterFatorParaInteiro(dados$ALCOOLIS)
            dados$TABAGISM <- converterFatorParaInteiro(dados$TABAGISM)
            dados$IDADE <- converterFatorParaInteiro(dados$IDADE)
            print("   Leitura finalizada!")
        },
        error=function(cond) {
            message(paste("   - Houve um erro ao ler o arquivo: ", pathFile))
            return(NA)
        },
        finally={
            print("Tarefa finalizada!")
        }
    )
    return(dados)
}
