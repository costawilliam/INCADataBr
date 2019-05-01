lerArquivoDBF<- function(file) {
    out <- tryCatch(
        {
            message("Tentando ler o arquivo...")
            dados <-foreign::read.dbf(file)

            dados$ALCOOLIS <- converterFatorParaInteiro(dados$ALCOOLIS)
            dados$TABAGISM <- converterFatorParaInteiro(dados$TABAGISM)
            dados$IDADE <- converterFatorParaInteiro(dados$IDADE)
            dados$ESTADIAG <- converterFatorParaCaracter(dados$ESTADIAG)
            dados$LOCTUDET <- converterFatorParaCaracter(dados$LOCTUDET)
            dados$RACACOR <- converterFatorParaInteiro(dados$RACACOR)
            dados$SEXO <- converterFatorParaInteiro(dados$SEXO)
        },
        error=function(cond) {
            message(paste("   - Houve um erro ao ler o arquivo: ", file))
            return(NA)
        },
        finally={
            message("Tarefa finalizada!")
        }
    )
    return(dados)
}
