lerArquivoDBF<- function(file) {
    out <- tryCatch(
        {
            message("Tentando ler o arquivo...")
            foreign::read.dbf(file)
        },
        error=function(cond) {
            message(paste("   - Houve um erro ao ler o arquivo: ", file))
            return(NA)
        },
        finally={
            message("Tarefa finalizada!")
        }
    )    
    return(out)
}