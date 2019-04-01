converterFatorParaInteiro <- function(dados){
  return (as.numeric(levels(dados))[dados])
}