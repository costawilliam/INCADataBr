verificarPacote <- function(nomePacote){
  if (!require(nomePacote,character.only = TRUE)){
    install.packages(nomePacote,dep=TRUE)
  }
}

