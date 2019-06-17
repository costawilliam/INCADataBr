#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
converterFatorParaInteiro <- function(dados){
  return (as.numeric(levels(dados))[dados])
}
