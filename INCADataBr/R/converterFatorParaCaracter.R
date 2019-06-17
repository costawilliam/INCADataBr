#' Title
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
converterFatorParaCaracter <- function(dados) {
  return (as.character(levels(dados))[dados])
}
