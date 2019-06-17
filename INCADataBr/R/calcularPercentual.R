#' calcularPercentual
#'
#' @params totalRegistros
#' @params quantidade
#'
#' @return percentual
#'
#'
#' @examples
calcularPercentual <- function(totalRegistros, quantidade) {
  percentual <- (quantidade * 100) / totalRegistros;

  return(percentual)
}
