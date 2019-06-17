#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
validarData <- function(d) {
  formatoData = "%d/%m/%y"
  tryCatch(!is.na(as.Date(d, formatoData)),
           error = function(err) {FALSE})
}
