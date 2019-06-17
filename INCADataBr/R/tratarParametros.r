#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tratarParametros <- function (...) {
  params = list(...)

  if (is.null(params$title)) {
    params$title <- "Title here"
  }

  if (is.null(params$titleX)) {
    params$titleX <- "Axis X"
  }

  if (is.null(params$titleY)) {
    params$titleY <- "Axis Y"
  }

  if (is.null(params$type)) {
    params$type <- "bar"
  }

  if (is.null(params$tipoArquivo)) {
    params$tipo <- "csv"
  }

  if (is.null(params$nomeArquivo)) {
    params$nomeArquivo <- "dados"
  }

  if (is.null(params$caminhoArquivo)) {
    params$caminhoArquivo <- paste(getwd(), "/", sep = "")
  }


  if (is.null(params$colors)) {
    params$colors <-
      colors <-
      c(
        'rgb(0,0,0)', #preto
        'rgb(192,192,192)',
        'rgb(65,105,225)',
        'rgb(60,179,113)',
        'rgb(244,164,96)',
        'rgb(75,0,130)',
        'rgb(225,100,100)',
        'rgb(100,150,200)',
        'rgb(255,180,72)'

      )
  }

  if (is.null(params$groups)) {
    params$groups <- c(0:15) * 10
  }

  if (is.null(params$pathFile)) {
    params$pathFile <- NA
  }

  return (params)
}

