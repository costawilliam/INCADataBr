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
        'rgb(0,0,0)',
        'rgb(128,0,0)',
        'rgb(255,0,0)',
        'rgb(255,255,0)',
        'rgb(128,128,0)',
        'rgb(255,0,255)',
        'rgb(128,255,128)',
        'rgb(255,255,128)',
        'rgb(0,0,255)',
        'rgb(0,128,0)',
        'rgb(0,255,255)',
        'rgb(0,255,0)',
        'rgb(0,255,128)',
        'rgb(128,255,255)',
        'rgb(0,0,128)',
        'rgb(128,0,128)',
        'rgb(128,0,255)',
        'rgb(128,128,128)',
        'rgb(128,128,255)',
        'rgb(0,128,128)',
        'rgb(255,128,128)',
        'rgb(255,0,128)',
        'rgb(128,255,0)',
        'rgb(0,128,255)',
        'rgb(255,128,0)',
        'rgb(255,128,255)'
      )
  }

  if (is.null(params$groups)) {
    params$groups <- c(0:15) * 10
  }

  return (params)
}

