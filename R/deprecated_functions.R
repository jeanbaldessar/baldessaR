#' Deprecated. Use renderAll
#' @export
gerarDocumentosLeves <- function(arquivo, profile='complete', pasta_destino='build'){
  renderLight(arquivo, profile, pasta_destino)
}

#' Deprecated. Use renderAll
#' @export
gerarDocumentos <- function(arquivoPandoc, profile='complete', pasta_destino='build'){
  renderAll(arquivoPandoc, profile, pasta_destino)
}

#' Deprecated. Use extractLastCommitFromGitRepository
#' @export
extraiUltimoCommit <- function(diretorio='.'){
  extractLastCommitFromGitRepository(diretorio)
}

#' Deprecated. Use extractPomVersion
#' @export
extraiVersaoPom <- function(arquivo){
  extractProjectVersionFromPomFile(arquivo)
}

#' Deprecated. Use renderPdf
#' @export
gerarPdf <- function(arquivo, profile='complete', pasta_destino='build'){
  renderPdf(arquivo, profile, pasta_destino)
}


#' @export
# Deprecated. Agora que os arquivos são gerados dentro de uma pasta esse método não se faz necessário
excluirDocumentos <- function(arquivoPandoc){
  file.remove(
    paste0(arquivoPandoc, '.md'),
    paste0(arquivoPandoc, '.pdf'),
    paste0(arquivoPandoc, '.html'),
    paste0(arquivoPandoc, '-github.html'),
    paste0(arquivoPandoc, '.xwiki'),
    paste0(arquivoPandoc, '.epub')
  )
}
