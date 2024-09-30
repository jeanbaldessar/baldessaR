#' @export
renderAll <- function(arquivo=NULL, profile='complete', pasta_destino='build'){
  if (is.null(arquivo)){
    arquivo <- stringr::str_replace_all(list.files(pattern = "*.Rmd"), ".Rmd$", "")
  }
  renderLight(arquivo, profile, pasta_destino)
  renderPdf(arquivo, profile, pasta_destino)
}

#' @export
renderLight <- function(arquivo=NULL, profile='complete', pasta_destino='build'){
  library(rmarkdown)
  if (is.null(arquivo)){
    arquivo <- stringr::str_replace_all(list.files(pattern = "*.Rmd"), ".Rmd$", "")
  }

  fonte <- paste0(arquivo, '.Rmd')
  destino <- paste0(arquivo)
  diretorio_destino <- paste0(pasta_destino,'/',profile)

  createDir(diretorio_destino)

  # Gera documento markdown que servirá de base para o xwiki
  render(fonte, output_file=paste0(destino, '.md'), output_format='md_document', params = list(profile=profile), output_dir = diretorio_destino)

  # Gera xwiki a partir do markdown
  pandoc_convert(paste0(destino, '.md'), output=paste0(destino, '.xwiki'), to='xwiki', wd=diretorio_destino)
  arquivoWiki <- readLines(paste0(diretorio_destino, '/', destino, '.xwiki'))

  arquivoWiki <- paste0('{{toc numbered="true"/}}\n\n', paste0(arquivoWiki, collapse = '\n'), collapse = '')
  # corrigindo sintaxe errada de links
  arquivoWiki <- str_replace_all(arquivoWiki, '>>#(.*?)\\]', '>>||anchor=\\1]')
  # removendo subpasta para imagens
  arquivoWiki <- str_replace_all(arquivoWiki, '\\[\\[image:.*/(.*?)\\]\\]', '[[image:\\1]]')
  writeLines(arquivoWiki, paste0(diretorio_destino, '/', destino, '.xwiki'))

  render(fonte, output_file=paste0(destino, '-github.html'), output_format='bookdown::gitbook', params = list(profile=profile))
  file.rename(paste0(destino, '-github.html'), paste0(diretorio_destino,'/', destino, '-github.html'))
  render(fonte, output_file=paste0(destino, '.html'), output_format='html_document', params = list(profile=profile))
  file.rename(paste0(destino, '.html'), paste0(diretorio_destino,'/', destino, '.html'))
  render(fonte, output_file=paste0(destino, '.epub'), output_format='bookdown::epub_book', params = list(profile=profile))
  file.rename(paste0(destino, '.epub'), paste0(diretorio_destino,'/', destino, '.epub'))
}

#' @export
# Método feito para facilitar a renderização de um documento para pdf por linha de comando
# Este método chama rmarkdown::render com o formato bookdown::pdf_book o salva os documentos resultantes em uma pasta padrão
renderPdf <- function(arquivo=NULL, profile='complete', pasta_destino='build'){
  if (is.null(arquivo)){
    arquivo <- stringr::str_replace_all(list.files(pattern = "*.Rmd"), ".Rmd$", "")
  }

  fonte <- paste0(arquivo, '.Rmd')
  destino <- paste0(arquivo, '.pdf')
  diretorio_destino <- paste0(pasta_destino,'/',profile)

  rmarkdown::render(fonte,
         output_file=destino,
         output_format='bookdown::pdf_book',
         params = list(profile=profile)
         )

  createDir(diretorio_destino)

  # move o arquivo para o diretório destino
  file.rename(destino, to = paste0(diretorio_destino,'/',destino))
}



