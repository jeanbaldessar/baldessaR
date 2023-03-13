# Este script define algumas funções para trabalhar com a montagem de documentos

# library(tidyverse)
extraiUltimoCommit <- function(diretorio='.'){
  library(git2r)
  repo <- repository(diretorio)
  last_commit <- head(commits(repo))[[1]]
  hash <- last_commit$sha
  commit_date <- format(as.POSIXct(last_commit$author$when$time, origin="1970-01-01"),"%d/%m/%Y %H:%M")
  return(list(hash=hash, data=commit_date))
}

extraiVersaoPom <- function(arquivo){
  library(xml2)
  
  x <- read_xml(arquivo)
  versao <- xml_find_all(x, ".//d1:version", xml_ns(x))[1]
  texto <- xml_text(versao[[1]])
  
  return(texto)
}

#substituí links para arquivos que foram incluídos por referências para as sessões do documento
substituiLInksParaSessoes <- function(texto, sessoes){
  # percorre lista de títulos existentes para substituir referência de links locais por sessões
  referenciasEncontradas <- list()
  for (row in 1:nrow(sessoes)) {
    arquivo <- unlist(sessoes[row, "arquivo"])
    
    if(!is.null(arquivo) && !is.null(texto)){
      # Somente o nome do arquivo sem o caminho
      
      nomeArquivo <- nomeArquivo(arquivo)
      referenciaSessao <- identificadorSessao(nomeArquivo)
      
      procuraPor <- escapaExpressaoRegular(str_replace_all(arquivo, " ", "%20"))
      # subtituí link por referência para sessão
      referenciasEncontradas   <- Filter(length, append(referenciasEncontradas, str_extract_all(texto, paste0("\\[(.*?)\\]\\(", procuraPor, "\\.md\\)"))))
      texto <- str_replace_all(texto, paste0("\\[(.*?)\\]\\(", procuraPor, "\\.md\\)"), paste0("[\\1](#",referenciaSessao,")"))
    }
  }
  return(list(texto=texto, referenciasEncontradas=referenciasEncontradas))
}


substituiLInks <- function(texto, substituicoes){
  # percorre lista de títulos existentes para substituir referência para links locais por sessões
  for (row in 1:nrow(substituicoes)) {
    original <- paste0(substituicoes[row, "original"])
    substituicao <- paste0(substituicoes[row, "substituicao"])
    
    procuraPor <- escapaExpressaoRegular(str_replace_all(original, " ", "%20"))
    substituicaoUrl <- str_replace_all(substituicao, " ", "%20")
    
    # subtituí link por referência para sessão
    texto <- str_replace_all(texto, paste0("\\[(.*?)\\]\\(", procuraPor, "\\)"), paste0("[\\1](",substituicaoUrl,")"))
  }
  return(texto)
}

substituiGeral <- function(texto, substituicoes){
  for (row in 1:nrow(substituicoes)) {
    original <- escapaExpressaoRegular(paste0(substituicoes[row, "original"]))
    substituicao <- paste0(substituicoes[row, "substituicao"])
    texto <- str_replace_all(texto, original, substituicao)
  }
  return(texto)
}

escapaExpressaoRegular <- function(texto){
  gsub("(\\W)", "\\\\\\1", texto)
}

# Converte título para identificador de sessão de acordo com padrão pandoc
identificadorSessao <- function(nome){
  str_to_lower(str_replace_all(nome, " ", "-"))
}

# Extrai nome do arquivo de um caminho completo
nomeArquivo <- function(filename){
  paste0(str_replace_all(filename, ".*/", ""), collapse = "")
}

# inclui arquivo corrigindo referências
incluirSessaoDeArquivo <- function(filename, nivel=1, titulo, sessoes, substituicoesLinks, substituicoesGeral){
  
  linksExternos <- list()
  linksLocais <- list()
  linksLocaisQuebrados <- list()
  referenciasEncontradas <- list()
  textoGerado <- ""
  
  # Se o arquivo não exite, é apenas uma sessão de título
  if(is.null(filename)){
    textoGerado <- paste0("\n\n", paste0(rep("#",nivel), collapse = ""), " ", titulo, "\n\n", collapse = "")
  }else{
    nomeArquivo <- nomeArquivo(filename)
    if(is.null(titulo)){
      # se não tiver um título selecionado usa o nome do arquivo
      titulo <- nomeArquivo
    }
    # A referência para a sessão é baseada no nome do arquivo, não no título
    referenciaSessao <- identificadorSessao(nomeArquivo)
    pasta <- dirname(filename)
    
    # le conteúdo do arquivo
    res <- readLines(str_c(c(filename, ".md"), collapse = ""), encoding = "UTF-8")
    if(length(pasta)>0 & pasta != "."){
      # adiciona pasta em imagens
      res <- str_replace_all(res, "!\\[(.*?)\\]\\((.*?)\\)", paste0("![\\1](", pasta,"/\\2)"))
      # adiciona pasta em links para arquivos md
      res <- str_replace_all(res, "\\[(.+?)\\]\\(([\\w%/.-]*?)\\.md\\)", paste0("[\\1](", pasta,"/\\2.md)"))
      
    }
    
    # Caso tenha adicionado um item de uma subpasta, é possível que exita uma referencia do tipo PASTA1/../PASTA2.
    # Isso deve ser substituído por PASTA2
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    
    
    
    
    # altera links locais para sessões que foram incluídas na lista de sessões
    susbstituicoesSessoes <- substituiLInksParaSessoes(res, sessoes)
    res <- susbstituicoesSessoes$texto
    referenciasEncontradas <- susbstituicoesSessoes$referenciasEncontradas
    # altera outros links locais de acordo com lista de substituições
    res <- substituiLInks(res, substituicoesLinks)
    
    res <- substituiGeral(res, substituicoesGeral)
    
    # extrai links que não foram substituídos para conferência
    linksExternos <- Filter(length, append(list(), str_extract_all(res,  "\\[(.+?)\\]\\((https://|http://|www).*?\\)")))
    linksLocais   <- Filter(length, append(list(), str_extract_all(res,  "\\[(.+?)\\]\\(.*?\\.md\\)")))
    
    if(length(linksLocais)>0){
      links_existentes <- linksLocais[sapply(gsub("%20", " ",gsub("\\[.+?\\]\\((.*?\\.md)\\)", "\\1", linksLocais)), file.exists)]
      links_nao_existentes <- linksLocais[!sapply(gsub("%20", " ",gsub("\\[.+?\\]\\((.*?\\.md)\\)", "\\1", linksLocais)), file.exists)]
      
      
      linksLocais <- links_existentes
      linksLocaisQuebrados <- links_nao_existentes
    }
     
    # remove links locais (quebrados ou não) sem substituição
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?)\\.md\\)", "\\1")
    
    # imprime título com nível configurado
    textoTitulo <- paste0("\n\n", paste0(rep("#",nivel), collapse = ""), " ", titulo, " {#", referenciaSessao,"}\n\n", collapse = "")
    # imprime conteúdo com adaptações
    # cat("\nPasta:", pasta, "\n", sep = '\n')
    
    textoArquivo <- paste0(res, collapse = "\n")
    textoGerado <- paste0(textoTitulo, textoArquivo, collapse = "")
  }
  return(list(textoGerado=textoGerado, linksExternos=linksExternos, linksLocais=linksLocais, linksLocaisQuebrados=linksLocaisQuebrados, referenciasEncontradas=referenciasEncontradas))
}
 
#' Monta documentos
#' @export

montaDocumento <- function(sessoes, substituicoesLinks, substituicoesGeral){
  
  # Essa estrutura será retornada para verificação das referências
  sessoesRetornadas <- sessoes %>% filter(nivel==-1) %>% add_column(referencias=list())

  
  # Links que não foram substituídos para verificação
  linksExternos <- list()
  linksLocais <- list()
  linksLocaisQuebrados <- list()
  textoGerado <- ""
  
 
  # incluí todos os arquivos da lista fazendo todas as substituições necessárias
  # pode ser feito 'manualmente' para ter mais controle sobre a estrutura, mas normalmente um for simples vai resolver
  for (row in 1:nrow(sessoes)) {
    arquivo <- unlist(sessoes[row, "arquivo"])
    nivel <- unlist(sessoes[row, "nivel"])
    titulo <- unlist(sessoes[row, "titulo"])

    retorno <- incluirSessaoDeArquivo(filename=arquivo, nivel=nivel, titulo=titulo, sessoes, substituicoesLinks, substituicoesGeral)

    
    referencias=retorno$referenciasEncontradas

    sessoesRetornadas <- sessoesRetornadas %>% 
      rbind(
        tribble(
          ~nivel, ~titulo, ~arquivo, ~referencias,
          arquivo, nivel, titulo, referencias
        )
      )
    
    linksExternos         <- Filter(length, append(linksExternos         , retorno$linksExternos       ))
    linksLocais           <- Filter(length, append(linksLocais           , retorno$linksLocais         ))
    linksLocaisQuebrados  <- Filter(length, append(linksLocaisQuebrados  , retorno$linksLocaisQuebrados))

    incremento <- retorno$textoGerado
    anterior <- textoGerado
    textoGerado <- paste0(anterior, incremento, collapse = "")
    
  }
  
  return(list(sessoes=sessoesRetornadas, textoGerado=textoGerado, linksExternos=linksExternos, linksLocais=linksLocais, linksLocaisQuebrados=linksLocaisQuebrados))
  
}

#' @export

gerarDocumentos <- function(arquivoPandoc, profile='complete'){
  gerarDocumentosLeves(arquivoPandoc, profile)
  gerarPdf(arquivoPandoc, profile)
}

#' @export

gerarDocumentosLeves <- function(arquivoPandoc, profile='complete'){
  library(rmarkdown)
  
  fonte <- paste0(arquivoPandoc, '.Rmd')
  destino <- paste0(arquivoPandoc)
  pasta_destino <- paste0('build_', profile)
  
  render(fonte, output_file=paste0(destino, '.md'), output_format='md_document', params = list(profile=profile), output_dir = pasta_destino)
  pandoc_convert(paste0(destino, '.md'), output=paste0(destino, '.xwiki'), to='xwiki', wd=pasta_destino)
  arquivoWiki <- readLines(paste0(pasta_destino, '/', destino, '.xwiki'))
  
  arquivoWiki <- paste0('{{toc numbered="true"/}}\n\n', paste0(arquivoWiki, collapse = '\n'), collapse = '')
  # corrigindo sintaxe errada de links
  arquivoWiki <- str_replace_all(arquivoWiki, '>>#(.*?)\\]', '>>||anchor=\\1]')
  # removendo subpasta para imagens
  arquivoWiki <- str_replace_all(arquivoWiki, '\\[\\[image:.*/(.*?)\\]\\]', '[[image:\\1]]')
  writeLines(arquivoWiki, paste0(pasta_destino, '/', destino, '.xwiki'))
  
  render(fonte, output_file=paste0(destino, '-github.html'), output_format='bookdown::gitbook', params = list(profile=profile), output_dir = pasta_destino)
  render(fonte, output_file=paste0(destino, '.html'), output_format='html_document', params = list(profile=profile), output_dir = pasta_destino)
  render(fonte, output_file=paste0(destino, '.epub'), output_format='bookdown::epub_book', params = list(profile=profile), output_dir = pasta_destino)
}

#' @export

gerarPdf <- function(arquivoPandoc, profile='complete'){
  library(rmarkdown)
  fonte <- paste0(arquivoPandoc, '.Rmd')
  destino <- paste0(arquivoPandoc)
  pasta_destino <- paste0('build_', profile)
  render(fonte, output_file=paste0(destino, '.pdf'), output_format='bookdown::pdf_book', params = list(profile=profile), output_dir = pasta_destino)
  
}

#' @export

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

