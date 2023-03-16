# Este script define algumas funções para trabalhar com a montagem de documentos

#' substituí links para arquivos que foram incluídos por referências para as sessões do documento
substituiLinksParaSessoes <- function(texto, sessoes){
  library(stringi)
  
  # percorre lista de títulos existentes para substituir referência de links locais por sessões
  referenciasEncontradas <- list()
  for (row in 1:nrow(sessoes)) {
    arquivo <- unlist(sessoes[row, "arquivo"])
    
    if(!is.null(arquivo) && !is.null(texto)){
      # Somente o nome do arquivo sem o caminho
      
      nomeArquivo <- nomeArquivo(arquivo)
      referenciaSessao <- identificadorSessao(nomeArquivo)
      
      procuraPor <- escapaExpressaoRegular(escapaEspacos(arquivo))
      procurarPorCompleto <- paste0("\\[([\\w%/._ (),‘’+-]*?)\\]\\(", procuraPor, "\\.md\\)")
      extractAll <- str_extract_all(texto, procurarPorCompleto)
      
      # subtituí link por referência para sessão
      referenciasEncontradas   <- paste(unlist(Filter(length, append(referenciasEncontradas, extractAll))),collapse="\n")
      texto <- str_replace_all(texto, paste0("\\[([\\w%/._ (),‘’+-]*?)\\]\\(", procuraPor, "\\.md\\)"), paste0("[\\1](#",referenciaSessao,")"))
    }
  }
  return(list(texto=texto, referenciasEncontradas=referenciasEncontradas))
}

#' substituí links
substituiLInks <- function(texto, substituicoes){
  # percorre lista de títulos existentes para substituir referência para links locais por sessões
  for (row in 1:nrow(substituicoes)) {
    original <- paste0(substituicoes[row, "original"])
    substituicao <- paste0(substituicoes[row, "substituicao"])
    
    procuraPor <- escapaExpressaoRegular(escapaEspacos(original))
    substituicaoUrl <- escapaEspacos(substituicao)
    
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


escapaEspacos <- function(texto){
  str_replace_all(texto, " ", "%20")
}

#' Escapa uma expressão regular para facilitar a busca pela string literal
#' @export
escapaExpressaoRegular <- function(texto){
  gsub("(\\W)", "\\\\\\1", texto)
}

#' Converte título para identificador de sessão de acordo com padrão pandoc
#' @export
identificadorSessao <- function(nome){
  str_to_lower(str_replace_all(nome, " ", "-"))
}

#' Extrai nome do arquivo de um caminho completo
#' removendo todo o texto até a última barra (/)
#' @export
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
    res <- paste(res, collapse = "\n")
    
    
    if(length(pasta)>0 & pasta != "."){
      # adiciona pasta em imagens
      res <- str_replace_all(res, "!\\[([\\w%/._ (),‘’+-]*?)\\]\\(([\\w%/._,‘’+-]*?)\\)", paste0("![\\1](", pasta,"/\\2)"))
      # adiciona pasta em links para arquivos md
      res <- str_replace_all(res, "\\[([\\w%/._ (),‘’+-]+?)\\]\\(([\\w%/._,‘’+-]*?)\\.md\\)", paste0("[\\1](", pasta,"/\\2.md)"))
      
    }
    
    # Caso tenha adicionado um item de uma subpasta, é possível que exita uma referencia do tipo PASTA1/../PASTA2.
    # Isso deve ser substituído por PASTA2
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    
   
    
    
    # altera links locais para sessões que foram incluídas na lista de sessões
    susbstituicoesSessoes <- substituiLinksParaSessoes(res, sessoes)
    res <- susbstituicoesSessoes$texto
    referenciasEncontradas <- susbstituicoesSessoes$referenciasEncontradas
    # altera outros links locais de acordo com lista de substituições
    res <- substituiLInks(res, substituicoesLinks)
    
    res <- substituiGeral(res, substituicoesGeral)
    
    # extrai links que não foram substituídos para conferência
    linksExternos <- Filter(length, append(list(), str_extract_all(res,  "\\[([\\w%/._ (),‘’+-]+?)\\]\\((https://|http://|www)[\\w%/.-]*?\\)")))
    linksLocais   <- Filter(length, append(list(), str_extract_all(res,  "\\[([\\w%/._ (),‘’+-]+?)\\]\\([\\w%/._,‘’+-]*?\\.md\\)")))
    
    if(length(linksLocais)>0){
      links_existentes 		<- linksLocais[ sapply(gsub("%20", " ",gsub("\\[[\\w%/._ (),‘’+-]+?\\]\\(([\\w%/._,‘’+-]*?\\.md)\\)", "\\1", linksLocais)), file.exists)]
      links_nao_existentes 	<- linksLocais[!sapply(gsub("%20", " ",gsub("\\[[\\w%/._ (),‘’+-]+?\\]\\(([\\w%/._,‘’+-]*?\\.md)\\)", "\\1", linksLocais)), file.exists)]
      
      # browser()
      linksLocais <- links_existentes
      linksLocaisQuebrados <- links_nao_existentes
    }
     
    # remove links locais (quebrados ou não) sem substituição
    res <- str_replace_all(res, "\\[([\\w%/._ (),‘’+-]+?)\\]\\(([\\w%/._()-]*?)\\.md\\)", "\\1")
    
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
  sessoesRetornadas <- sessoes %>% filter(nivel==-1) %>% 
    add_column(referencias=list()) %>% 
    add_column(textoGerado="")

  
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

    
    sessoesRetornadas <- sessoesRetornadas %>% 
      rbind(
        tribble(
          ~nivel, ~titulo, ~arquivo, ~referencias, ~textoGerado,
          nivel, titulo, arquivo, retorno$referenciasEncontradas, retorno$textoGerado
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

#' Imprime estrutura gerada por montaDocumento incluíndo backlinks
#' @export

imprimirComReferencias <- function(retorno, ignoraReferenciasDe){
  for (row in 1:nrow(retorno$sessoes)) {
    cat(retorno$sessoes$textoGerado[row])  
    
    
    teste <- retorno$sessoes %>%
      filter(!arquivo %in% ignoraReferenciasDe) %>%  # removendo sessões de índices
      filter(grepl(escapaEspacos(retorno$sessoes$arquivo[row]), referencias ))
    
    teste <- teste %>% 
      arrange(coalesce(paste0(titulo), paste0(nomeArquivo(arquivo))))
    
    if(nrow(teste)>0){
      
      cat("\n\n\nReferenciado por:\n\n")
      
      for (row2 in 1:nrow(teste)) {  
        arquivo2 <- unlist(teste$arquivo[row2])
        titulo2 <- unlist(teste$titulo[row2])
        
        if(is.null(titulo2)){
          titulo2 <- nomeArquivo(arquivo2)
        }
        
        cat("- [",titulo2, "](#",arquivo2 %>% nomeArquivo() %>% identificadorSessao(),")\n", sep = "")
        
      }
      cat("\n\n\n")
    }
  }
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

