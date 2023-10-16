# Este script define algumas funções para trabalhar com a montagem de documentos

#' substituí links para arquivos que foram incluídos por referências para as sessões do documento
replaceSessionLinks <- function(texto, sessoes){
  library(stringi)

  # percorre lista de títulos existentes para substituir referência de links locais por sessões
  referenciasEncontradas <- list()
  for (arquivo in sessoes) {

    if(!is.null(arquivo) && !is.null(texto)){
      # Somente o nome do arquivo sem o caminho

      nomeArquivo <- extractFileName(arquivo)
      referenciaSessao <- convertToPandocSessionIdentifier(nomeArquivo)

      procuraPor <- escapeRegexp(escapeEspaces(arquivo))
      procurarPorCompleto <- paste0("\\[([\\w%/._(),‘’'+ -]*?)\\]\\(", procuraPor, "\\.md\\)")
      extractAll <- str_extract_all(texto, procurarPorCompleto)
      # guarda referências encontradas para permitir a criação de backlinks posteriormente
      referenciasEncontradas   <- paste(unlist(Filter(length, append(referenciasEncontradas, extractAll))),collapse="\n")

      # subtituí link por referência para sessão
      texto <- str_replace_all(texto, paste0("\\[([\\w%/._(),‘’'+ -]*?)\\]\\(", procuraPor, "\\.md\\)"), paste0("[\\1](#",referenciaSessao,")"))
    }
  }
  return(list(texto=texto, referenciasEncontradas=referenciasEncontradas))
}

#' substituí links
replaceLinks <- function(texto, substituicoes){
  # se não veio nada para sustituir retornar o texto normal
  if(is.null(substituicoes))
    return(texto)

  # percorre lista de títulos existentes para substituir referência para links locais por sessões
  for (row in 1:nrow(substituicoes)) {
    original <- paste0(substituicoes[row, "original"])
    substituicao <- paste0(substituicoes[row, "substituicao"])

    procuraPor <- escapeRegexp(escapeEspaces(original))
    substituicaoUrl <- escapeEspaces(substituicao)

    # subtituí link por referência para sessão
    texto <- str_replace_all(texto, paste0("\\[(.*?)\\]\\(", procuraPor, "\\)"), paste0("[\\1](",substituicaoUrl,")"))
  }
  return(texto)
}

replaceList <- function(texto, substituicoes){
  # se não veio nada para sustituir retornar o texto normal
  if(is.null(substituicoes))
    return(texto)

  for (row in 1:nrow(substituicoes)) {
    original <- escapeRegexp(paste0(substituicoes[row, "original"]))
    substituicao <- paste0(substituicoes[row, "substituicao"])
    texto <- str_replace_all(texto, original, substituicao)
  }
  return(texto)
}


# inclui arquivo corrigindo referências
includeFileSession <- function(filename, nivel=1, titulo, sessoes, substituicoesLinks=NULL, substituicoesGeral=NULL){

  linksExternos <- list()
  linksLocais <- list()
  linksLocaisQuebrados <- list()
  referenciasEncontradas <- list()
  textoGerado <- ""

  # Se o arquivo não exite, é apenas uma sessão de título
  if(is.null(filename)){
    textoGerado <- paste0("\n\n", paste0(rep("#",nivel), collapse = ""), " ", titulo, "\n\n", collapse = "")
  }else{
    nomeArquivo <- extractFileName(filename)
    if(is.null(titulo)){
      # se não tiver um título selecionado usa o nome do arquivo
      titulo <- nomeArquivo
    }
    # A referência para a sessão é baseada no nome do arquivo, não no título
    referenciaSessao <- convertToPandocSessionIdentifier(nomeArquivo)
    pasta <- dirname(filename)

    # le conteúdo do arquivo
    res <- readLines(str_c(c(filename, ".md"), collapse = ""), encoding = "UTF-8")
    res <- paste(res, collapse = "\n")


    if(length(pasta)>0 & pasta != "."){
      # adiciona pasta em imagens
      res <- str_replace_all(res, "!\\[([\\w%/._(),‘’'+ -]*?)\\]\\(([\\w%/._(),‘’'+-]*?)\\)", paste0("![\\1](", pasta,"/\\2)"))
      # adiciona pasta em links para arquivos md
      res <- str_replace_all(res, "\\[([\\w%/._(),‘’'+ -]+?)\\]\\(([\\w%/._(),‘’'+-]*?)\\.md\\)", paste0("[\\1](", pasta,"/\\2.md)"))

    }

    # Caso tenha adicionado um item de uma subpasta, é possível que exita uma referencia do tipo PASTA1/../PASTA2.
    # Isso deve ser substituído por PASTA2
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))
    res <- str_replace_all(res, "\\[(.+?)\\]\\((.*?/)?\\w*?/../(.*?)\\.md\\)", paste0("[\\1](\\2\\3.md)"))




    # altera links locais para sessões que foram incluídas na lista de sessões
    susbstituicoesSessoes <- replaceSessionLinks(res, sessoes$arquivo %>% unlist)
    res <- susbstituicoesSessoes$texto
    referenciasEncontradas <- susbstituicoesSessoes$referenciasEncontradas
    # altera outros links locais de acordo com lista de substituições
    res <- replaceLinks(res, substituicoesLinks)

    res <- replaceList(res, substituicoesGeral)

    # extrai links que não foram substituídos para conferência
    linksExternos <- Filter(length, append(list(), str_extract_all(res,  "\\[([\\w%/._(),‘’'+ -]+?)\\]\\((https://|http://|www)[\\w%/.-]*?\\)")))
    linksLocais   <- Filter(length, append(list(), str_extract_all(res,  "\\[([\\w%/._(),‘’'+ -]+?)\\]\\([\\w%/._(),‘’'+-]*?\\.md\\)")))

    if(length(linksLocais)>0){
      links_existentes 		<- linksLocais[ sapply(gsub("%20", " ",gsub("\\[[\\w%/._(),‘’'+ -]+?\\]\\(([\\w%/._(),‘’'+-]*?\\.md)\\)", "\\1", linksLocais)), file.exists)]
      links_nao_existentes 	<- linksLocais[!sapply(gsub("%20", " ",gsub("\\[[\\w%/._(),‘’'+ -]+?\\]\\(([\\w%/._(),‘’'+-]*?\\.md)\\)", "\\1", linksLocais)), file.exists)]

      # browser()
      linksLocais <- links_existentes
      linksLocaisQuebrados <- links_nao_existentes
    }

    # remove links locais (quebrados ou não) sem substituição
    res <- str_replace_all(res, "\\[([\\w%/._(),‘’'+ -]+?)\\]\\(([\\w%/._(),‘’'+-]*?)\\.md\\)", "\\1")

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

buildDocument <- function(sessoes, substituicoesLinks=NULL, substituicoesGeral=NULL){

  # Essa estrutura será retornada para verificação das referências
  sessoesRetornadas <- sessoes %>% filter(nivel==-1) %>%
    add_column(referencias=list()) %>%
    add_column(textoGerado="")


  # Links que não foram substituídos para verificação
  linksExternos <- list()
  linksLocais <- list()
  linksLocaisQuebrados <- list()


  # incluí todos os arquivos da lista fazendo todas as substituições necessárias
  # pode ser feito 'manualmente' para ter mais controle sobre a estrutura, mas normalmente um for simples vai resolver
  for (row in 1:nrow(sessoes)) {
    arquivo <- sessoes$arquivo[[row]]
    nivel <- sessoes$nivel[[row]]
    titulo <- sessoes$titulo[[row]]

    retorno <- includeFileSession(filename=arquivo, nivel=nivel, titulo=titulo, sessoes, substituicoesLinks, substituicoesGeral)


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
  }

  return(list(sessoes=sessoesRetornadas, linksExternos=linksExternos, linksLocais=linksLocais, linksLocaisQuebrados=linksLocaisQuebrados))

}

#' Imprime estrutura gerada por montaDocumento incluíndo backlinks
#' @export

printWithBacklinks <- function(imprimir, referencias, ignoraReferenciasDe){
  for (row in 1:nrow(imprimir)) {
    cat(imprimir$textoGerado[row])

    arquivoAtual <- imprimir$arquivo[row]
    arquivoAtualEscapacdo <- escapeEspaces(arquivoAtual)

    teste <- referencias %>%
      filter(!arquivo %in% ignoraReferenciasDe) %>%  # removendo sessões de índices
      filter(grepl(escapeRegexp(arquivoAtualEscapacdo), referencias ))

    teste <- teste %>%
      arrange(coalesce(paste0(titulo), paste0(extractFileName(arquivo))))

    if(nrow(teste)>0){

      cat("\n\n\nReferenciado por:\n\n")

      for (row2 in 1:nrow(teste)) {
        arquivo2 <- unlist(teste$arquivo[row2])
        titulo2 <- unlist(teste$titulo[row2])

        if(is.null(titulo2)){
          titulo2 <- extractFileName(arquivo2)
        }

        cat("- [",titulo2, "](#",arquivo2 %>% extractFileName() %>% convertToPandocSessionIdentifier(),")\n", sep = "")

      }
      cat("\n\n\n")
    }
  }
}