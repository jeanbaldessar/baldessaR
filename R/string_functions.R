escapeEspaces <- function(texto){
  str_replace_all(texto, " ", "%20")
}

#' Escapa uma expressão regular para facilitar a busca pela string literal
#' @export
escapeRegexp <- function(texto){
  gsub("(\\W)", "\\\\\\1", texto)
}

#' Converte título para identificador de sessão de acordo com padrão pandoc
#' @export
convertToPandocSessionIdentifier <- function(nome){
  nome %>%
    str_to_lower() %>%
    str_replace_all(" ", "-") %>%
    str_replace_all("\\(|\\)", "")
}
