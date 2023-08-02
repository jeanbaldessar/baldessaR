#' Extract project version from a pom.xml file (maven)
#' @export
extractProjectVersionFromPomFile <- function(arquivo){
  library(xml2)

  x <- read_xml(arquivo)
  versao <- xml_find_all(x, ".//d1:version", xml_ns(x))[1]
  texto <- xml_text(versao[[1]])

  return(texto)
}
