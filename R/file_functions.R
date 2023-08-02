#' Extrai nome do arquivo de um caminho completo
#' removendo todo o texto até a última barra (/)
#' @export
extractFileName <- function(filename){
  paste0(str_replace_all(filename, ".*/", ""), collapse = "")
}

# cria o diretório destino se não existe ainda
createDir <- function(dir_name){
  if(!dir.exists(dir_name)){
    dir.create(dir_name, recursive = TRUE)
  }
}
