# Funções uteis relacionadas à criação de packages e funções genéricas do R

#' Compila package a partir do diretório de trabalho pai do atual
#' @export

installUpdatedBaldessaR <- function(){
  tmpDir <- getwd()
  setwd("../")
  devtools::document()
  setwd("../")
  devtools::install("baldessaR")
  setwd(tmpDir)
  rm(tmpDir)
}