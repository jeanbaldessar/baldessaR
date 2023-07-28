Repositório com funções utilitárias em R.

Este projeto foi criado seguindo a ideia de Hilary Parker descrita [aqui](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)

Para utilizar estas funções como uma biblioteca, utilize o comando abaixo:

```r 
devtools::install_github('jeanbaldessar/baldessaR')
```

Ou baixe o código e rode os seguintes comandos a partir da pasta R do projeto:

```r
tmpDir <- getwd()
setwd("../")
devtools::document()
setwd("../")
devtools::install("baldessaR")
setwd(tmpDir)
rm(tmpDir)
```