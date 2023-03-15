# le todos os CSV para um dataframe identificando o arquivo e a linha
#' @export
leNubank <- function(pasta, conta){
  list.files(pattern = ".*.csv(.gz)?", path = pasta) %>% 
    map_df(~
             cbind(
               read_csv(paste0(pasta, "/", .)),
               list(ARQUIVO=str_replace(., '(\\.csv)?(\\.gz)?$', ''), CONTA=conta)
             ) %>% 
             mutate(LINHA=row_number(), amount=amount*-1)
    ) %>% 
    return()
}

#' @export
leNuconta <- function(pasta, conta){
  list.files(pattern = ".*.csv(.gz)?", path = pasta) %>% 
    map_df(~
             cbind(
               read_csv(
                 paste0(pasta, "/", .), 
                 col_types = cols(Data = col_date(format = "%d/%m/%Y"))
               ),
               list(ARQUIVO=str_replace(., '(\\.csv)?(\\.gz)?$', ''), CONTA=conta)) %>% 
             mutate(LINHA=row_number())
    )  %>% 
    return()
}

#' @export
leCaixa <- function(pasta, conta){
  list.files(pattern = "*.txt(.gz)?", path = pasta) %>% 
    map_df(~
             cbind(
               read_delim(
                 paste0(pasta, .), 
                 ";", 
                 escape_double = FALSE, 
                 trim_ws = TRUE, 
                 col_types = cols(
                   Conta = col_character(), 
                   Data_Mov = col_date(format = "%Y%m%d")
                 )
               ),
               list(ARQUIVO=str_replace(., '(\\.txt)?(\\.gz)?$', ''), CONTA=conta)
             ) %>% 
             mutate(LINHA=row_number())
    ) %>% 
    mutate(Valor = Valor*ifelse(Deb_Cred=='D', -1,1))  %>% 
    return()
}

#' @export
leBB <- function(pasta, conta){
  list.files(pattern = "*.csv(.gz)?", path = pasta) %>% 
    map_df(~
             cbind(
               read_csv(
                 paste0(pasta, .), 
                 col_types = cols(
                   Data = col_date(format = "%d/%m/%Y"), 
                   `Dependencia Origem` = col_character(), 
                   ...7 = col_skip(), 
                   `Número do documento` = col_character()
                 ), 
                 locale = locale(encoding = "ISO-8859-1")
               ),
               list(ARQUIVO=str_replace(., '(\\.csv)?(\\.gz)?$', ''), CONTA=conta)
             ) %>% 
             mutate(LINHA=row_number())
    ) %>% 
    filter(`Histórico` != 'Saldo Anterior' & `Histórico` != 'S A L D O') %>% 
    return()
}

#' @export
leCasan <- function(pasta, conta){
  list.files(pattern = "^\\d{6}.*\\.tsv", path = pasta) %>% 
    map_df(~ 
             cbind(
               read_delim(
                 paste0(pasta, .), 
                 "\t", 
                 escape_double = FALSE, 
                 col_types = cols(
                   Quantidade = col_number(), 
                   Valor = col_number()
                 ), 
                 locale = locale(
                   date_names = "pt", 
                   decimal_mark = ",", grouping_mark = "."
                 ), 
                 trim_ws = TRUE
               ),
               list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA=conta, DATA=add.bizdays(dmy(str_replace(., '(\\d\\d\\d\\d)(\\d\\d).*\\.tsv', '01/\\2/\\1')) %m+% months(1), -1, cal = calendario) )
             ) %>% mutate(LINHA=row_number())
    )
}


# casan_ferias <-
#     list.files(pattern = "^\\d{8}.*\\.tsv", path = "dados_casan_ferias/") %>% 
#     map_df(~ 
#       cbind(
#         read_delim(
#           paste0("dados_casan_ferias/", .), 
#           "\t", 
#           escape_double = FALSE, 
#           col_types = cols(
#             Quantidade = col_number(), 
#             Valor = col_number()
#           ), 
#           locale = locale(
#             date_names = "pt", 
#             decimal_mark = ",", grouping_mark = "."
#           ), 
#           trim_ws = TRUE
#         ),
#         list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA="casan", DATA=add.bizdays(dmy(str_replace(., '(\\d\\d\\d\\d)(\\d\\d)(\\d\\d).*\\.tsv', '\\3/\\2/\\1')), -1, cal = calendario))
#       ) %>% mutate(LINHA=row_number(), VALOR=PROVENTOS-DESCONTOS)
#     )

#' @export
lePapel <- function(pasta, conta){
  list.files(pattern = "*.tsv", path = pasta) %>% 
    map_df(~ cbind(
      read_delim(paste0(pasta, .), 
                 "\t", escape_double = FALSE, col_types = cols(DATA = col_date(format = "%d/%m/%Y"), 
                                                               VALOR = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                      grouping_mark = "."), trim_ws = TRUE), 
      list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA=conta)
    ) %>% mutate(LINHA=row_number()))
}

#' @export
leVale <- function(pasta, conta){
  list.files(pattern = "*.tsv", path = pasta) %>% 
    map_df(~ cbind(read_delim(paste0(pasta, .), 
          "\t", escape_double = FALSE, col_types = cols(Data = col_date(format = "%d/%m/%Y"), 
            Valor = col_number()), locale = locale(decimal_mark = ",", 
     grouping_mark = "."), trim_ws = TRUE), list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA=conta)) %>% 
     mutate(LINHA=row_number())) %>% mutate(Valor = Valor*ifelse(Local=='DISPONIB. BENEFICIO', 1,-1))
}

#' @export
leContaC6 <- function(pasta, conta){
  list.files(pattern = "*.tsv", path = pasta) %>% 
    map_df(~ cbind(
      read_delim(paste0(pasta, .), 
                 "\t", escape_double = FALSE, col_types = cols(DATA = col_date(format = "%d/%m/%Y"), 
                 VALOR = col_number()), locale = locale(decimal_mark = ",", 
                grouping_mark = "."), trim_ws = TRUE), 
             list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA=conta)
    ) %>% mutate(LINHA=row_number()))
}

#' @export
leCartaoC6 <- function(pasta, conta){
  list.files(pattern = "*.tsv", path = pasta) %>% 
    map_df(~ cbind(
      read_delim(paste0(pasta, .), 
                 "\t", escape_double = FALSE, col_types = cols(DATA = col_date(format = "%d/%m/%Y"), 
                                                               VALOR = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                      grouping_mark = "."), trim_ws = TRUE), 
      list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA=conta)
    ) %>% mutate(LINHA=row_number()))
}

#' @export
leFgts <- function(pasta, conta){
  list.files(pattern = "*.tsv", path = pasta) %>% 
    map_df(~ cbind(
      read_delim(paste0(pasta, .), 
                 "\t", escape_double = FALSE, col_types = cols(DATA = col_date(format = "%d/%m/%Y"), 
                                                               VALOR = col_number()), locale = locale(decimal_mark = ",", 
                                                                                                      grouping_mark = "."), trim_ws = TRUE), 
      list(ARQUIVO=str_replace(., '(\\.tsv)?(\\.gz)?$', ''), CONTA=conta)
    ) %>% 
      mutate(LINHA=row_number())) %>% 
    mutate(VALOR=if_else(str_starts(LANÇAMENTOS, "SAQUE"), -VALOR, VALOR))
}

#' @export
leAnotacoes <- function(arquivo){
  read_delim(arquivo, "\t", escape_double = FALSE, trim_ws = TRUE, #lazy=FALSE,
             col_types = cols(
               ARQUIVO = col_character(),
               SUBVALOR = col_number(),
               DATA_CORRIGIDA = col_date(format = '%d/%m/%Y')
               
             ),
             locale = locale(decimal_mark = ",", 
                             grouping_mark = "."), 
  ) %>% mutate(L=row_number())
}

#' @export
converteNubank <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA=date, VALOR=amount, DESCRICAO=title)
}

#' @export
converteNuconta <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA=Data, VALOR=Valor, DESCRICAO=`Descrição`)
}

#' @export
converteCaixa <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA=Data_Mov, VALOR=Valor, DESCRICAO=`Historico`)
}

#' @export
converteCasan <- function(.data){
  .data %>% 
    select(CONTA, ARQUIVO, LINHA, DATA, VALOR=Valor, DESCRICAO=`Descrição`) %>% 
    rbind(.data %>% totalizaCasan())
  
}

#' @export
totalizaCasan <- function(.data){
  .data %>% group_by(CONTA, ARQUIVO, DATA) %>% 
    summarise(VALOR=sum(Valor)*-1) %>% 
    mutate(DESCRICAO="Tranferencia para conta", LINHA=0) %>% 
    select(CONTA, ARQUIVO, LINHA, DATA, VALOR, DESCRICAO)
}

#' @export
converteBB <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA=Data, VALOR=Valor, DESCRICAO=`Histórico`)
}

#' @export
convertePapel <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA, VALOR, DESCRICAO)
}

#' @export
converteVale <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA=Data, VALOR=Valor, DESCRICAO=`Local`)
}

#' @export
converteContaC6 <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA, VALOR, DESCRICAO)
}

#' @export
converteCartaoC6 <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA, VALOR, DESCRICAO)
}

#' @export
converteFgts <- function(.data){
  .data %>% select(CONTA, ARQUIVO, LINHA, DATA, VALOR, DESCRICAO=LANÇAMENTOS)
}
