extraiUltimoCommit <- function(diretorio='.'){
  library(git2r)
  repo <- repository(diretorio)
  last_commit <- head(commits(repo))[[1]]
  hash <- last_commit$sha
  commit_date <- format(as.POSIXct(last_commit$author$when$time, origin="1970-01-01"),"%d/%m/%Y %H:%M")
  return(list(hash=hash, data=commit_date))
}