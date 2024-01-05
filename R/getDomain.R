getDomain <- function(directive){
  vecStr <- stringr::str_split(directive, "\\.") 
  return(vecStr[[1]][2])
}