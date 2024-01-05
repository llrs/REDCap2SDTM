getTestcd <- function(directive){
  vecStr <- stringr::str_split(directive, "\\.") 
  if (length(vecStr[[1]]) >= 4){
      return(vecStr[[1]][4])
  }
  else{
      return("")
  }
}