is.brank.str <- function(strVal){
  if (is.character(strVal) == TRUE){
      str.length <- stringr::str_count(stringr::str_trim(strVal))
      if ( str.length == 0 ){
        return( TRUE )
      }
      else if ( str.length > 0 ){
        return( FALSE )
      }
  }
  else {
    stop(str_c(strVal, " is NOT character value!"))
  }
}