sameCols <- function(df1, df2){
  flags <- names(df1) %in% names(df2)
  #flags <- names(DM) %in% names(HO)
  cols <- c()
  
  for (i in 1:length(flags)){
    #print(paste0("sameCols:Line 6: ", i))
    if (flags[i] == TRUE){
      cols <- append(cols, names(df1)[i])
      #cols <- append(cols, names(DM)[i])
    }
  }
  return(cols)
  
  #if (asOneString==FALSE){
  #  return(cols)
  #}
  #else if (asOneString==TRUE){
  #  oneString <- stringr::str_c("\"", cols, "\"", collapse = ",")
    #oneString <- stringr::str_c("\"", cols, "\"", collapse = ",")
  #  return(oneString)
  #}
  
}