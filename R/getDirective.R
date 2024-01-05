getDirective <- function(field_annotation){
  directive <- str_split(str_split(field_annotation, "SDTM\\:")[[1]][2], "\\;")[[1]][1]
  return(directive)
}
