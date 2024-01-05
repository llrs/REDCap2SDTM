getFormData <- function(record, metadata, keyVars){
  forms <- unique(metadata$form_name)
  
  for ( i in forms){
    cols <- subset(metadata, form_name==i, select=field_name)
    names(cols) <- NULL
    cols_f <- unlist(subset(cols, str_detect(unlist(cols), "^desc_")==FALSE))
    if ("usubjid" %in% cols_f){
      cols_f <- c("redcap_event_name", cols_f)
    }
    else{
      cols_f <- c(c("redcap_event_name", "usubjid"), cols_f)
    }
    

    #print(cols_f)
    #print( stringr::str_c(cols_f, collapse="\", \""))
    prsTxt <- paste0(i, " <<- record[, c(\"", stringr::str_c(cols_f, collapse="\", \""), "\")]")
    eval(parse(text=prsTxt))
    
    #delete blank records
    prsTxt2 <- paste0(i, " <<- delBlankRecord(", i, ", c(\"", stringr::str_c(keyVars, collapse="\", \""), "\"))")
    eval(parse(text=prsTxt2))
  
  }
}