cmplDTC <- function(df){
  #find a vrialble named '--DTC'
  varDTCs <- names(df)[str_detect(names(df), "^.{2}DTC$")==TRUE]
  if (length(varDTCs)>0){
    prsTxt <- paste0(
      "DTC <- subset(df, is.na(", varDTCs, ")==FALSE, select=c(USUBJID, redcap_event_name, ", varDTCs, "))"
      )
    eval(parse(text = prsTxt))
    #Remove duplicate
    DTC <- DTC[!duplicated(DTC), ]
  
    df2 <- merge( df, DTC, by = c("USUBJID", "redcap_event_name"),  all = TRUE)
  
    varDTCs.x <- paste0(varDTCs, ".x")
    varDTCs.y <- paste0(varDTCs, ".y")
    for (i in 1:nrow(df2)){
      if ( is.na(df2[i, varDTCs.x])){
        df2[i, varDTCs.x] <- df2[i, varDTCs.y]
      }
    }
    prsTxt1 <- paste0("names(df2$",varDTCs.x,") <- \"", varDTCs, "\"")
    eval(parse(text = prsTxt1))
  
    #rename varname to --DTC
    colnames(df2)[which(names(df2) == varDTCs.x)] <- varDTCs  
    #drop varDTCs.y
    prsTxt2 <- paste0("df2$",varDTCs.y," <- NULL")
    eval(parse(text = prsTxt2))

  
    return(df2)
  }
  else{
    return(df)
  }
}