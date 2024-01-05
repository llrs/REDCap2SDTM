# basic mapping to variable level
basedMapDf <- function(form_name, field_name, SDTM.DOMAIN, SDTM.VARIABLE, Codelist.Spec, CODELIST.OID){
    prsTxt_usubjid <- paste0("USUBJID", " <- ", form_name, "[,\"usubjid\"]" )
    eval(parse(text = prsTxt_usubjid))
    
    prsTxt_event <- paste0("redcap_event_name", " <- ", form_name, "[,\"redcap_event_name\"]" )
    eval(parse(text = prsTxt_event))
    
    prsTxt2 <- paste0("var <- ", form_name, "[,\"", field_name,"\"]" )
    eval(parse(text = prsTxt2))

    df <- data.frame(USUBJID, redcap_event_name, var)
    
    # Convert to CDISC Terminology 
    if (CODELIST.OID != ""){
      cl <- subset(Codelist.Spec, CODELIST.OID == CODELIST.OID & REDCAP.VAR.NAME == field_name, select = c("REDCAP.VAR.NAME", "REDCAP.CLI.CODE", "CODELIST.OID", "NCI.CODE", "NCI.CODELIST.CODE", "CODELIST.NAME", "SDTM.TERM"))
      for( i in 1:nrow(df)){
        df[i, "var"] <- getCodedVal(df[i, "var"], cl)
      }
    }
    
    names(df) <- c("USUBJID", "redcap_event_name", SDTM.VARIABLE)
    return(df)
}