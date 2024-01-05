genSDTMSpec <- function(redcap.md){
  cols <- c("field_name", "form_name", "field_type", "field_label", 
            "select_choices_or_calculations", "text_validation_type_or_show_slider_number", 
            "required_field", "field_annotation")
  tmpdf <- redcap.md[,cols]
  
  vecBlk <- rep("", nrow(tmpdf))
  tmpdf <- within(tmpdf, {SDTM.TESTCD <- vecBlk
                          SDTM.VARIABLE <- vecBlk
                          SDTM.DOMAIN <- vecBlk
                          CODELIST.OID <- vecBlk})

  for (i in 1:nrow(tmpdf)){
    if (tmpdf[i, "field_annotation"] != ""){
      strDirective <- getDirective(tmpdf[i, "field_annotation"])
      tmpdf[i, "SDTM.DOMAIN"] <- as.character(getDomain(strDirective))
      tmpdf[i, "SDTM.VARIABLE"] <- getVariable(strDirective)
      tmpdf[i, "SDTM.TESTCD"] <- getTestcd(strDirective)
    }
  }

  
  return(tmpdf)

}