genCLSpec <- function(redcap.md) {
  tmpdf <- redcap.md[redcap.md$field_type == "radio",]
  choices <- tmpdf$select_choices_or_calculations
  varNames <- tmpdf$field_name
  
  for (i in 1:nrow((tmpdf))){
    tmpcltxt <- choices[i]
    tmpcl <- getREDCap.CL(tmpcltxt)
    varName <- varNames[i]
    if (i == 1){
        tmpcldf <- data.frame(varName, tmpcl$REDCAP.CLI.CODE, tmpcl$REDCAP.CLI.TERM)
    }else{
        tmpdf <- data.frame(varName, tmpcl$REDCAP.CLI.CODE, tmpcl$REDCAP.CLI.TERM)
        tmpcldf <- rbind(tmpcldf, tmpdf)
    }
  }
  
  rownames(tmpcldf) <- NULL
  vecBlk <- rep("", nrow(tmpcldf))

  tmpcldf <- within(tmpcldf, {SDTM.TERM <- vecBlk
                              CODELIST.NAME <- vecBlk
                              NCI.CODELIST.CODE <- vecBlk
                              NCI.CODE <- vecBlk
                              CODELIST.OID <- vecBlk
                              DECODED.VALUE <- vecBlk
                              })
  names(tmpcldf) <- c("REDCAP.VAR.NAME", "REDCAP.CLI.CODE", "REDCAP.CLI.TERM", "CODELIST.OID", "NCI.CODE", "NCI.CODELIST.CODE", "CODELIST.NAME", "SDTM.TERM", "DECODED.VALUE")
  
  return(tmpcldf)
}