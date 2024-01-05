getCodedVal <- function(rawval, Codelist.Df){
  codedval <- Codelist.Df[Codelist.Df$REDCAP.CLI.CODE == rawval, "SDTM.TERM"]
  return(codedval)
}