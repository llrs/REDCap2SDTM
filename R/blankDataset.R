blankDataset <- function(domain){
  sdtm.variable.metadata <- read.csv(system.file("extdata", "sdtm.variable.metadata.csv", package="REDCap2SDTM"),
                                      sep=",", stringsAsFactors = FALSE)
  sdtm.vars <- sdtm.variable.metadata[sdtm.variable.metadata$Domain==domain,"Variable"]

  for (i in 1:length(sdtm.vars)){
    prsTxt <- paste0(sdtm.vars[i], " <- ", "c(NA)")
    eval(parse(text=prsTxt))
  }

  #add redcap_event_name to SDTM domain variables
  redcap_event_name <- c(NA)
  prsTxt2 <- paste0(domain, " <- ", "data.frame(", paste0(sdtm.vars, collapse=", "),
                    ", redcap_event_name", ",row.names = FALSE, stringsAsFactors=FALSE)")
  eval(parse(text=prsTxt2))
  prsTxt3 <- paste0(domain, " <- ", "na.omit(", domain, ")")
  eval(parse(text=prsTxt3))

}
