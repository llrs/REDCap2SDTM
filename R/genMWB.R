genMWB <- function(redcap.md, file){
  general.spec <- read.csv(paste0(system.file("extdata", "general_spec.csv", package = "REDCap2SDTM")))
  blank.dataset.spec <- read.csv(paste0(system.file("extdata", "blank_dataset_md.csv", package = "REDCap2SDTM")))
  sdtm.spec <- genSDTMSpec(redcap.md)
  uDomains <- unique(sdtm.spec$SDTM.DOMAIN)
  uDomains <- uDomains[uDomains != ""]
  sdtm.dataset.metadata <- read.csv(system.file("extdata", "sdtm.dataset.metadata.csv", package="REDCap2SDTM"),
                                    sep=",", stringsAsFactors = FALSE)
  ref.dataset.md <- sdtm.dataset.metadata[sdtm.dataset.metadata$Dataset %in% uDomains,]
  dataset.spec <- rbind(blank.dataset.spec, ref.dataset.md)
  dataset.spec <- dataset.spec[-1, ] # delete blank record

  #visit.spec <-
  codelist.spec <- genCLSpec(redcap.md)
  ta.spec <- read.csv(paste0(system.file("extdata", "blank_ta.csv", package = "REDCap2SDTM")))
  te.spec <- read.csv(paste0(system.file("extdata", "blank_te.csv", package = "REDCap2SDTM")))
  tv.spec <- read.csv(paste0(system.file("extdata", "blank_tv.csv", package = "REDCap2SDTM")))
  ti.spec <- read.csv(paste0(system.file("extdata", "blank_ti.csv", package = "REDCap2SDTM")))
  ts.spec <- read.csv(paste0(system.file("extdata", "blank_ts.csv", package = "REDCap2SDTM")))

  write.xlsx2(general.spec, file, sheetName="General", col.names=TRUE, row.names=FALSE, append=FALSE)
  write.xlsx2(dataset.spec, file, sheetName="DatasetMetadata", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(sdtm.spec, file, sheetName="VariableMetadata", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(codelist.spec, file, sheetName="Codelist", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(ta.spec, file, sheetName="TA", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(te.spec, file, sheetName="TE", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(tv.spec, file, sheetName="TV", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(ti.spec, file, sheetName="TI", col.names=TRUE, row.names=FALSE, append=TRUE)
  write.xlsx2(ts.spec, file, sheetName="TS", col.names=TRUE, row.names=FALSE, append=TRUE)
}
