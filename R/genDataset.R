genDataset <- function(SpecFile, OutputDir, REDCapData){
  # Import General Spec
  General.Spec <- read.xlsx2(SpecFile, sheetName="General", stringsAsFactors=FALSE)
  # Import Variable Spec
  sdtm.variable.metadata <- read.csv(system.file("extdata", "sdtm.variable.metadata.csv", package="REDCap2SDTM"),
                                     sep=",", stringsAsFactors = FALSE)
  SDTM.Spec <- read.xlsx2(SpecFile, sheetName="VariableMetadata", stringsAsFactors=FALSE)
  uDomains <- unique(SDTM.Spec$SDTM.DOMAIN)
  uDomains <- uDomains[uDomains != ""] #remove blank element

  # Import Codelist Spec
  Codelist.Spec <- read.xlsx2(SpecFile, sheetName="Codelist", stringsAsFactors=FALSE)

  getFormData(REDCapData, SDTM.Spec, c("usubjid", "redcap_event_name"))


  # Map from REDCapa data to SDTM
  for (i in 1:length(uDomains)){
  #for (i in 1:1){

      SDTM.Spec_ <- SDTM.Spec[SDTM.Spec$SDTM.DOMAIN == uDomains[i],]
      bds <- blankDataset(uDomains[i])

      for ( j in 1:nrow(SDTM.Spec_)){
          form_name_ <- SDTM.Spec_[j, "form_name"]
          field_name_ <-  SDTM.Spec_[j, "field_name"]
          CODELIST.OID_ <- SDTM.Spec_[j, "CODELIST.OID"]
          SDTM.DOMAIN_ <- SDTM.Spec_[j, "SDTM.DOMAIN"]
          SDTM.VARIABLE_ <- SDTM.Spec_[j, "SDTM.VARIABLE"]
          SDTM.TESTCD_ <- SDTM.Spec_[j, "SDTM.TESTCD"]

          # --TESTCD is populated or not
          if (SDTM.TESTCD_ == ""){
              dftmp <- basedMapDf(form_name_, field_name_, SDTM.DOMAIN_, SDTM.VARIABLE_, Codelist.Spec, CODELIST.OID_)
              bds2 <- merge(bds, dftmp, by = names(dftmp), all=TRUE, stringsAsFactors=FALSE)
          }
          # something in --TESTCD
          else if (SDTM.TESTCD_ != ""){
            dftmp <- basedMapDf2(form_name_, field_name_, SDTM.DOMAIN_, SDTM.VARIABLE_, SDTM.TESTCD_, Codelist.Spec, CODELIST.OID_)
            bds2 <- merge(bds, dftmp, by = names(dftmp), all=TRUE, stringsAsFactors=FALSE)
          }
      dropNAVars(bds2)
      # Temporary accumulated dataframe: tmpds
      # If tmpds doen't exit, bds2 is transfered into tmpds
      if ( !("tmpds" %in% ls(envir=environment())) ){
        tmpds <- bds2
        rm(bds2)
      }
      else{
        #print(paste0(uDomains[i], ": 39に入った"))
        tmpds <- dropNAVars(tmpds)
        bds2 <- dropNAVars(bds2)
        by.cols <- sameCols(tmpds, bds2)
        names.tmpds <- names(tmpds)
        names.bds2 <- names(bds2)

        tmpds <- merge(tmpds, bds2, by = by.cols, all=TRUE, all.x = TRUE, stringsAsFactors=FALSE)
        #completion DTC
        tmpds <- cmplDTC(tmpds)
        rm(bds2)
      }
      bds <- blankDataset(uDomains[i])
      tmpds <- merge(bds, tmpds, by = names(tmpds), all=TRUE, stringsAsFactors=FALSE)
      tmpds$DOMAIN <- uDomains[i]
      tmpds$STUDYID <- General.Spec[General.Spec$Parameter=="STUDYID", "Value"]

      # remove redcap_event_name variable
      tmpds_ <- tmpds[, setdiff(colnames(tmpds), "redcap_event_name")]
      # fix variable order
      tmpds_ <- varRelocate(tmpds_)
      assign(uDomains[i], tmpds_, envir = .GlobalEnv)

      }
      rm(tmpds, tmpds_, bds2, by.cols, names.tmpds, names.bds2, form_name_, field_name_,
          SDTM.DOMAIN_, SDTM.VARIABLE_, SDTM.TESTCD)
  }
}
