genDefine.Spec <- function(SDTMSpecFile, DefineSpecFile){

  # Import Variable Spec

  sdtm.variable.metadata <- read.csv(system.file("extdata", "sdtm.variable.metadata.csv", package="REDCap2SDTM"),
                                     sep=",", stringsAsFactors = FALSE)
  var.mapping.spec <- read.xlsx2(SDTMSpecFile, sheetName="VariableMetadata", stringsAsFactors=FALSE)
  uDomains <- unique(var.mapping.spec$SDTM.DOMAIN)
  uDomains <- uDomains[uDomains != ""] #remove blank element
  uDomains <- append(uDomains, c("TA", "TE", "TV", "TI", "TS")) #add TDM datasets

  # Make Study Metadata
  General.Spec <- read.xlsx2(SDTMSpecFile, sheetName="General", stringsAsFactors=FALSE)
  std.Name <- General.Spec[General.Spec$Parameter=="STUDYID", "Value"]
  std.Attribute <- c("StudyName", "StudyDescription", "ProtocolName", "StandardName", "StandardVersion", "Language")
  std.Value <- c(std.Name, "", std.Name, "SDTM-IG", "3.2", "en")
  std.md.spec <- data.frame(Attribute=std.Attribute, Value=std.Value)

  write.xlsx2(std.md.spec, DefineSpecFile, sheetName="Study", col.names=TRUE, row.names=FALSE, append=FALSE)

  # Make dataset Level Metadata
  # Cols: Dataset, Description, Class, Structure, Purpose, Key Variables,
  #       Repeating, Reference Data, Comment

  #ref.dataset.md <- sdtm.dataset.metadata[sdtm.dataset.metadata$Dataset %in% uDomains,]
  def.dataset.md <- read.xlsx2(SDTMSpecFile, sheetName="DatasetMetadata", stringsAsFactors=FALSE)

  ds.Dataset <- c("Dataset", def.dataset.md$Dataset)
  ds.Description <- c("Description", def.dataset.md$Description)
  ds.Class <- c("Class", def.dataset.md$Class)
  ds.Structure <- c("Structure", def.dataset.md$Structure)
  ds.Purpose <- c("Purpose")
  ds.Key.Variables <- c("Key Variables", def.dataset.md$Key.Variables)
  ds.Repeating <- c("Repeating")
  ds.Reference.Data <- c("Reference Data")
  ds.Comment <- c("Comment")

  ds.md.spec <- data.frame(ds.Dataset,
                           ds.Description,
                           ds.Class,
                           ds.Structure,
                           ds.Purpose,
                           ds.Key.Variables,
                           ds.Repeating,
                           ds.Reference.Data,
                           ds.Comment,
                           stringsAsFactors = FALSE)

  for (i in 2:nrow(ds.md.spec)){
    ds.md.spec[i,"ds.Purpose"] <- "Tabulation"
    ds.md.spec[i,"ds.Comment"] <- ""
    if (ds.md.spec[i,"ds.Dataset"] %in% c("DM", "TA", "TE", "TI", "TS", "TV")){
      ds.md.spec[i,"ds.Repeating"] <- "No"
    }else{
      ds.md.spec[i,"ds.Repeating"] <- "Yes"
    }
    #Processing for Reference Data
    if (ds.md.spec[i,"ds.Dataset"] %in% c("TA", "TE", "TI", "TS", "TV")){
      ds.md.spec[i,"ds.Reference.Data"] <- "Yes"
    }else{
      ds.md.spec[i,"ds.Reference.Data"] <- "No"
    }
  }
  write.xlsx2(ds.md.spec, DefineSpecFile, sheetName="Datasets", col.names=FALSE, row.names=FALSE, append=TRUE)



  # Make Variable Level Metadata
  # Order	Dataset	Variable	Label, Data Type,	Length,	Significant Digits,
  # Format,	Mandatory, Codelist, Origin, Pages, Method, Predecessor, Role,
  # Comment
  ref.metadata <- sdtm.variable.metadata[sdtm.variable.metadata$Domain %in% uDomains,]
  def.var.md <- read.csv(system.file("extdata", "blank.def.variable.spec.csv", package="REDCap2SDTM"),
                         sep=",", stringsAsFactors = FALSE)
  # extract records with non blank field_annotation
  var.mapping.spec2 <- subset(var.mapping.spec, field_annotation != "", select=c("CODELIST.OID", "SDTM.DOMAIN", "SDTM.VARIABLE"))
  # elimininate the duplicated record with the variable level
  var.mapping.spec2 <- var.mapping.spec2 %>% dplyr::distinct(CODELIST.OID, SDTM.DOMAIN, SDTM.VARIABLE)
  vartmpdf <- merge(ref.metadata, var.mapping.spec2, all.x=TRUE, all.y=TRUE, by.x=c("Domain", "Variable"), by.y=c("SDTM.DOMAIN", "SDTM.VARIABLE"))

  var.Order <- c("Order", vartmpdf$Order)
  var.Dataset <- c("Dataset", vartmpdf$Domain)
  var.Variable <- c("Variable", vartmpdf$Variable)
  var.Label <- c("Label", vartmpdf$Label)
  var.Data.Type <- c("Data Type", vartmpdf$Type)
  var.Length <- c("Length")
  var.Significant.Digits <- c("Significant Digits")
  var.Format <- c("Format")
  var.Mandatory <- c("Mandatory", vartmpdf$Core)
  var.Codelist <- c("Codelist", vartmpdf$CODELIST.OID)
  var.Origin <- c("Origin")
  var.Pages <- c("Pages")
  var.Method <- c("Method")
  var.Predecessor <- c("Predecessor")
  var.Role <- c("Role", vartmpdf$Role)
  var.Comment <- c("Comment")

  var.md.spec <- data.frame(var.Order, var.Dataset,
                            var.Variable, var.Label,
                            var.Data.Type, var.Length,
                            var.Significant.Digits,
                            var.Format, var.Mandatory,
                            var.Codelist, var.Origin,
                            var.Pages, var.Method,
                            var.Predecessor, var.Role,
                            var.Comment,
                            stringsAsFactors = FALSE)
  #var.md.spec <<- var.md.spec[order(var.md.spec$var.Dataset, pmax(var.md.spec$var.Dataset, var.md.spec$var.Order)),]
  # Output Col name
  var.md.spec.header <- var.md.spec[1,]


  var.md.spec2 <- var.md.spec[-1,]
  var.md.spec2$var.Order <- as.numeric(var.md.spec2$var.Order)
  var.md.spec2 <- var.md.spec2 %>% dplyr::arrange(var.Dataset , var.Order)
  var.md.spec2$var.Order <- as.character(var.md.spec2$var.Order)
  var.md.spec2 <- rbind(var.md.spec.header, var.md.spec2)

  for (i in 2:nrow(var.md.spec2)){
    var.md.spec2[i,"var.Length"] <- ""
    var.md.spec2[i,"var.Significant.Digits"] <- ""
    var.md.spec2[i,"var.Format"] <- ""
    var.md.spec2[i,"var.Origin"] <- ""
    var.md.spec2[i,"var.Method"] <- ""
    var.md.spec2[i,"var.Pages"] <- ""
    var.md.spec2[i,"var.Predecessor"] <- ""
    var.md.spec2[i,"var.Comment"] <- ""
    #Processing for data type
    if (var.md.spec2[i,"var.Data.Type"] == "Char"){
      var.md.spec2[i,"var.Data.Type"] <- "text"
    }else{
      var.md.spec2[i,"var.Data.Type"] <- "integer"
    }
    #Processing for date type
    if (str_detect(var.md.spec2[i,"var.Variable"], "DTC$") ){
      var.md.spec2[i,"var.Data.Type"] <- "date"
    }
    #Processing for Mandatory
    if (var.md.spec2[i,"var.Mandatory"] == "Req"){
      var.md.spec2[i,"var.Mandatory"] <- "Yes"
    }else{
      var.md.spec2[i,"var.Mandatory"] <- "No"
    }
    #Processing for Codelist
    if ( !is.na(var.md.spec2[i, "var.Codelist"])){
      var.md.spec2[i,"var.Codelist"] <- stringr::str_replace(var.md.spec2[i,"var.Codelist"], "CL\\.", "")
    }
  }
  write.xlsx2(var.md.spec2, DefineSpecFile, sheetName="Variables", col.names=FALSE, row.names=FALSE, append=TRUE)



  # Make Value Level Metadata
  # Order, Dataset,	Variable,	Where Clause,	Data Type, Length, Significant Digits, Format, Mandatory, Codelist, Origin,
  # Pages, Method, Predecessor, Value Level Comment, Join Comment
  def.value.md <- var.mapping.spec[var.mapping.spec$SDTM.TESTCD != "",]
  whereclause <- stringr::str_c(def.value.md$SDTM.DOMAIN, paste0(def.value.md$SDTM.DOMAIN, "TESTCD"), def.value.md$SDTM.TESTCD, sep=".")

  val.Order <- c("Order")
  val.Dataset <- c("Dataset", def.value.md$SDTM.DOMAIN)
  val.Variable <- c("Variable", def.value.md$SDTM.VARIABLE)
  val.Where.Clause <- c("Where Clause", whereclause)
  val.Data.Type <- c("Data Type")
  val.Length <- c("Length")
  val.Significant.Digits <- c("Significant Digits")
  val.Format <- c("Format")
  val.Mandatory <- c("Mandatory")
  val.Codelist <- c("Codelist",  def.value.md$CODELIST.OID)
  val.Origin <- c("Origin")
  val.Pages <- c("Pages")
  val.Method <- c("Method")
  val.Predecessor <- c("Predecessor")
  val.Value.Level.Comment <- c("Value Level Comment")
  val.Join.Comment <- c("Join Comment")

  val.md.spec <- data.frame(val.Order,
                            val.Dataset,
                            val.Variable,
                            val.Where.Clause,
                            val.Data.Type,
                            val.Length,
                            val.Significant.Digits,
                            val.Format,
                            val.Mandatory,
                            val.Codelist,
                            val.Origin,
                            val.Pages,
                            val.Method,
                            val.Predecessor,
                            val.Value.Level.Comment,
                            val.Join.Comment,
                            stringsAsFactors = FALSE)

  for (i in 2:nrow(val.md.spec)){
    val.md.spec[i,"val.Order"] <- ""
    val.md.spec[i,"val.Data.Type"] <- "text"
    val.md.spec[i,"val.Length"] <- ""
    val.md.spec[i,"val.Significant.Digits"] <- ""
    val.md.spec[i,"val.Format"] <- ""
    val.md.spec[i,"val.Mandatory"] <- "No"
    val.md.spec[i,"val.Format"] <- ""
    val.md.spec[i,"val.Origin"] <- ""
    val.md.spec[i,"val.Pages"] <- ""
    val.md.spec[i,"val.Method"] <- ""
    val.md.spec[i,"val.Predecessor"] <- ""
    val.md.spec[i,"val.Value.Level.Comment"] <- ""
    val.md.spec[i,"val.Join.Comment"] <- ""

    #Processing for Codelist
    if ( !is.na(val.md.spec[i, "val.Codelist"])){
      val.md.spec[i,"val.Codelist"] <- stringr::str_replace(val.md.spec[i,"val.Codelist"], "CL\\.", "")
    }

  }
  write.xlsx2(val.md.spec, DefineSpecFile, sheetName="ValueLevel", col.names=FALSE, row.names=FALSE, append=TRUE)

  # Make WhereClauses Metadata
  # ID,	Dataset, Variable, Comparator, Value
  def.wc.md <- var.mapping.spec[var.mapping.spec$SDTM.TESTCD != "",]
  whereclause <- stringr::str_c(def.wc.md$SDTM.DOMAIN, paste0(def.wc.md$SDTM.DOMAIN, "TESTCD"), def.wc.md$SDTM.TESTCD, sep=".")

  wc.ID <- c("ID", whereclause)
  wc.Dataset <- c("Dataset", def.wc.md$SDTM.DOMAIN)
  wc.Variable <- c("Variable", def.wc.md$SDTM.VARIABLE)
  wc.Comparator <- c("Comparator")
  wc.Value <- c("Value", def.wc.md$SDTM.TESTCD)

  wc.md.spec <- data.frame(wc.ID,
                           wc.Dataset,
                           wc.Variable,
                           wc.Comparator,
                           wc.Value,
                           stringsAsFactors = FALSE)

  for (i in 2:nrow(wc.md.spec)){
    wc.md.spec[i,"wc.Comparator"] <- "EQ"
    wc.md.spec[i,"wc.Variable"] <- paste0(wc.md.spec[i,"wc.Dataset"], "TESTCD")
  }

  write.xlsx2(wc.md.spec, DefineSpecFile, sheetName="WhereClauses", col.names=FALSE, row.names=FALSE, append=TRUE)


  # Make Codelist Metadata
  # ID,	Name,	NCI Codelist Code, Data Type, Order, Term, NCI Term Code, Decoded Value
  cl.mapping.spec <- read.xlsx2(SDTMSpecFile, sheetName="Codelist", stringsAsFactors=FALSE)

  cl.mapping.spec <- subset(cl.mapping.spec, CODELIST.OID != "", select=c("CODELIST.OID", "NCI.CODE", "NCI.CODELIST.CODE", "CODELIST.NAME", "SDTM.TERM", "DECODED.VALUE"))
  CODELIST.ID <- sapply(stringr::str_split(cl.mapping.spec$CODELIST.OID, "\\."), as.character)[2,]

  cl.ID <- c("ID", CODELIST.ID)
  cl.Name <- c("Name", cl.mapping.spec$CODELIST.NAME)
  cl.NCI.Codelist.Code <- c("NCI Codelist Code", cl.mapping.spec$NCI.CODELIST.CODE)
  cl.Data.Type <- c("Data Type")
  cl.Order <- c("Order")
  cl.Term <- c("Term", cl.mapping.spec$SDTM.TERM)
  cl.NCI.Term.Code <- c("NCI Term Code", cl.mapping.spec$NCI.CODE)
  cl.Decoded.Value <- c("Decoded Value", cl.mapping.spec$DECODED.VALUE)

  cl.md.spec <- data.frame(cl.ID,
                           cl.Name,
                           cl.NCI.Codelist.Code,
                           cl.Data.Type,
                           cl.Order,
                           cl.Term,
                           cl.NCI.Term.Code,
                           cl.Decoded.Value,
                           stringsAsFactors = FALSE)
  for (i in 2:nrow(cl.md.spec)){
    cl.md.spec[i,"cl.Data.Type"] <- "text"
    cl.md.spec[i,"cl.Order"] <- ""
  }
  cl.md.spec <- cl.md.spec %>% dplyr::distinct(cl.ID,
                                               cl.Name,
                                               cl.NCI.Codelist.Code,
                                               cl.Data.Type,
                                               cl.Order,
                                               cl.Term,
                                               cl.NCI.Term.Code,
                                               cl.Decoded.Value)
  write.xlsx2(cl.md.spec, DefineSpecFile, sheetName="Codelists", col.names=FALSE, row.names=FALSE, append=TRUE)




  # Make Dictionary Metadata
  # ID,	Name,	Data Type,	Dictionary,	Version
  dic.ID <- c("ID")
  dic.Name <- c("Name")
  dic.Data.Type <- c("Data Type")
  dic.Dictionary <- c("Dictionary")
  dic.Version <- c("Version")

  dic.md.spec <- data.frame(dic.ID,
                            dic.Name,
                            dic.Data.Type,
                            dic.Dictionary,
                            dic.Version,
                            stringsAsFactors = FALSE)

  write.xlsx2(dic.md.spec, DefineSpecFile, sheetName="Dictionaries", col.names=FALSE, row.names=FALSE, append=TRUE)


  # Make Methods Metadata
  # ID, Name, Type, Description, Expression Context, Expression Code,	Document, Pages
  mt.ID <- c("ID")
  mt.Name <- c("Name")
  mt.Data.Type <- c("Type")
  mt.Description <- c("Description")
  mt.Expression.Context <- c("Expression Context")
  mt.Expression.Code <- c("Expression Code")
  mt.Document <- c("Document")
  mt.Pages <- c("Pages")


  mt.md.spec <- data.frame(mt.ID,
                           mt.Name,
                           mt.Data.Type,
                           mt.Description,
                           mt.Expression.Context,
                           mt.Expression.Code,
                           mt.Document,
                           mt.Pages,
                           stringsAsFactors = FALSE)

  write.xlsx2(mt.md.spec, DefineSpecFile, sheetName="Methods", col.names=FALSE, row.names=FALSE, append=TRUE)



  # Make Comment Metadata
  # ID,	Description, Document, Pages
  com.ID <- c("ID")
  com.Description <- c("Description")
  com.Document <- c("Document")
  com.Pages <- c("Pages")


  com.md.spec <- data.frame(com.ID,
                            com.Description,
                            com.Document,
                            com.Pages,
                            stringsAsFactors = FALSE)

  write.xlsx2(com.md.spec, DefineSpecFile, sheetName="Comments", col.names=FALSE, row.names=FALSE, append=TRUE)



  # Make Documents Metadata
  # ID,	Title,	Href
  doc.ID <- c("ID")
  doc.Title <- c("Title")
  doc.Href <- c("Href")

  doc.md.spec <- data.frame(  doc.ID,
                              doc.Title,
                              doc.Href,
                              stringsAsFactors = FALSE)

  write.xlsx2(doc.md.spec, DefineSpecFile, sheetName="Documents", col.names=FALSE, row.names=FALSE, append=TRUE)

}
