read.REDCap.CSV <- function(file, type) {
  if (.Platform$OS.type == "windows") {
    CHR_ENCD <- "CP932"
  }
  else if(.Platform$OS.type == "unix") {
    CHR_ENCD <- "CP932"  #"utf-8"
  }

  varNames <- c("field_name",
                "form_name",
                "section_header",
                "field_type",
                "field_label",
                "select_choices_or_calculations",
                "field_note",
                "text_validation_type_or_show_slider_number",
                "text_validation_min",
                "text_validation_max",
                "identifier",
                "branching_logic",
                "required_field",
                "custom_alignment",
                "question_number",
                "matrix_group_name",
                "matrix_ranking",
                "field_annotation")

  if (type %in% c("DATA", "METADATA")){
    fpath <- file
    if (type=="METADATA"){
      rc.metadata <- read.csv2(fpath, header=TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE, fileEncoding = CHR_ENCD)
      #rc.metadata <- read.csv2(fpath, header=TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
      names(rc.metadata) <- varNames
      return(rc.metadata)
    }

    else if (type=="DATA") {
      rc.data <- read.csv2(fpath, header=TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE, fileEncoding = CHR_ENCD)
      #rc.data <- read.csv2(fpath, header=TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
      return(rc.data)
    }
  } else {
    stop("type argument is incorrect")
  }
}
