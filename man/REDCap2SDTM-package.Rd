\name{REDCap2SDTM-package}
\alias{REDCap2SDTM-package}
\alias{REDCap2SDTM}
\docType{package}
\title{
\packageTitle{REDCap2SDTM}
}
\description{
\packageDescription{REDCap2SDTM}
}
\author{
\packageAuthor{REDCap2SDTM}

Maintainer: \packageMaintainer{REDCap2SDTM}
}
\references{

}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R ~~
~~ documentation directory ~~
\keyword{ package }
\examples{
library(REDCap2SDTM)
options(stringsAsFactors=FALSE)

# Import REDCap Metada
# Example Data Dictionary file is stored in REDCapFiles
# directory under this package.
fp <- system.file("REDCapFiles",
                  "A4.REDCap2CDISC_DataDictionary.csv",
                  package = "REDCap2SDTM")
RC.MD <- read.REDCap.CSV(fp, type="METADATA")

# Generate a mapping spec as xlsx file to current working directory
genMWB(RC.MD, paste0(getwd(),"/SDTM-SPEC.xlsx"))

# Import REDCap Data
# Example Data file is stored in REDCapFiles
# directory under this package.
data.file <- system.file("REDCapFiles",
                         "A5.REDCap2CDISC_DATA.csv",
                         package="REDCap2SDTM")
RC.DATA <- read.REDCap.CSV(data.file, type="DATA")

# A6.Mapping-spec.xlsx is modified from SDTM-SPEC.xlsx that is generated "genMWB" function.
mapping.spec <- system.file("REDCapFiles",
                            "A6.Mapping-spec.xlsx",
                            package="REDCap2SDTM")

# Generate SDTM dataset as R dataframe.
genDataset(mapping.spec, REDCapData=RC.DATA)

# Generate Define.xml spec for Pinnacle 21 Community
# which is a define.xml generator.
genDefine.Spec(mapping.spec, paste0(getwd(),"/A7.Define-spec.xlsx"))

}
