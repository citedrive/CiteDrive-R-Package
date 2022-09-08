require ("stringr")

# input:  API String to synchronise CiteDrive's .bib file
# output: R Object
# Description: Downloads and extracts all DOIs from .bib File
getDOIsFromBib <- function(citeDriveString){
  bibFile <- readLines(citeDriveString)
  DOIs <- bibFile[grepl("doi = ", bibFile)]
  DOIs <- as.data.frame(str_extract(DOIs, "(?<=doi = \\{).*(?=\\})"))
  return(DOIs)
}
