require ("stringr")

# input:  API String to synchronise CiteDrive's .bib file
# output: R Object
# Description: Downloads and extracts all DOIs from .bib File
getDOIsFromBib <- function(citeDriveString){
  # dummy string
  citeDriveString <- "https://api.citedrive.com/bib/065aff98-83ac-4d4d-b175-4b4f58ca5bba/references.bib?x=eyJpZCI6ICIwNjVhZmY5OC04M2FjLTRkNGQtYjE3NS00YjRmNThjYTViYmEiLCAidXNlciI6ICI5NzgiLCAic2lnbmF0dXJlIjogImQ1YzBlZWZjYjY2YjBiODBlMGE5YTUzYTg2ZTBhMWFmMzgzM2ViNDU3Yjc5YTdkNjNmOTMxNjc5YjIzNjkxZjgifQ==.bib"
  bibFile <- readLines(citeDriveString)
  DOIs <- bibFile[grepl("doi = ", bibFile)]
  DOIs <- as.data.frame(str_extract(DOIs, "(?<=doi = \\{).*(?=\\})"))
  return(DOIs)
}
