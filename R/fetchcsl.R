# toDo: fetch citation style from github and place into appropriate file to style rMarkdown project
require("curl")

getCsl <- function(url){
  #urlValid <- suppressWarnings(try(open.connection(url, open="r", timeout=2), silent=T))
  #suppressWarnings(try(close.connection(url), silent=T))

#  if(1){
    url <- sub(".*github.com", "https://raw.githubusercontent.com", url)
    url <- sub("/blob", "", url)

    cslName <- sapply(strsplit(url, split = "/"), tail, 1)

    print(cslName)
    if(grepl("raw.githubusercontent.com/citation-style-language/styles", url, fixed=TRUE) && grepl(".csl", url, fixed=TRUE)){
      curl_download(url, paste(".//CSL/", cslName, ".xml"))
    }
#  }
#  else{
#   print("CSL Url is Invalid\n")
#  }
}
