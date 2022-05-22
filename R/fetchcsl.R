# toDo: fetch citation style from github and place into appropriate file to style rMarkdown project
require("curl")

getCsl <- function(cslURL="https://github.com/citation-style-language/styles/blob/master/academy-of-management-review.csl"){

  con <- suppressWarnings(try(url(cslURL), silent=TRUE))
  urlError <- suppressWarnings(try(open.connection(con, open="r", timeout=2), silent=TRUE))
  suppressWarnings(try(close.connection(con), silent = TRUE))

  if(is.null(urlError)){
    cslURL <- sub(".*github.com", "https://raw.githubusercontent.com", cslURL)
    cslURL <- sub("/blob", "", cslURL)

    cslName <- sapply(strsplit(cslURL, split = "/"), tail, 1)

    if(grepl("raw.githubusercontent.com/citation-style-language/styles", cslURL, fixed=TRUE) && grepl(".csl", cslName, fixed=TRUE)){
      dir.create(file.path(".", "//CSL/"), showWarnings = FALSE)
      curl_download(cslURL, paste(".//CSL/", cslName))
    }

    return(cslName)
  }
  else{
   print("CSL URL is Invalid\n")
  }
  return("")
}
