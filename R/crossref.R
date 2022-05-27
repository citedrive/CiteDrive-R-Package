# sources metadata from crossref's API endpoint
# toDo: Decide whether to use citationKey or DOI to fetch data
# potential idea: use regex provided here:
# http://web.archive.org/web/20180321212859/https://www.crossref.org/blog/dois-and-matching-regular-expressions/
# to check if DOI or citationKey was used

#dummy DOI 10.2337/dc12-1805

require(httr)
require(jsonlite)

#User-Agent header used in all API Calls as per crossref etiquette section at https://api.crossref.org/swagger-ui/index.html
#toDo CHANGE HEADER
userAgentHeader <- "Testlib (mailto:nadja.stieger@stud.hn.de) based on RStudio/httr/1.4.3"

#returns string and opens website or NULL
URLCrossRef <- function(DOI){
  APICall <- paste("https://api.crossref.org/works/", DOI, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))
  if(res$status == "200"){
    res<- fromJSON(rawToChar(res$content))
    srcURL <- res$message$resource$primary$URL
    srcName <- sapply(strsplit(srcURL, split = "/"), tail, 1)
    srcType <- res$message$type

    browseURL(srcURL)
    print(paste("The ",srcType, " >",srcName , "< can be found at:", sep = ""))
    return(srcURL)
  }
  else{
    print(paste("No source for DOI",DOI,"was found"))
    return()
  }
}

#only prints 20 papers at a time
#toDo: fix output and iterate through papers list
findWorksByAuthor <- function(authorName,listSize=20){
  if(!is.numeric(listSize) || listSize < 0){
    listSize = 20
    pageSize = 20
  }
  else if(listSize > 1000){
    pageSize = 1000
  }
  APICall <- paste("https://api.crossref.org/works?select=DOI,title,is-referenced-by-count&query.author=", gsub(" ", "+", authorName),"&rows=",pageSize, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))

  if(res$status == "200" && fromJSON(rawToChar(res$content))$message$`total-results` > 0){
    res <- fromJSON(rawToChar(res$content))
    print("sack")
    print(res)
    if(listSize>1000){
      resTemp <- res
      tmpListSize <- listSize
      listSize <- listSize - 1000
      offset <- 1000
      while(length(res$message$items) && listSize > 0){
        if(listSize < 1000 && listSize > 0){
          pageSize <- listSize
        }
        APICall <- paste("https://api.crossref.org/works?select=DOI,title,is-referenced-by-count&query.author=", gsub(" ", "+", authorName),"&rows=",pageSize,"&offset=",offset , sep="")
        res <- GET(APICall, user_agent(userAgentHeader))
        res <- fromJSON(rawToChar(res$content))
        resTemp <- rbind(resTemp$message$items, res$message$items)
        #toDo Wrong append, needs to append data not header

        listSize <- listSize - 1000
        offset <- offset + 1000
        print("ficker")
        print(res)
      }
      res <- resTemp
      listSize <- tmpListSize
    }
    print(paste("Returning", listSize, "out of",res$message$`total-results`, "results"))
    print("To query more results provide findWorksByAuthor with listSize parameter")
    return(res)
  }
  else{
    print(paste("No works by", authorName, "have been found in crossref's database"))
    return()
  }
}

referencedBy <- function(DOI){
  refs <- URLCrossRef(DOI)
  refsSubset <- subset(refs$message$reference, select=c("first-page", "DOI", "article-title"))
  print(refsSubset)
}


