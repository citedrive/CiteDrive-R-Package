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

findWorksByAuthor <- function(authorName,pageSize=20){
  # assures that page size is between 1 and 1000 to allow maximum amount of data to be feched in one call as per API definition
  # page sizes > 1000 are fetched and appended afterwards
  if(!is.numeric(pageSize) || pageSize < 1){
    pageSize <- 20
    actualPageSize <- 20
  }
  else if(pageSize <= 1000) {
    actualPageSize <- pageSize
  }
  else {
    actualPageSize <- pageSize
    pageSize <- 1000
  }
  #APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author,published&query.author=", gsub(" ", "+", authorName),"&rows=",pageSize, sep="")
  APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author&query.author=", gsub(" ", "+", authorName),"&rows=",pageSize, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))

  if(res$status == "200" && fromJSON(rawToChar(res$content))$message$`total-results` > 0){
    res <- fromJSON(rawToChar(res$content))
    resTotal <- res$message$`total-results`

    # prevents fetching of data larger than the maximum entires available
    if(resTotal < pageSize){
      pageSize <- resTotal
      APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author&query.author=", gsub(" ", "+", authorName),"&rows=",pageSize, sep="")
      res <- GET(APICall, user_agent(userAgentHeader))
      res <- fromJSON(rawToChar(res$content))
      resTotal <- res$message$`total-results`
    }

    # fetch and append results > 1000
    if(actualPageSize > 1000){
      # prevents fetching of data larger than the maximum entires available
      if(resTotal < actualPageSize){
        actualPageSize <- resTotal
      }
      tmpPageSize <- actualPageSize
      tmpPageSize <- tmpPageSize - 1000
      offset <- 1000
      resTemp <- res


      while(tmpPageSize > 0){
        #APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author,published&query.author=", gsub(" ", "+", authorName),"&rows=",tmpPageSize,"&offset=",offset , sep="")
        APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author&query.author=", gsub(" ", "+", authorName),"&rows=",tmpPageSize,"&offset=",offset , sep="")
        res <- GET(APICall, user_agent(userAgentHeader))
        res <- fromJSON(rawToChar(res$content))

        resTemp$message$items <- rbind(resTemp$message$items, res$message$items)

        tmpPageSize <- tmpPageSize - 1000
        offset <- offset + 1000
      }
      res$message$items <- resTemp$message$items
      pageSize <- actualPageSize
    }
    print(paste("Returning", pageSize, "out of",resTotal, "results"))
    print("To query more results provide findWorksByAuthor with the desired pageSize parameter")

    return(res$message$items)
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


