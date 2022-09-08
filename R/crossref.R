################################################################################
##############################Crossref integration##############################
############################https://www.crossref.org############################
############################https://api.crossref.org############################
################################################################################

require(httr)
require(jsonlite)

#User-Agent header used in all API Calls as per crossref etiquette section at https://api.crossref.org/swagger-ui/index.html
#toDo CHANGE HEADER
userAgentHeader <- "CiteDriveIntegration (mailto:nadja.stieger@stud.hn.de) based on RStudio/httr/1.4.3"

# input:  String (DOI)
# output: String or NULL
# Description: Accepts a DOI and returns a link to the corresponding paper
URLCrossRef <- function(DOI){
  APICall <- paste("https://api.crossref.org/works/", DOI, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))
  if(res$status == "200"){
    res<- fromJSON(rawToChar(res$content))
    srcURL <- res$message$resource$primary$URL
    srcName <- res$message$title
    srcType <- res$message$type

    browseURL(srcURL)
    print(paste("The ",srcType, " >",srcName , "< can be found at:", sep = ""))
    return(srcURL)
  }
  else{
    print(paste("No source for DOI",DOI,"was found"))
    print("Try using URLSemanticScholar instead!")
    return()
  }
}

# input:  String (author's name) and int (number of expected results)
# output: R Object
# Description: Fetches desired amount of works by a given author
findWorksByAuthor <- function(authorName,numberOfResults=20){
  # assures that page size is between 1 and 1000 to allow maximum amount of data
  # to be feched in one call as per API definition
  # page sizes > 1000 are fetched and appended afterwards
  if(!is.numeric(numberOfResults) || numberOfResults < 1){
    numberOfResults <- 20
    actualnumberOfResults <- 20
  }
  else if(numberOfResults <= 1000) {
    actualnumberOfResults <- numberOfResults
  }
  else {
    actualnumberOfResults <- numberOfResults
    numberOfResults <- 1000
  }

  APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author&query.author=", gsub(" ", "+", authorName),"&rows=",numberOfResults, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))

  if(res$status == "200" && fromJSON(rawToChar(res$content))$message$`total-results` > 0){
    res <- fromJSON(rawToChar(res$content))
    resTotal <- res$message$`total-results`

    # prevents fetching of data larger than the maximum entires available
    if(resTotal < numberOfResults){
      numberOfResults <- resTotal
      APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author&query.author=", gsub(" ", "+", authorName),"&rows=",numberOfResults, sep="")
      res <- GET(APICall, user_agent(userAgentHeader))
      res <- fromJSON(rawToChar(res$content))
      resTotal <- res$message$`total-results`
    }

    # fetch and append results > 1000
    if(actualnumberOfResults > 1000){
      # prevents fetching of data larger than the maximum entires available
      if(resTotal < actualnumberOfResults){
        actualnumberOfResults <- resTotal
      }
      tmpnumberOfResults <- actualnumberOfResults
      tmpnumberOfResults <- tmpnumberOfResults - 1000
      offset <- 1000
      resTemp <- res


      while(tmpnumberOfResults > 0){
        APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author&query.author=", gsub(" ", "+", authorName),"&rows=",tmpnumberOfResults,"&offset=",offset , sep="")
        res <- GET(APICall, user_agent(userAgentHeader))
        res <- fromJSON(rawToChar(res$content))

        resTemp$message$items <- rbind(resTemp$message$items, res$message$items)

        tmpnumberOfResults <- tmpnumberOfResults - 1000
        offset <- offset + 1000
      }
      res$message$items <- resTemp$message$items
      numberOfResults <- actualnumberOfResults
    }
    print(paste("Returning", numberOfResults, "out of",resTotal, "results"))
    print("To query more results provide findWorksByAuthor with the desired numberOfResults parameter")

    return(res$message$items)
  }
  else{
    print(paste("No works by", authorName, "have been found in crossref's database"))
    return()
  }
}

# input:  String (DOI)
# output: R Object or NULL
# Description: Returns references for a given DOI
referencesCrossRef <- function(DOI){
  APICall <- paste("https://api.crossref.org/works/", DOI, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))
  if(res$status == "200"){
    res <- fromJSON(rawToChar(res$content))
    return(subset(res$message$reference, select=c("DOI", "article-title", "author", "year")))
  }
  print(paste("DOI", DOI, "was found in Crossref's Database"))
  return()
}

# input:  String (DOI)
# output: R Object or NULL
# Description: Returns Name ISSN of publishing journal
workPublishedIn <- function(DOI){
  APICall <- paste("https://api.crossref.org/works/", DOI, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))
  if(res$status == "200"){
    tmp <- fromJSON(rawToChar(res$content))$message
    paperTitle <- tmp$title

    res <- data.frame(ISSN=character(),
                      Title=character(),
                      Publisher=character(),
                      stringsAsFactors=FALSE)

    for (x in 1:length(tmp$ISSN)) {
      dta <- c(tmp$ISSN[x],
              JournalInfo(tmp$ISSN[x])$title,
              JournalInfo(tmp$ISSN[x])$publisher)
      res[nrow(res) + 1, ] <- dta
    }

    print(paste("Printing Journal Infomation for", paperTitle, sep = " "))
    return(res)
  }
  print(paste("DOI", DOI, "was found in Crossref's Database"))
  return()
}

# input:  String (Journal Subjects) and int (number of expected results)
# output: R Object
# Description: Fetches desired amount of Journals by a given subject
findJournalByKeywords <- function(Keywords, numberOfResults=20){
  # assures that page size is between 1 and 1000 to allow maximum amount of data
  # to be feched in one call as per API definition
  # page sizes > 1000 are fetched and appended afterwards
  if(!is.numeric(numberOfResults) || numberOfResults < 1){
    numberOfResults <- 20
    actualnumberOfResults <- 20
  }
  else if(numberOfResults <= 1000) {
    actualnumberOfResults <- numberOfResults
  }
  else {
    actualnumberOfResults <- numberOfResults
    numberOfResults <- 1000
  }

  APICall <- paste("https://api.crossref.org/journals?query=", gsub(" ", "+", Keywords),"&rows=",numberOfResults, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))

  if(res$status == "200" && fromJSON(rawToChar(res$content))$message$`total-results` > 0){
    res <- fromJSON(rawToChar(res$content))
    resTotal <- res$message$`total-results`

    # prevents fetching of data larger than the maximum entires available
    if(resTotal < numberOfResults){
      numberOfResults <- resTotal
      APICall <- paste("https://api.crossref.org/journals?query=", gsub(" ", "+", Keywords),"&rows=",numberOfResults, sep="")
      res <- GET(APICall, user_agent(userAgentHeader))
      res <- fromJSON(rawToChar(res$content))
      resTotal <- res$message$`total-results`
    }

    # fetch and append results > 1000
    if(actualnumberOfResults > 1000){
      # prevents fetching of data larger than the maximum entires available
      if(resTotal < actualnumberOfResults){
        actualnumberOfResults <- resTotal
      }
      tmpnumberOfResults <- actualnumberOfResults
      tmpnumberOfResults <- tmpnumberOfResults - 1000
      offset <- 1000
      resTemp <- res


      while(tmpnumberOfResults > 0){
        #APICall <- paste("https://api.crossref.org/works?select=DOI,type,title,is-referenced-by-count,references-count,author,published&query.author=", gsub(" ", "+", authorName),"&rows=",tmpnumberOfResults,"&offset=",offset , sep="")
        APICall <- paste("https://api.crossref.org/journals?query=", gsub(" ", "+", Keywords), "&rows=",tmpnumberOfResults, "&offset=", offset, sep="")
        res <- GET(APICall, user_agent(userAgentHeader))
        res <- fromJSON(rawToChar(res$content))

        resTemp$message$items <- rbind(resTemp$message$items, res$message$items)

        tmpnumberOfResults <- tmpnumberOfResults - 1000
        offset <- offset + 1000
      }
      res$message$items <- resTemp$message$items
      numberOfResults <- actualnumberOfResults
    }
    print(paste("Returning", numberOfResults, "out of",resTotal, "results"))
    print("To query more results provide findJournalByKeywords with the desired numberOfResults parameter")

    return(res$message$items)
  }
  else{
    print(paste("No Journals for >", Keywords, "< have been found in crossref's database", sep = ""))
    return()
  }
}

# input:  String (Journal ISSN)
# output: R Object
# Description: Fetches inforamtion about Journal by ISSN
JournalInfo <- function(ISSN){
  APICall <- paste("https://api.crossref.org/journals/", ISSN, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))
  if(res$status == "200"){
    res <- fromJSON(rawToChar(res$content))$message
    print(paste("Printing Journal information for",res$title))
    return(res)
  }
  else{
    print(paste("No journal with ISSN:",ISSN,"was found in crossref's database"))
    return()
  }
}

# input:  String (Journal ISSN)
# output: R Object
# Description: Fetches inforamtion about Journal by ISSN
JournalWorks <- function(ISSN, numberOfResults=20){
  # assures that page size is between 1 and 1000 to allow maximum amount of data
  # to be feched in one call as per API definition
  # page sizes > 1000 are fetched and appended afterwards
  if(!is.numeric(numberOfResults) || numberOfResults < 1){
    numberOfResults <- 20
    actualnumberOfResults <- 20
  }
  else if(numberOfResults <= 1000) {
    actualnumberOfResults <- numberOfResults
  }
  else {
    actualnumberOfResults <- numberOfResults
    numberOfResults <- 1000
  }

  APICall <- paste("https://api.crossref.org/journals/", ISSN,"/works?", "rows=", numberOfResults, sep="")
  res <- GET(APICall, user_agent(userAgentHeader))
  print(APICall)

  if(res$status == "200" && fromJSON(rawToChar(res$content))$message$`total-results` > 0){
    res <- fromJSON(rawToChar(res$content))
    resTotal <- res$message$`total-results`

    # prevents fetching of data larger than the maximum entires available
    if(resTotal < numberOfResults){
      numberOfResults <- resTotal
      APICall <- paste("https://api.crossref.org/journals/", ISSN,"/works", "?rows=",numberOfResults, sep="")
      res <- GET(APICall, user_agent(userAgentHeader))
      res <- fromJSON(rawToChar(res$content))
      resTotal <- res$message$`total-results`
    }

    # fetch and append results > 1000
    if(actualnumberOfResults > 1000){
      # prevents fetching of data larger than the maximum entires available
      if(resTotal < actualnumberOfResults){
        actualnumberOfResults <- resTotal
      }
      tmpnumberOfResults <- actualnumberOfResults
      tmpnumberOfResults <- tmpnumberOfResults - 1000
      offset <- 1000
      resTemp <- res


      while(tmpnumberOfResults > 0){
        APICall <- paste("https://api.crossref.org/journals/", ISSN,"/works", "?rows=",tmpnumberOfResults, "&offset=", offset, sep="")
        res <- GET(APICall, user_agent(userAgentHeader))
        res <- fromJSON(rawToChar(res$content))

        resTemp$message$items <- rbind(resTemp$message$items, res$message$items)

        tmpnumberOfResults <- tmpnumberOfResults - 1000
        offset <- offset + 1000
      }
      res$message$items <- resTemp$message$items
      numberOfResults <- actualnumberOfResults
    }
    print(paste("Returning", numberOfResults, "out of",resTotal, "results"))
    print("To query more results provide JournalWorks with the desired numberOfResults parameter")
    return(res$message$items)
  }
  else{
    print(paste("No Journals for", ISSN, "have been found in crossref's database"))
    return()
  }
}
