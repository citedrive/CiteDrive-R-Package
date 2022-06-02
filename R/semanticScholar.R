# sources metadata from semantic scholar's API endpoint
# toDo: Decide whether to use citationKey or DOI to fetch data
# potential idea: use regex provided here:
# http://web.archive.org/web/20180321212859/https://www.crossref.org/blog/dois-and-matching-regular-expressions/
# to check if DOI or citationKey was used

#dummy DOI 10.2337/dc12-1805

require(httr)
require(jsonlite)

#utility functions#
#getDOI <- function(citationKey){

#}
################################################################################

##############################data sourced by paper#############################
#returns string or NULL
abstract <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,abstract", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
    print(paste("Abstract for ", res$title, ":", sep = ""))
    return(res$abstract)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns R Object or NULL
authors <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,authors", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
    print(paste(res$title, "was written by:"))
    return(res$authors)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns integer or NULL
citationCount <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,citationCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
  print(paste("Citation count for ", res$title, ":", sep = ""))
  return(res$citationCount)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns R Object or NULL
fieldsOfStudy <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,fieldsOfStudy", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
  print(paste("Fields of study for ", res$title, ":", sep = ""))
  return(res$fieldsOfStudy)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns R Object of 10 entries or NULL
findPapers <- function(keywords){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/search?query==", gsub(" ", "+", keywords),"&fields=externalIds,title,year,fieldsOfStudy,isOpenAccess", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(res$total[1] != 0){
    subset(res$data$externalIds <- subset(res$data$externalIds, select=c(DOI)))
    return(res$data)
  }
  else{
    print(paste("Error: No results found for keywords", keywords))
    return()
  }
}

#returns integer or NULL
influentialCitationCount <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,influentialCitationCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
  print(paste("Influential citationCount count for ", res$title, ":", sep = ""))
  return(res$influentialCitationCount)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns BOOL or NULL
isOpenAccess <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=isOpenAccess", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
  return(res$isOpenAccess)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#toDo:
#add CrossRef function name
#returns string and opens website or NULL
URLSemanticScholar <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,url", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$url)){
    browseURL(res$url)
    print(paste("Sematic Scholar URL for ", res$title, ":", sep = ""))
    return(res$url)
  }
  else{
    print(paste("Error: No URL for", DOIorPaperID, "could be found in Semantic Scholar's Database!"))
    print(paste("If you are using DOI try URLCrossRef(", DOIorPaperID, ")", sep = ""))
    return()
  }
}

#returns string and opens website or NULL
references <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title", sep="")
  paper <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(paper$paperId)){
    APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"/references?fields=externalIds,title,year", sep="")
    res <- fromJSON(rawToChar(GET(APICall)$content))
    subset(res$data$citedPaper$externalIds <- subset(res$data$citedPaper$externalIds, select=c(DOI)))
    print(paste("References for paper ", paper$title, ":", sep = ""))
    return(res$data)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns integer or NULL
referenceCount <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,referenceCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
  print(paste("Reference count for ", res$title, ":", sep = ""))
  return(res$referenceCount)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns string or NULL
tldr <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,tldr", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
  print(paste("Short summary of ", res$title, ":", sep = ""))
  return(res$tldr)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}
################################################################################


#############################data sourced by author#############################
#returns R Object or NULL
authorInfo <- function(authorID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/?fields=name,hIndex,aliases", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers", sep="")
  res$paperCount <-  nrow(fromJSON(rawToChar(GET(APICall)$content))$data)
  if(!is.null(res$authorId)){
  return(res)
  }
  else{
    print(paste("Error:", authorID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns R Object or NULL
#toDo: possibly iterate through aliases and either provide ID or run findAuthor for IDs too
findAuthorID <- function(authorName){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/search?query=", gsub(" ", "+", authorName), sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(res$total[1] != 0){
  return(res$data)
  }
  else{
    print(paste("Error:", authorName, "not found in Semantic Scholar's Database!"))
    return()
  }
}

#returns R Object or NULL
papersByAuthor <- function(authorID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/?fields=name,hIndex,aliases", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$authorId)){
    APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers/?fields=year,title,venue,isOpenAccess,externalIds", sep="")
    res <- fromJSON(rawToChar(GET(APICall)$content))
    #toDo pick best method for print out
    print(res$data)
    print("\nor\n")
    tryCatch(
      res$data$externalIds <- subset(res$data$externalIds, select=c(DOI)),
      error = function(e){
        print("No DOI in Semantic Scholar's Database! Printing other external IDs instead")
      }
    )
    return(subset(res$data, select=c(externalIds, title, venue, year, isOpenAccess)))
  }
  else{
    print(paste("Error:", authorID, "not found in Semantic Scholar's Database!"))
    return()
  }
}
################################################################################
