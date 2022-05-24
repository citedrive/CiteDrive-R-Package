# fetches meta data through DOI from semantic scholar using api calls
# toDo: Decide whether to use citationKey or DOI to fetch data
# potential idea: use regex provided here:
# http://web.archive.org/web/20180321212859/https://www.crossref.org/blog/dois-and-matching-regular-expressions/
# to check if DOI or citationKey was used

#toDo: remove dummy DOI by paper functions

require(httr)
require(jsonlite)

#utility functions#
#getDOI <- function(citationKey){

#}
###################

#data sourced by paper#
abstract <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,abstract", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Abstract for ", res$title, ":", sep = ""))
  return(res$abstract)
}

#may be truncated
authors <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.7759/cureus.r53"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,authors", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(res)
  print(paste(res$title, "was written by:"))
  return(res$authors)
}

citationCount <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,citationCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Citation count for ", res$title, ":", sep = ""))
  return(res$citationCount)
}

fieldsOfStudy <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,fieldsOfStudy", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Fields of study for ", res$title, ":", sep = ""))
  return(res$fieldsOfStudy)
}

influentialCitationCount <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,influentialCitationCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Influential citationCount count for ", res$title, ":", sep = ""))
  return(res$influentialCitationCount)
}

isOpenAccess <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=isOpenAccess", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  return(res$isOpenAccess)
}

paperURL <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.1021/jacs.9b13484"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,url", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  browseURL(res$URL)
  print(paste("Sematic Scholar URL for ", res$title, ":", sep = ""))
  return(res$tldr)
}

references <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.1021/jacs.9b13484"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"/references", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("References for paper ", res$title, ":", sep = ""))
  return(res)
}

referenceCount <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,referenceCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Reference count for ", res$title, ":", sep = ""))
  return(res$referenceCount)
}

tldr <- function(DOIorPaperID){
  #DOI<-getDOI(citationKey)
  DOIorPaperID <- "10.1021/jacs.9b13484"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,tldr", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Short summary of ", res$title, ":", sep = ""))
  return(res$tldr)
}
########################


#data sourced by author#
authorInfo <- function(authorID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/?fields=name,hIndex,aliases", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers", sep="")
  res$paperCount <-  nrow(fromJSON(rawToChar(GET(APICall)$content))$data)
  return(res)
}

findAuthor <- function(authorName){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/search?query=", gsub(" ", "+", authorName), sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  return(res$data)
}

papersByAuthor <- function(authorID){
  #authorID <- getAuthorID(citationKey)
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers/?fields=year,title,venue,isOpenAccess,externalIds", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  #toDo pick best method for print out
  print(res$data)
  print("\nor\n")
  subset(res$data$externalIds <- subset(res$data$externalIds, select=c(DOI)))
  return(subset(res$data, select=c(externalIds, title, venue, year, isOpenAccess)))
}
########################
