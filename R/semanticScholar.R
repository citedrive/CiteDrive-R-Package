# fetches meta data through DOI from semantic scholar using api calls
# toDo: Replace dummy bibtex file with actual bibtex

require(httr)
require(jsonlite)

#utility functions#
#getDOI <- function(citationKey){

#}
###################

citationCount <- function(citationKey){
  #DOI<-getDOI(citationKey)
  DOI <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOI,"?fields=title,citationCount", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste(res$title, " was cited ", res$citationCount, " times", sep = ""))
}

authors <- function(citationKey){
  #DOI<-getDOI(citationKey)
  DOI <- "10.7759/cureus.r53"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOI,"?fields=title,authors", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(res)
  print(paste(res$title, " was written by:", sep = ""))
  print(res$authors)
}

abstract <- function(citationKey){
  #DOI<-getDOI(citationKey)
  DOI <- "10.2337/dc12-1805"
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOI,"?fields=title,abstract", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(paste("Abstract for ", res$title, ":", sep = ""))
  print(res$abstract)
}

findAuthor <- function(authorName){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/search?query=", gsub(" ", "+", authorName), sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(res$data)
}

papersByAuthor <- function(authorID){
  #authorID <- getAuthorID(citationKey)
  #APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "?fields=paperId,title", sep="")
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers/?fields=year,title,venue,isOpenAccess,externalIds", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  #toDo pick best method for print out
  print(res$data)
  print("\nor\n")
  subset(res$data$externalIds <- subset(res$data$externalIds, select=c(DOI)))
  print(subset(res$data, select=c(externalIds, title, venue, year, isOpenAccess)))
}

authorInfo <- function(authorID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/?fields=name,hIndex,aliases,papers", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  print(res)
}

#https://api.semanticscholar.org/graph/v1/paper/10.2337/dc12-1805?fields=title,citations.authors
#"https://api.semanticscholar.org/graph/v1/paper/10.1109/tcyb.2017.2751646?fields=citationCount"

