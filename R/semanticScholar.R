################################################################################
##########################SEMANTIC SCHOLAR integration##########################
########################https://www.semanticscholar.org#########################
########################https://api.semanticscholar.org#########################
################################################################################

library(httr)
library(jsonlite)
library(plyr)

##############################data sourced by paper#############################
# input:  String (DOI or paperID)
# output: String or NULL
# Description: Finds and returns abstract of a given paper
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

# input:  String (DOI or paperID)
# output: R Object or NULL
# Description: Returns list of authors for a given paper
authors <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,authors&limit=1000", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
    return(res$authors)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

# input:  String (DOI or paperID)
# output: int or NULL
# Description: Returns the number of citations of a given paper
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

# input:  String (DOI or paperID)
# output: String or NULL
# Description: Returns the field of study of a given paper
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
# input:  String (keywords)
# output: R Object or NULL
# Description: Find papers matching a keyword
findPapers <- function(keywords, numberOfResults=20){
  if(!is.numeric(numberOfResults) || numberOfResults < 1){
    numberOfResults <- 20
    actualNumberOfResults <- 20
  }
  else if(numberOfResults <= 100) {
    actualNumberOfResults <- numberOfResults
  }
  else {
    actualNumberOfResults <- numberOfResults
    numberOfResults <- 100
  }

  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/search?query==", gsub(" ", "+", keywords),"&fields=externalIds,title,year,fieldsOfStudy,isOpenAccess&limit=",numberOfResults, sep="")
  # number of total results available for keyword
  res <- fromJSON(rawToChar(GET(APICall)$content))
  totalResults <- res$total[1]

  if(totalResults != 0){
    displayedResults = res$`next`

    #prevent endless fetch-loop if total amount of data < requested size of dataset
    if(displayedResults < totalResults){
      #prevent overfetching by dividing query
      while(displayedResults < actualNumberOfResults){
        if((actualNumberOfResults - displayedResults) > 100){
          numberOfResults <- 100
        }
        else{
          numberOfResults <- actualNumberOfResults - displayedResults
        }

        APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/search?query==", gsub(" ", "+", keywords),"&fields=externalIds,title,year,fieldsOfStudy,isOpenAccess&limit=",numberOfResults,"&offset=",displayedResults, sep="")
        resTemp <- fromJSON(rawToChar(GET(APICall)$content))

        displayedResults <- resTemp$`next`
        row.names(resTemp$data$externalIds) <- NULL
        res$data <- rbind(res$data, resTemp$data)
      }
    }
    print(paste(displayedResults, " of ", totalResults, " papers with the keywords >", keywords, "<:"), sep="")
    return(res$data)
  }
  else{
    print(paste("Error: No results found for keywords", keywords))
    return()
  }
}

# input:  String (DOI or paperID)
# output: int or NULL
# Description: Returns the influential citation count for a given paper
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

# input:  String (DOI or paperID)
# output: boolean or NULL
# Description: Returns true if a paper is open access, false if it isn't
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

# input:  String (DOI or paperID)
# output: String or NULL
# Description: Returns a link to a paper corresponding to a DOI or paperID and opens it
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
    print(paste("If you are using DOI try URLCrossRef(", DOIorPaperID, ") instead", sep = ""))
    return()
  }
}

# input:  String (DOI)
# output: R Object or NULL
# Description: Returns references for a given DOI
referencesSemanticScholar <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title", sep="")
  paper <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(paper$paperId)){
    APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"/references?fields=externalIds,title,year&limit=1000", sep="")
    res <- fromJSON(rawToChar(GET(APICall)$content))
    print(res)
    print(paste("References for paper ", paper$title, ":", sep = ""))
    return(res$data)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

# input:  String (DOI or paperID)
# output: int or NULL
# Description: Returns the amount of references of the paper
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

# input:  String (DOI or paperID)
# output: String or NULL
# Description: Returns a short summary of the paper
tldr <- function(DOIorPaperID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/paper/",DOIorPaperID,"?fields=title,tldr", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$paperId)){
    return(res$tldr$text)
  }
  else{
    print(paste("Error:", DOIorPaperID, "not found in Semantic Scholar's Database!"))
    return()
  }
}
################################################################################


#############################data sourced by author#############################
# input:  int (authorID)
# output: R Object or NULL
# Description: Returns name,hIndex and aliases of a given author
authorInfo <- function(authorID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/?fields=name,hIndex,aliases", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers?limit=1000", sep="")
  res$paperCount <-  nrow(fromJSON(rawToChar(GET(APICall)$content))$data)
  if(!is.null(res$authorId)){
  return(res)
  }
  else{
    print(paste("Error:", authorID, "not found in Semantic Scholar's Database!"))
    return()
  }
}

# input:  String (author name)
# output: R Object or NULL
# Description: Returns author id matching a provided name
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

# input:  int (authorID)
# output: R Object or NULL
# Description: Returns papers credited to a given author
papersByAuthor <- function(authorID){
  APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/?fields=name,hIndex,aliases", sep="")
  res <- fromJSON(rawToChar(GET(APICall)$content))
  if(!is.null(res$authorId)){
    APICall <- paste("https://api.semanticscholar.org/graph/v1/author/", authorID, "/papers/?fields=year,title,venue,isOpenAccess,externalIds&limit=1000", sep="")
    res <- fromJSON(rawToChar(GET(APICall)$content))
    print(res$data)
    return(subset(res$data, select=c(externalIds, title, venue, year, isOpenAccess)))
  }
  else{
    print(paste("Error:", authorID, "not found in Semantic Scholar's Database!"))
    return()
  }
}
################################################################################
################################################################################

###############################Evaluation author################################
# input:  R Object
# output: R Object
# Description:  Returns list of authors for each entry in a list of DOIs
getAuthorList <- function(DOIsAsDataFrame){
  authorList <- c()
  tmp <- c()
  for (i in 1:nrow(DOIsAsDataFrame)) {
    authorList <- rbind(authors(DOIsAsDataFrame[i, ]), authorList)
  }

  authorList <- authorList[!duplicated(authorList), ]
  return(authorList)
}

# input:  R Object
# output: R Object
# Description:  Returns h-Index of authors from a list of DOIs
getHirschIndexList <- function(DOIsAsDataFrame){
  authorList <- getAuthorList(DOIsAsDataFrame)

  authorList['hIndex'] <- c(0)
  for (i in 1:nrow(authorList)) {
    authorList[i, ]$hIndex <- authorInfo(authorList[i, ]$authorId)$hIndex
  }
  authorList <- subset(authorList, select = -authorId)
  return(authorList)
}

# input:  R Object
# output: R Object
# Description:  Returns paper count for each item in a list of DOIS
getPaperCountList <- function(DOIsAsDataFrame){
  authorList <- getAuthorList(DOIsAsDataFrame)

  authorList['paperCount'] <- c(0)
  for (i in 1:nrow(authorList)) {
    authorList[i, ]$paperCount <- authorInfo(authorList[i, ]$authorId)$paperCount
  }
  authorList <- subset(authorList, select = -authorId)
  return(authorList)
}
################################################################################


###############################Evaluation papers################################
# input:  R Object
# output: R Object or NULL
# Description:  Returns citation count count for each item in a list of DOIS
getCitationCountList <- function(DOIsAsDataFrame){
  DOIList <- DOIsAsDataFrame

  CitationCountList <- c()

  for (i in 1:nrow(DOIList)) {
    CitationCountList <- rbind(citationCount(DOIList[i, ]), CitationCountList)
  }

  CitationCountList <- CitationCountList[!duplicated(CitationCountList), ]
  return(CitationCountList)
}

# input:  R Object
# output: R Object or NULL
# Description:  Returns influential citation count count
#               for each item in a list of DOIS
getInfluentialCitationCountList <- function(DOIsAsDataFrame){

  DOIList <- DOIsAsDataFrame

  CitationCountList <- c()

  for (i in 1:nrow(DOIList)) {
    CitationCountList <- rbind(influentialCitationCount(DOIList[i, ]), CitationCountList)
  }

  CitationCountList <- CitationCountList[!duplicated(CitationCountList), ]
  return(CitationCountList)
}
