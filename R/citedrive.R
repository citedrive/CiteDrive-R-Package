getBib <- function(name, url) {
    fileName <- paste(name,".bib",sep="")
    z <- readLines(url)
    fileConn<-file(fileName)
    writeLines(z, fileConn)
    close(fileConn)
}

