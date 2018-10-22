#load libraries

library(xml2)
library("plyr")
library(stringr)
library("editData")

#load faculty file
load("")

#API key
wskey <- "&wskey="

#function to get books by author and year

getNewBooks <- function(last,first, year,wskey){
  pubs <- data.frame(creator = character(),
                     title = character(),
                     year = integer(),
                     oclcnum = character(),
                     true = logical(),
                     stringsAsFactors = FALSE
  )
  name <-str_replace(paste0("srw.au+%3D+%22",last,"%22+and+srw.au+%3D+%22",first,"%22"),"\\s+","%20")
  pubyear <- paste0("+and+srw.yr+%3D+%22",year,"+%22")
  #requests only bks document type
  #constructs a URL that looks like this: 
 reqURLsru <- paste0("http://www.worldcat.org/webservices/catalog/search/sru?query=srw.dt+%3D+%22bks%22+and+",name,pubyear,"&recordSchema=info%3Asrw%2Fschema%2F1%2Fdc",wskey)
  #    print(reqURLsru)
  #converts to a list for east extraction
  resultsList <- as_list(read_xml(reqURLsru))
  #checks the numbers of results
  numResults <- resultsList$searchRetrieveResponse$numberOfRecords[[1]]
  #if there are no results, the process ends
  if (numResults=="0") {
    return(NULL)
  }
  #when there are more than one results, loops through them
  for (i in resultsList$searchRetrieveResponse$records) {
    wholerecord <- unlist(i$recordData$oclcdcs)
    #finds fields that have both names in the same field
    matched <- grepl(last, wholerecord) & grepl(first, wholerecord)
    # if any meet that criteria, calculates the distance between the names
    #within the field
    if (sum(matched) > 0) {
      
      #find the location of the first and last names in the record
      loc_first <- as.data.frame(str_locate(wholerecord[matched], first))
      loc_last <- as.data.frame(str_locate(wholerecord[matched], last))
      
      # find the distance between first and last names
      if (loc_first$start[1] < loc_last$start[1]) {
        dist <- abs(loc_first$end[1] - loc_last$start[1])
      } else {
        dist <- abs(loc_last$end[1] - loc_first$start[1])
      }
      
      #if the names are within 10 characters, adds a record to the list
      if (dist < 10) {
        
        title <- i$recordData$oclcdcs$title[[1]]
        #        print(title)
        recordIdentifier <- i$recordData$oclcdcs[which(names(i$recordData$oclcdcs)=="recordIdentifier")]
        oclcnum <- recordIdentifier[which(!lapply(recordIdentifier, function(x) attr(x, "type"))=="http://purl.org/oclc/terms/lccn")][[1]][[1]]
        if (is.null(i$recordData$oclcdcs$creator[[1]])){
          creator <- ""
        }
        else {
          creator <- i$recordData$oclcdcs$creator[[1]][1]
        }
        pub <- data.frame(creator, title, year, oclcnum, stringsAsFactors = FALSE)
        pubs <- rbind(pub,pubs, stringsAsFactors = FALSE)
      }
      
    }
  }
  
  
  Sys.sleep(.2)
  return(data.frame(pubs))
}


getNewBooksBySheet <- function(data,year,wskey){
  retrieval <- mdply(.data=data, .fun=getNewBooks,
                     year = year, wskey = wskey)
  return(retrieval)
}

retrieval <- getNewBooksBySheet(faculty[100:110,2:3],2017,wskey)

#check whether the book is a real result



if (!exists("bookslist")) {
  bookslist <- retrieval
} else {
  # find books that haven't been retrieved yet, and add to running bookslist
  newRetrieved <- retrieval[which(!retrieval$oclcnum %in% bookslist$oclcnum),]
  if(dim(newRetrieved)[1]>0){
    newRetrieved <- editData(newRetrieved)
    bookslist <- rbind(bookslist,newRetrieved, stringsAsFactors = FALSE)
    row.names(bookslist) <- 1:dim(bookslist)[1]
    write.csv(bookslist[which(bookslist$true==TRUE),], file="newBooksbyBates.csv", row.names = FALSE)
  }
  else{
    print("No new books found.")
  }
}


#this will print out a string you can put in WorldCat Expert Search
makeWorldCatSearch <- function(oclcnum){
  first <- paste0("no:",oclcnum[1])
  therest <- paste0(" or no:",oclcnum[2:length(oclcnum)])
  cat(c(first,therest))
  
}

makeWorldCatSearch(bookslist$oclcnum[which(bookslist$true==TRUE)])
