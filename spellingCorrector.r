# parallel library parallel and foreach
library(parallel)

# corpus file path
words.file = file.path("Data","words.txt")

# get the word frequent table
getwords <- function(words.file) {
  # read file
  con <- file(words.file, open="rt")
  words <- readLines(con)
  close(con)
  # construct dataframe for words
  # can change to parallel run
  # connect to one string
  result <- tolower(paste(words, collapse=" "))
  # delete the chars without a-z, blankspace
  str <- gsub("[^a-z\ ]","",result)
  words <- strsplit(str, ' +')
  return(table(words))
}

# nearly words of inputing word
# change one char
getnearlywords <- function(word) {
  #cObj <- makeCluster(detectCores())
  result <- c()
  # can change to parallel run
  for (j in 0:(nchar(word))) {
     a <- substr(word, 0, j)
     b <- substr(word, j+1, nchar(word))
     if (nchar(b)>0) {
       # delete a char
       w <- paste(c(a,substr(b,2,nchar(word))),collapse="")
       result <- c(result, w)       
       # replace a char
       result <- c(result,do.call(rbind,lapply(letters,
                                               FUN=function(alp) {
                                                 w <- paste(c(a, alp, substr(b,2,nchar(word))),collapse="")
                                                 return(w)
                                               })))
     }
     # transpose    
     if (nchar(b)>1) {
       w<- paste(c(a,substr(b,2,2),substr(b,1,1), substr(b,3,nchar(b))),collapse="")
       result <- c(result,w)
     }
     # insert
     result <- c(result, do.call(rbind, lapply(letters,
                                               FUN=function(alp) {
                                                 w <- paste(c(a,alp,b),collapse="")
                                                 return(w)
                                               })))
   }                      
  return(unique(result))
}

correct <- function(input) {
  if (is.na(words.table[input])) {
    #change one char
    words.1char <- getnearlywords(input)
    length(words.1char)
    #change two chars
    # parallel run
    cObj <- makeCluster(detectCores())
    words.2char <- c(do.call(rbind,parLapply(cObj,words.1char,fun=getnearlywords))[,])
    stopCluster(cObj)
    # sequencial run
    #words.2char <- c(do.call(rbind,lapply(words.1char,FUN=getnearlywords))[,])
    #total chars
    words.total <- unique(c(words.1char,words.2char))
    length(words.total)
    # main time consume.
    #system.time(words.table[words.total])
    #system.time(which(!is.na(words.table[words.total])))
    #system.time(which.max(words.table[words.total]))
    #system.time(words.valid[which.max(words.table[words.valid])])
    words.valid <- words.total[words.total %in% names(words.table)]
    words.valid
    return(words.valid[which.max(words.table[words.valid])])
  }
  else {
    return(input)
  }
}
  
# word table, value is frequency, collum is words
words.table <- getwords(words.file)
# get the right word
input <- "corect"
#system.time(correct(input))
correct(input)

