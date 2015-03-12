process.stemwords <- function(document) {
  if(!require("Rstem")){
    install.packages("Rstem")
    library("Rstem")
  }
  words <- str_trim(unlist(strsplit(document, " ")))
  words <- words[words!=""]
  # stem words
  stemmed_words <- wordStem(words)
  # collapse back into one blob
  output <- paste(stemmed_words, collapse=" ")
  return(output)
}

process.striplinks <- function(x)
{
  .internal<-function(x)
  {
    y=unlist(strsplit(x,split='[\ \n\t]'))
    
    if( length(y) >= 1) {
      return(paste( y[grepl('http',y) == 0], collapse=" " ) )
    } 
    else {
      if( grep('http',x)==0 ) return(x) 
      else return(NA)
    }
    
  }
  
  if(is.vector(x)) {
    return( unlist(lapply(x,FUN=.internal))) 
  }
  else 
    .internal(x)
}