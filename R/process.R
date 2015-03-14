#' Word Stemming Utility
#'
#' Provides a word stemming utility function for use in the naive
#' bayes classifier.
#' 
#' @param document a vector of sentences to stem
#' @export
#' 
process.stemwords <- function(document) {
  require("SnowballC")
  require("stringr")
  words <- str_trim(unlist(strsplit(document, " ")))
  words <- words[words!=""]
  # stem words
  stemmed_words <- wordStem(words)
  # collapse back into one blob
  output <- paste(stemmed_words, collapse=" ")
  return(output)
}

#' Stip Text Links Utility
#' 
#' Strips 'http*' links from text in order for cleaner analysis
#'
#' @param x the text to process
#' @export
#' @examples
#' 
#' process.striplinks("http://google.com is a search engine brought to you by Google")
#' 
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