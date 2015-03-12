scrape.IMDb <- function(url)
{
  require(XML)
  doc <- htmlParse(url)
  tables <- readHTMLTable(doc)
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(nrows)]]
  
  return(df)
}

scrape.striplinks <-function(x)
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