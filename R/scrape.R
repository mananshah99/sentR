scrape.IMDb <- function(url)
{
  require(XML)
  doc <- htmlParse(url)
  tables <- readHTMLTable(doc)
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(nrows)]]
  
  return(df)
}

scrape.retrieveklout <- function(handles, api.key, na_omit=TRUE, .progress='none') {
  twitter_handles <- gsub("@", "", handles)
  getID <- function(twitter_handle) {
    base <- 'http://api.klout.com/v2/identity.json/twitter?screenName='
    url <- paste0(base, twitter_handle, "&key=", api.key)
    out <- try(fromJSON(getURL(url))$id, TRUE)
    if(class(out)=='try-error'){
      id <- NA
    } else {
      id <- str_trim(out)
    }
    return(id)
  }
  ids <- laply(twitter_handles, getID, .progress=.progress)

  exists <- !is.na(ids)
  twitter_handles_TRUE <- twitter_handles[exists]
  
  if(length(twitter_handles_TRUE) < length(twitter_handles)) {
    
    warning(paste("No Klout Scores for:",
                  paste(twitter_handles[is.na(ids)], collapse=" "))
    )
    
    # partition output by error status
    exists <- !is.na(ids)
    ids <- ids[exists]
    twitter_handles_TRUE <- twitter_handles[exists]
    twitter_handles_FALSE <- twitter_handles[!exists]
  }
  
  getScore <- function(id) {
    url <- paste0('http://api.klout.com/v2/user.json/', id,'/score', '?key=', api.key)
    out <- try(fromJSON(getURL(url))$score, TRUE)
    if(class(out)=='try-error'){
      score <- NA
    } else {
      score <- str_trim(out)
    }
    return(score)
  }
  score <- laply(ids, getScore, .progress=.progress)
  
  # prepare output, leave NAs
  output_TRUE <- data.frame(handle = twitter_handles_TRUE, id=ids, score=score, stringsAsFactors=F)
  if(na_omit){
    output <- output_TRUE
  } else {
    output_FALSE <- data.frame(handle = twitter_handles_FALSE, stringsAsFactors=F)
    output_FALSE$id <- NA
    output_FALSE$score <- NA
    output <- data.frame(rbind(output_TRUE, output_FALSE), stringsAsFactors=F)
  }
  
  output$score <- as.numeric(str_trim(output$score))
  return(output)
}