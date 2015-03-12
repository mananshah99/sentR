scrape.IMDb <- function(url)
{
  require(XML)
  doc <- htmlParse(url)
  tables <- readHTMLTable(doc)
  nrows <- unlist(lapply(tables, function(t) dim(t)[1]))
  df <- tables[[which.max(nrows)]]
  
  return(df)
}