#simple aggregation function with regex driven gsub
classify.aggregate = function(sentences, pos.words, neg.words, .progress='none')  
{ 
  require(plyr)  
  require(stringr)
  
  # array ("a") of scores back =  "l" + "a" + "ply" = "laply":  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub("[[:digit:]]", "", sentence)
    sentence = gsub('\\d+', '', sentence)   
    sentence = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sentence)
    sentence = gsub("@\\w+", "", sentence)
    sentence = gsub("http\\w+", "", sentence)
    sentence = gsub("[ \t]{2,}", "", sentence)
    sentence = gsub("^\\s+|\\s+$", "", sentence)
    
    # define "tolower error handling" function 
    try.error = function(x)
    {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      
      # if not an error
      if (!inherits(try_error, "error"))
        y = tolower(x)
      
      return(y)
    }
    
    sentence = sapply(sentence, try.error)
    sentence = sentence[!is.na(sentence)]
    names(sentence) = NULL
    
    word.list = str_split(sentence, '\\s+')  
    words = unlist(word.list)  
    
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    
    # match() returns the position of the matched term or NA (we only want a boolean value)
    #  which is fixed by sum
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
  }, pos.words, neg.words, .progress=.progress)
  
  # returns a data frame
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
}

# Uses ViralHeat's API to calculate the sentiment of a given piece of text.
# Note that maximum number of characters is 360
classify.viralheat = function (text, api.key) {
  library(RCurl);
  library(RJSONIO);
  
  text <- URLencode(text);
  text <- str_replace_all(text, "%20", " ");
  text <- str_replace_all(text, "%\\d\\d", "");
  text <- str_replace_all(text, " ", "%20");
  
  if (str_length(text) > 360){
    text <- substr(text, 0, 359);
  }
  
  data <- getURL(paste("http://www.viralheat.com/api/sentiment/review.json?text=",text,"&api_key=", api.key, sep=""))
  
  js <- fromJSON(data, asText=TRUE);
  
  sent <- js$prob;
  
  j <<- js;
  
  if (js$mood == "negative") {
    sent <- sent * -1;
  }
  else {
    if (js$mood != "positive") {
      #must be neutral
      sent <- 0;
    }
  }
  
  return(sent);
}

classify.naivebayes <- function(textColumns, pstrong=0.5,
                              pweak=1.0, prior=1.0, ...) {
  
  matrix <- classify.dtm(textColumns, ...)
  fpath <- system.file("extdata", "subjectivity.csv", package="sentR")
  lexicon <- read.csv(fpath, head = FALSE)
  
  counts <- list(positive = length(which(lexicon[,3]=="positive")), 
                 negative = length(which(lexicon[,3]=="negative")),
                 total = nrow(lexicon))
  
  documents <- c()
  
  for (i in 1:nrow(matrix)) {
    scores <- list(positive=0, negative=0)
    doc <- matrix[i,]
    words <- findFreqTerms(doc, lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word,lexicon[,1],nomatch=0)
      if (index > 0) {
        entry <- lexicon[index,]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        score <- abs(log(score*prior/count))
        scores[[category]] <- scores[[category]]+score
      }		
    }
    
    for (key in names(scores)) {
      count <- counts[[key]]
      total <- counts[["total"]]
      score <- abs(log(count/total))
      scores[[key]] <- scores[[key]]+score
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    
    ratio <- abs(scores$positive/scores$negative)
    
    if (ratio > 0.90 && ratio < 1.10)
      best_fit <- "neutral"
    
    documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
  }
  
  colnames(documents) <- c("POS","NEG","POS/NEG","SENT")
  return(documents)
}

classify.dtm <- function(textColumns, language="english", 
                          minDocFreq = 1, minWordLength = 4, 
                          removeNumbers = TRUE, removePunctuation = TRUE, 
                          removeStopwords = TRUE, 
                          stemWords = FALSE, stripWhitespace = TRUE, 
                          toLower = TRUE, weighting = weightTf) {

  require('tm')
  control <- list(language = language, tolower = toLower,
                  removeNumbers = removeNumbers, removePunctuation = removePunctuation,
                  stripWhitespace = stripWhitespace, minWordLength = minWordLength,
                  stopwords = removeStopwords, minDocFreq = minDocFreq, 
                  weighting = weighting)
  
  if (stemWords == TRUE)
    control <- append(control, list(stemming = process.stemwords), after=6)
  
  train <- apply(as.matrix(textColumns), 1, paste, collapse=" ")
  train <- sapply(as.vector(train, mode="character"),
                           iconv, to="UTF8", sub="byte")
  
  corpus <- Corpus(VectorSource(train), readerControl=list(language=language))
  matrix <- DocumentTermMatrix(corpus,control=control)
  gc() # garbage collect
  
  return(matrix)
}
