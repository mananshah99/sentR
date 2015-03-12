#simple aggregation function with regex driven gsub
score.aggregate = function(sentences, pos.words, neg.words, .progress='none')  
{ 
  require(plyr)  
  require(stringr)       
  
  # add smileys
  good.smiley <- c(":)")
  bad.smiley <- c(":(",";(",":'(") 
  
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

#all sentences have to be annotated
score.naivebayes = function(train.sentences, test.sentences, .progress = 'none') {
  library(RTextTools)
  library(e1071)
  matrix= create_matrix(train.sentences[,1], language="english", 
                        removeStopwords=TRUE, removeNumbers=TRUE,
                        stemWords=TRUE) 
  
  # train the model
  mat = as.matrix(matrix)
  classifier = naiveBayes(mat[1:length(train.sentences)/2,], as.factor(train.sentences[1:length(train.sentences)/2,2]))
  
  matrix2 = create_matrix(test.sentences[,1], language="english", 
                        removeStopwords=TRUE, removeNumbers=TRUE,
                        stemWords=TRUE)
  
  mat2 = as.matrix(matrix2)
  predicted = predict(classifier, mat2[0:5,]); 
  return(predicted)
}


# Uses ViralHeat's API to calculate the sentiment of a given piece of text.
# Note that maximum number of characters is 360
score.viralheat = function (text, api.key){
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
  else{
    if (js$mood != "positive") {
      #must be neutral
      sent <- 0;
    }
  }
  
  return(sent);
}
