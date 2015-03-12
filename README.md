# sentR
A continually developed R package to provide functional sentiment analysis utility. 

## Install

1. Open the R shell (32 or 64 bit)
2. Install devtools: run `install.packages('devtools')`
2. Require devtools: run `require('devtools')`
3. Install sentR@latest: run `install_github('mananshah99/sentR')`

## Sentiment Scoring Functions

### score.aggregate = function(sentences, pos.words, neg.words, .progress='none')
Scores the sentiment of text by setting words in `pos.words` to `+1` and words in `neg.words` to `-1` in the vector of sentences. 

Example:
```
require('sentR')
positive <- c('happy', 'well-off', 'good')
negative <- c('sad', 'bad', 'miserable', 'terrible')
test <- c('I am a very happy person.', 'I am a very sad person', 'R has good data analysis tools')
out <- score.aggregate(test, positive, negative)
out
```
Output:
```
  score                           text
1     1      I am a very happy person.
2    -1         I am a very sad person
3     1 R has good data analysis tools
```

### score.naivebayes = function(train.sentences, test.sentences, .progress = 'none') 
Scores the sentiment of text by using the Naive Bayes modeling functionality from `e1071`. 

### score.viralheat = function (text, api.key)
Scores the sentiment of text using the ViralHeat API (requires an API key)

## Sentiment Scraping Utilities

### scrape.IMDb <- function(url)
Scrapes the IMDb website for movie reviews and returns a list of scores.

### scrape.striplinks <- function(text)
Strips all links of the form `http*` found in text. Useful for normalizing sentimetn scores

## Plotting Utilities

### multiplot <- function(..., plotlist=NULL, cols)
Allows for multiple `ggplot` instances to be printed together. 
