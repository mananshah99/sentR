# sentR
An R package that provide functional sentiment analysis utilities.

## Install

1. Open the R shell (32 or 64 bit)
2. Install devtools: run `install.packages('devtools')`
2. Require devtools: run `require('devtools')`
3. Install sentR@latest: run `install_github('mananshah99/sentR')`

# Sentiment Scoring Examples

Input:

```
require('sentR')

# Create small vectors for happy and sad words (useful in aggregate(...) function)
positive <- c('happy', 'well-off', 'good', 'happiness')
negative <- c('sad', 'bad', 'miserable', 'terrible')

# Words to test sentiment
test <- c('I am a very happy person.', 'I am a very sad person', 
'I’ve always understood happiness to be appreciation. There is no greater happiness than appreciation for what one has- both physically and in the way of relationships and ideologies. The unhappy seek that which they do not have and can not fully appreciate the things around them. I don’t expect much from life. I don’t need a high paying job, a big house or fancy cars. I simply wish to be able to live my life appreciating everything around me. 
')

# 1. Simple Summation
out <- classify.aggregate(test, positive, negative)
out

# 2. Naive Bayes
out <- classify.naivebayes(test)
out
```

Output:
```
  score
1     1
2    -1
3     2

     POS                NEG                 POS/NEG             SENT      
[1,] "9.47547003995745" "0.445453222112551" "21.2715265477714"  "positive"
[2,] "1.03127774142571" "9.47547003995745"  "0.108836578774127" "negative"
[3,] "67.1985217685598" "35.1792261323723"  "1.9101762362738"   "positive"
```