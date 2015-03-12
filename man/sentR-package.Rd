\name{sentR-package}
\alias{sentR-package}
\alias{sentR}
\docType{package}
\title{
Provides varied techniques for big data sentiment analysis in R
}
\description{
Provides a more holistic understanding of sentiment analysis with numerous implemented methodologies in R
}
\details{
\tabular{ll}{
Package: \tab sentR\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2015-03-11\cr
License: \tab MIT \cr
}
Version 0.1: implementation of score.aggregate, a basic aggregation sentiment scoring function with regex driven substitution of key elements in Twitter and Amazon corpuses.
}
\author{
Manan Shah
}

\keyword{ package, sentiment, analysis }
\examples{
pos <- scan(posTextFile, what='character', comment.char=';')
neg <- scan(negTextFile, what='character', comment.char=';')
text <- "I am happy!"
scores = score.aggregate(text, pos, neg, .progress="text")
}
