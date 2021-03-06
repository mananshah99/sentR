% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/classify.R
\name{classify.naivebayes}
\alias{classify.naivebayes}
\title{Naive Bayes Sentiment Classification}
\usage{
classify.naivebayes(sentences, pstrong = 0.5, pweak = 1, prior = 1, ...)
}
\arguments{
\item{sentences}{the sentences to classify}

\item{pstrong}{the probability that a strong subjective term appears in the text}

\item{pweak}{the probability that a weakly subjective term appears in the text}

\item{prior}{the prior probability to use (1.0 by default)}
}
\description{
Implements the Naive-Bayes prior probability classification function for
determining the sentiment of text. Multiple customization parameters may defined
for the created document term matrix in order to ensure maximum accuracy.
}
\examples{
classify.naivebayes(c("I am happy", "I am sad"))
}

