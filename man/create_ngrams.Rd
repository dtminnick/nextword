% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_ngrams.R
\name{create_ngrams}
\alias{create_ngrams}
\title{Create Ngrams}
\usage{
create_ngrams(corpus, n = 1)
}
\arguments{
\item{n}{An integer specifying the number of elements to be concatenated in each ngram.}

\item{tokens}{A quanteda tokens class object.}
}
\value{
A data frame containing the ngrams along with the count, frequency,
    cumulative count and cumulative frequency of each ngram.
}
\description{
\code{create_ngrams} generates...
}
\examples{
content <- "Dream delivers us to dream, and there is no end to illusion."
content <- create_tokens(content)
unigrams <- create_ngrams(content, n = 1)

}
