% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ngram.R
\name{get_sample}
\alias{get_sample}
\title{Get Sample}
\usage{
get_sample(source, seed_num = 91210, sample_size = 0.1)
}
\arguments{
\item{source}{A character vector with one element for each line of a text.}

\item{seed_num}{A single integer value used to set a seed for random
number generation.  \code{seed_num} is coerced to an integer value.}

\item{sample_size}{A floating point number used to specify the size of the sample
as a probability; should be valued between zero and one.  A value of zero returns
an empty character vector.}
}
\value{
A character vector containing the randomly selected lines from the
source vector.
}
\description{
\code{get_sample} selects a random sample of lines from a character vector.
}
\examples{

data(faulkner)

sample >- get_sample(source = faulkner,
                     seed_num = 091210,
                     sample_size = 0.10)

}
