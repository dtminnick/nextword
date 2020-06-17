
#' Create Corpus
#'
#' \code{create_corpus} creates a corpus object from a character vector.
#'
#' @param content A character vector with one element for each line of a text.
#'
#' @return A quanteda tokens class object containing the document and corpus data
#' for subsequent processing.  \code{create_corpus} returns a corpus object consisting
#' of a single document compiled from the input vector.
#'
#' @importFrom quanteda corpus ndoc texts
#'
#' @examples
#' content <- "Dream delivers us to dream, and there is no end to illusion."
#' cp <- create_corpus(content)
#'
#' @export

create_corpus <- function(content) {

      tryCatch({

            # Create corpus.

            cp <- quanteda::corpus(content)

            # Group into single document.

            cp <- quanteda::corpus(quanteda::texts(cp, groups = rep(1, quanteda::ndoc(cp))))

      }, warning = function(w) {

            warning(paste("create_corpus: ", w, sep = ""))

            return(NULL)

      }, error = function(e) {

            stop(paste("create_corpus: ", e, sep = ""))

            return(NULL)

      }, finally = {

      })

      return(cp)

}

#' Create Ngrams
#'
#' \code{create_ngrams} creates a set of ngrams from a corpus object, tokenizing the set
#'     of words in the corpus, removing punctuation, symbols, numbers, URLs and separators.
#'     \code{create_ngrams} splits hyphenated words.
#'
#' @param tokens A quanteda corpus class object.
#'
#' @param n An integer specifying the number of elements to be concatenated in each ngram.
#'
#' @return A data frame containing the ngrams along with the count, frequency,
#'     cumulative count and cumulative frequency of each ngram.  Ngrams are created
#'     with an underscore concatenator when combining words.
#'
#' @importFrom dplyr arrange desc group_by mutate n summarise
#'
#' @importFrom magrittr "%>%"
#'
#' @importFrom quanteda tokens tokens_ngrams
#'
#' @examples
#' content <- "Dream delivers us to dream, and there is no end to illusion."
#' content <- create_tokens(content)
#' unigrams <- create_ngrams(content, n = 1)
#'
#' @export

create_ngrams <- function(corpus, n = 1) {

      tryCatch({

            # Tokenize corpus.

            tokens <- quanteda::tokens(corpus,
                                       what = 'word',
                                       remove_numbers = TRUE,
                                       remove_punct = TRUE,
                                       remove_symbols = TRUE,
                                       remove_separators = TRUE,
                                       split_hyphens = TRUE,
                                       verbose = TRUE)

            # Create ngrams.

            ngrams <- (quanteda::tokens_ngrams(tokens, n))

            # Unlist ngrams and coerce to a data frame.

            ngrams <- as.data.frame(unlist(ngrams))

            # Rename first column of data frame.

            colnames(ngrams) <- c("ngram")

            # Use data frame to build frequency table.

            ngrams <- ngrams %>%
                  dplyr::group_by(ngram) %>%
                  dplyr::summarise(count = dplyr::n()) %>%
                  dplyr::arrange(dplyr::desc(count)) %>%
                  dplyr::mutate(frequency = round(count / sum(count), 5),
                                cumulative_count = cumsum(count),
                                cumulative_frequency = round(cumsum(count) / sum(count), 5))

      }, warning = function(w) {

            warning(paste("create_ngrams: ", w, sep = ""))

            return(NULL)

      }, error = function(e) {

            stop(paste("create_ngrams: ", e, sep = ""))

            return(NULL)

      }, finally = {

      })

      return(ngrams)

}
