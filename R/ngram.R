
#' Get Sample
#'
#' \code{get_sample} selects a random sample of lines from a character vector.
#'
#' @param source A character vector with one element for each line of a text.
#'
#' @param seed_num A single integer value used to set a seed for random
#' number generation.  \code{seed_num} is coerced to an integer value.
#'
#' @sample_size A floating point number used to specify the size of the sample
#' as a probability; should be valued between zero and one.  A value of zero returns
#' an empty character vector.
#'
#' @return A character vector containing the randomly selected lines from the
#' source vector.
#'
#' @examples
#'
#' data(faulkner)
#'
#' sample >- get_sample(source = faulkner,
#'                      seed_num = 9796,
#'                      sample_size = 0.10)
#'
#' @export

get_sample <- function(source, seed_num = 091210, sample_size = 0.10) {

    tryCatch({

        # Check function parameters.

        if(!is.character(source)) stop("get_sample: source must be a character vector.")

        if(!is.numeric(seed_num)) stop("get_sample: seed_num must contain an integer value.")

        if(!is.numeric(sample_size)) stop("get_sample: sample_size must contain a numeric value.")

        if(sample_size <= 0) stop("get_sample: sample_size must be value between zero and one.")

        if(sample_size > 1) stop("get_sample: sample_size must be value between zero and one.")

        # Coerce seed_num parameter to an integer value.

        seed_num <- as.integer(seed_num)

        # Set seed.

        set.seed(seed_num)

        # Generate random sample.

        source <- source[rbinom(n = length(source),
                                size = 1,
                                prob = sample_size) == 1]

    }, warning = function(w) {

        warning(paste("get_sample: ", w, sep = ""))

        return(NULL)

    }, error = function(e) {

        stop(paste("get_sample: ", e, sep = ""))

        return(NULL)

    }, finally = {

    })

    return(source)

}

#' Create Corpus
#'
#' \code{create_corpus} creates a corpus object from a character vector.
#'
#' @param content A character vector with one element for each line of a text.
#'
#' @return A quanteda tokens class object containing the document and corpus data
#' for subsequent processing.  \code{create_corpus} returns a corpus object consisting
#' of documents compiled from the input vector, with one document per line of
#' text.
#'
#' @importFrom quanteda corpus corpus_group ndoc
#'
#' @examples
#'
#' data(faulkner)
#'
#' cp <- create_corpus(faulkner)
#'
#' @export

create_corpus <- function(source) {

    tryCatch({

        # Create corpus.

        cp <- quanteda::corpus(source)

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
#' of words in the corpus, removing punctuation, symbols, numbers, URLs and separators.
#' \code{create_ngrams} splits hyphenated words.
#'
#' @param tokens A quanteda corpus class object.
#'
#' @param n An integer specifying the number of elements to be concatenated in each ngram.
#'
#' @param dec_pos An integer specifying the number of positions following the
#' decimal to return for frequency and cumulative frequency.  Value of '5' returns
#' numbers with five positions following the decimal.
#'
#' @param min_count An integer specifying the minimum count of ngrams to return, i.e.
#' min_count = 1 will return all ngrams with a count of 1 or more.
#'
#' @param simple Boolean; if TRUE, then function will return a simple table including
#' ngram and count.  Otherwise, will return ngram, count, frequency, cumulative count
#' and cumulative frequency.  Default is TRUE.
#'
#' @return A data frame containing the ngrams along with the count, frequency,
#' cumulative count and cumulative frequency of each ngram.  Ngrams are created
#' with an underscore concatenator when combining words.
#'
#' @importFrom dplyr arrange desc filter group_by mutate n summarise
#'
#' @importFrom magrittr "%>%"
#'
#' @importFrom quanteda is.corpus tokens tokens_ngrams
#'
#' @importFrom stringr str_to_lower str_squish
#'
#' @examples
#'
#' data(faulker)
#'
#' cp <- create_corpus(faulkner)
#'
#' ug <- create_ngrams(corpus = cp,
#'                     n = 1,
#'                     dec_pos = 5,
#'                     min_count = 2)
#'
#' @export

create_ngrams <- function(corpus, n = 1, dec_pos = 5, min_count = 1, simple = TRUE) {

    tryCatch({

        # Check function parameters.

        if(!quanteda::is.corpus(corpus)) stop("create_ngrams: corpus must be a quanteda corpus class object.")

        if(!is.numeric(n)) stop("create_ngrams: n must contain an integer value.")

        if(!is.numeric(dec_pos)) stop("create_ngrams: dec_pos must contain an integer value.")

        if(!is.numeric(min_count)) stop("create_ngrams: min_count must contain an integer value.")

        if(min_count < 1) stop("create_ngrams: min_count must be a value of one or greater.")

        # Coerce n, dec_pos and min_count parameters to integer value.

        n <- as.integer(n)

        dec_pos <- as.integer(dec_pos)

        min_count <- as.integer(min_count)

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

        ngrams <- (quanteda::tokens_ngrams(tokens,
                                           n,
                                           concatenator = " "))

        # Unlist ngrams and coerce to a data frame.

        ngrams <- as.data.frame(unlist(ngrams))

        # Rename first column of data frame.

        colnames(ngrams) <- c("ngram")

        # Use data frame to build frequency table.

        if(simple == TRUE) {

                ngrams <- ngrams %>%
                    dplyr::mutate(ngram = stringr::str_squish(ngram),
                                  ngram = stringr::str_to_lower(ngram)) %>%
                    dplyr::group_by(ngram) %>%
                    dplyr::summarise(count = dplyr::n()) %>%
                    dplyr::arrange(dplyr::desc(count)) %>%
                    dplyr::filter(count >= min_count) %>%
                    dplyr::mutate(frequency = round(count / sum(count), dec_pos))

        } else {

                ngrams <- ngrams %>%
                  dplyr::mutate(ngram = stringr::str_squish(ngram),
                                ngram = stringr::str_to_lower(ngram)) %>%
                    dplyr::group_by(ngram) %>%
                    dplyr::summarise(count = dplyr::n()) %>%
                    dplyr::arrange(dplyr::desc(count)) %>%
                    dplyr::mutate(frequency = round(count / sum(count), dec_pos),
                                  cumulative_count = cumsum(count),
                                  cumulative_frequency = round(cumsum(count) / sum(count), dec_pos)) %>%
                    dplyr::filter(count >= min_count)

        }

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
