
#' Ngram Predictor
#'
#' \code{ngram_predictor} predicts the next word in a character string using an
#' input phrase and source content with which to construct an ngram model.
#'
#' @param input_phrase A character string containing the text to be used to predict
#' the next word.
#'
#' @param source A character vector with one element for each line of a text.
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
#' ngram, count and frequency.  Otherwise, will return ngram, count, frequency, cumulative count
#' and cumulative frequency.  Default is TRUE.
#'
#' @return A data frame containing the ngrams matching the input phrase provided,
#' along with the count and frequency of each ngram.
#'
#' @importFrom dplyr arrange desc filter
#'
#' @importFrom magrittr "%>%"
#'
#' @importFrom stringr str_starts
#'
#' @examples
#'
#' data(faulkner)
#'
#' input_phrase <- "it is"
#'
#' next_word_table <- ngram_predictor(input_phrase,
#'                                    faulkner,
#'                                    n = 3,
#'                                    dec_pos = 3,
#'                                    min_count = 1,
#'                                    simple = TRUE)
#'
#' @export

ngram_predictor <- function(input_phrase,
                            source,
                            n = 3,
                            dec_pos = 5,
                            min_count = 1,
                            simple = TRUE) {

    tryCatch({

        # Check function parameters.

        if(!is.numeric(n)) stop("ngram_predictor: n must contain an integer value.")

        if(!is.numeric(dec_pos)) stop("ngram_predictor: dec_pos must contain an integer value.")

        if(!is.numeric(min_count)) stop("ngram_predictor: min_count must contain an integer value.")

        if(min_count < 1) stop("ngram_predictor: min_count must be a value of one or greater.")

        # Coerce n, dec_pos and min_count parameters to integer value.

        n <- as.integer(n)

        dec_pos <- as.integer(dec_pos)

        min_count <- as.integer(min_count)

        # Create corpus.

        cp <- create_corpus(source)

        # Create ngrams.

        ngrams <- create_ngrams(cp, n, dec_pos, min_count, simple)

        # Filter and arrange next word table.

        next_word_table <- ngrams %>%
            dplyr::filter(stringr::str_starts(ngram, input_phrase)) %>%
            dplyr::arrange(dplyr::desc(count))

    }, warning = function(w) {

        warning(paste("ngram_predictor: ", w, sep = ""))

        return(NULL)

    }, error = function(e) {

        stop(paste("ngram_predictor: ", e, sep = ""))

        return(NULL)

    }, finally = {

    })

    return(next_word_table)

}
