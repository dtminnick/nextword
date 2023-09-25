
#' Ngram Predictor
#'
#' \code{ngram_predictor} ...
#'
#' @param source ..
#'
#' @param target_ngram ...
#'
#' @param source ...
#'
#' @param n ...
#'
#' @param dec_pos ...
#'
#' @param min_count ...
#'
#' @param simple ...
#'
#' @return ...
#'
#' @examples
#'
#' @export

ngram_predictor <- function(target_ngram,
                            source,
                            n = 3,
                            dec_pos = 5,
                            min_count = 1,
                            simple = TRUE) {

    tryCatch({

        cp <- create_corpus(source)

        ngrams <- create_ngrams(cp, n, dec_pos, min_count, simple)

        next_word_table <- ngrams %>%
            filter(stringr::str_starts(ngram, target_ngram)) %>%
            arrange(desc(count))

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
