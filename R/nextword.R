
#' Next Word (simple)
#'
#' \code{next_word_simple} ...
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

next_word_simple <- function(target_ngram,
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

        warning(paste("next_word_simple: ", w, sep = ""))

        return(NULL)

    }, error = function(e) {

        stop(paste("next_word_simple: ", e, sep = ""))

        return(NULL)

    }, finally = {

    })

    return(next_word_table)

}
