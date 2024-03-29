
read_source <- function(file_path) {

        tryCatch({

                source <- readr::read_lines(file_path)

        }, warning = function(w) {

                warning(paste("read_source: ", w, sep = ""))

                return(NULL)

        }, error = function(e) {

                stop(paste("read_source: ", e, sep = ""))

                return(NULL)

        }, finally = {

        })

        return(source)

}

#' Get Sample
#'
#' \code{get_sample} selects a random sample of lines from a character vector.
#'
#' @param source A character vector with one element for each line of a text.
#'
#' @param seed_num A single integer value used to set a seed for random
#' number generation.  \code{seed_num} is coerced to an integer value.
#'
#' @param sample_size A floating point number used to specify the size of the sample
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
#'                      seed_num = 091210,
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
#' ngram, count and frequency.  Otherwise, will return ngram, count, frequency, cumulative count
#' and cumulative frequency.  Default is TRUE.
#'
#' @return A data frame containing the ngrams along with the count, frequency,
#' of each ngram.  Ngrams are created with a space concatenator when combining words.
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
#' @param top_results An integer specifying the number of rows to return in the data
#' frame of matching ngrams, e.g. value of '10' returns the top ten rows.
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
                            top_results = 10,
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
            dplyr::arrange(dplyr::desc(count), ngram) %>%
            dplyr::top_n(top_results)

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

get_predicted_word <- function(tbl) {

        tryCatch({

                predicted_word <- stringr::word(as.character(tbl[1,]$ngram), -1)

        }, warning = function(w) {

                warning(paste("get_predicted_word: ", w, sep = ""))

                return(NULL)

        }, error = function(e) {

                stop(paste("get_predicted_word: ", e, sep = ""))

                return(NULL)

        }, finally = {

        })

        return(predicted_word)

}

get_plot <- function(df) {

        tryCatch({

                p <- ggplot(df, aes(x = reorder(ngram, count), y = count)) +
                        geom_bar(stat = "identity", fill = "#7698D4") +
                        coord_flip() +
                        labs(x = "Predicted Words (n-grams)", y = "Count") +
                        theme(legend.position = "none")

        }, warning = function(w) {

                warning(paste("get_plot: ", w, sep = ""))

                return(NULL)

        }, error = function(e) {

                stop(paste("get_plot: ", e, sep = ""))

                return(NULL)

        }, finally = {

        })

        return(p)

}

get_heatmap <- function(df) {

  tryCatch({

    p <- ggplot(df, aes(x = ngram, y = frequency, fill = frequency)) +
            geom_tile() +
            scale_fill_gradient(low = "lightblue",
                                high = "darkblue") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(title = "Text Heatmap of Word Frequencies",
                 x = "Ngrams",
                 y = "Frequency")

  }, warning = function(w) {

          warning(paste("get_heatmap: ", w, sep = ""))

          return(NULL)

  }, error = function(e) {

          stop(paste("get_heatmap: ", e, sep = ""))

          return(NULL)

  }, finally = {

  })

  return(p)

}
