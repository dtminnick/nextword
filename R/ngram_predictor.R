#' Read Source File
#'
#' \code{read_source} reads a text file and returns it as a character vector
#' with one element per line.
#'
#' @param file_path A character string containing the path to the text file.
#'
#' @return A character vector with one element for each line of the file.
#'
#' @importFrom readr read_lines
#'
#' @export
read_source <- function(file_path) {
  tryCatch({
    readr::read_lines(file_path)
  }, error = function(e) {
    stop(paste("read_source: ", e, sep = ""))
  })
}

#' Get Sample
#'
#' \code{get_sample} selects a random sample of lines from a character vector.
#'
#' @param source A character vector with one element for each line of a text.
#' @param seed_num A single integer value used to set a seed for random
#'   number generation. \code{seed_num} is coerced to an integer value.
#' @param sample_size A floating point number used to specify the size of the sample
#'   as a probability; should be valued between zero and one. A value of zero returns
#'   an empty character vector.
#'
#' @return A character vector containing the randomly selected lines from the
#'   source vector.
#'
#' @examples
#' data(faulkner)
#' sample <- get_sample(source = faulkner,
#'                      seed_num = 091210,
#'                      sample_size = 0.10)
#'
#' @export
get_sample <- function(source, seed_num = 091210, sample_size = 0.10) {
  # Check function parameters
  if (!is.character(source)) {
    stop("get_sample: source must be a character vector.")
  }
  if (!is.numeric(seed_num)) {
    stop("get_sample: seed_num must contain an integer value.")
  }
  if (!is.numeric(sample_size)) {
    stop("get_sample: sample_size must contain a numeric value.")
  }
  if (sample_size <= 0 || sample_size > 1) {
    stop("get_sample: sample_size must be value between zero and one.")
  }

  # Coerce seed_num parameter to an integer value
  seed_num <- as.integer(seed_num)

  # Set seed
  set.seed(seed_num)

  # Generate random sample
  source[rbinom(n = length(source), size = 1, prob = sample_size) == 1]
}

#' Create Corpus
#'
#' \code{create_corpus} creates a corpus object from a character vector.
#'
#' @param source A character vector with one element for each line of a text.
#'
#' @return A quanteda corpus class object containing the document and corpus data
#'   for subsequent processing. \code{create_corpus} returns a corpus object consisting
#'   of documents compiled from the input vector, with one document per line of text.
#'
#' @importFrom quanteda corpus
#'
#' @examples
#' data(faulkner)
#' cp <- create_corpus(faulkner)
#'
#' @export
create_corpus <- function(source) {
  tryCatch({
    quanteda::corpus(source)
  }, error = function(e) {
    stop(paste("create_corpus: ", e, sep = ""))
  })
}

#' Create Ngrams
#'
#' \code{create_ngrams} creates a set of ngrams from a corpus object, tokenizing the set
#' of words in the corpus, removing punctuation, symbols, numbers, URLs and separators.
#' \code{create_ngrams} splits hyphenated words.
#'
#' @param corpus A quanteda corpus class object.
#' @param n An integer specifying the number of elements to be concatenated in each ngram.
#' @param dec_pos An integer specifying the number of positions following the
#'   decimal to return for frequency and cumulative frequency. Value of '5' returns
#'   numbers with five positions following the decimal.
#' @param min_count An integer specifying the minimum count of ngrams to return, i.e.
#'   min_count = 1 will return all ngrams with a count of 1 or more.
#' @param simple Boolean; if TRUE, then function will return a simple table including
#'   ngram, count and frequency. Otherwise, will return ngram, count, frequency,
#'   cumulative count and cumulative frequency. Default is TRUE.
#'
#' @return A data frame containing the ngrams along with the count, frequency,
#'   of each ngram. Ngrams are created with a space concatenator when combining words.
#'
#' @importFrom dplyr arrange desc filter group_by mutate n summarise
#' @importFrom magrittr "%>%"
#' @importFrom quanteda is.corpus tokens tokens_ngrams
#' @importFrom stringr str_to_lower str_squish
#'
#' @examples
#' data(faulkner)
#' cp <- create_corpus(faulkner)
#' ug <- create_ngrams(corpus = cp,
#'                     n = 1,
#'                     dec_pos = 5,
#'                     min_count = 2)
#'
#' @export
create_ngrams <- function(corpus, n = 1, dec_pos = 5, min_count = 1, simple = TRUE) {
  # Check function parameters
  if (!quanteda::is.corpus(corpus)) {
    stop("create_ngrams: corpus must be a quanteda corpus class object.")
  }
  if (!is.numeric(n)) {
    stop("create_ngrams: n must contain an integer value.")
  }
  if (!is.numeric(dec_pos)) {
    stop("create_ngrams: dec_pos must contain an integer value.")
  }
  if (!is.numeric(min_count)) {
    stop("create_ngrams: min_count must contain an integer value.")
  }
  if (min_count < 1) {
    stop("create_ngrams: min_count must be a value of one or greater.")
  }

  # Coerce n, dec_pos and min_count parameters to integer value
  n <- as.integer(n)
  dec_pos <- as.integer(dec_pos)
  min_count <- as.integer(min_count)

  # Tokenize corpus
  tokens <- quanteda::tokens(corpus,
                             what = 'word',
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             remove_separators = TRUE,
                             split_hyphens = TRUE,
                             verbose = TRUE)

  # Create ngrams
  ngrams <- quanteda::tokens_ngrams(tokens, n, concatenator = " ")

  # Unlist ngrams and coerce to a data frame
  ngrams <- as.data.frame(unlist(ngrams))
  colnames(ngrams) <- c("ngram")

  # Build frequency table
  if (simple) {
    ngrams %>%
      dplyr::mutate(ngram = stringr::str_squish(ngram),
                    ngram = stringr::str_to_lower(ngram)) %>%
      dplyr::group_by(ngram) %>%
      dplyr::summarise(count = dplyr::n()) %>%
      dplyr::arrange(dplyr::desc(count)) %>%
      dplyr::filter(count >= min_count) %>%
      dplyr::mutate(frequency = round(count / sum(count), dec_pos))
  } else {
    ngrams %>%
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
}

#' Ngram Predictor
#'
#' \code{ngram_predictor} predicts the next word in a character string using an
#' input phrase and source content with which to construct an ngram model.
#'
#' @param input_phrase A character string containing the text to be used to predict
#'   the next word.
#' @param source A character vector with one element for each line of a text.
#' @param n An integer specifying the number of elements to be concatenated in each ngram.
#' @param dec_pos An integer specifying the number of positions following the
#'   decimal to return for frequency and cumulative frequency. Value of '5' returns
#'   numbers with five positions following the decimal.
#' @param min_count An integer specifying the minimum count of ngrams to return, i.e.
#'   min_count = 1 will return all ngrams with a count of 1 or more.
#' @param top_results An integer specifying the number of rows to return in the data
#'   frame of matching ngrams, e.g. value of '10' returns the top ten rows.
#' @param simple Boolean; if TRUE, then function will return a simple table including
#'   ngram, count and frequency. Otherwise, will return ngram, count, frequency,
#'   cumulative count and cumulative frequency. Default is TRUE.
#'
#' @return A data frame containing the ngrams matching the input phrase provided,
#'   along with the count and frequency of each ngram.
#'
#' @importFrom dplyr arrange desc filter top_n
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_starts
#'
#' @examples
#' data(faulkner)
#' input_phrase <- "it is"
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
  # Check function parameters
  if (!is.numeric(n)) {
    stop("ngram_predictor: n must contain an integer value.")
  }
  if (!is.numeric(dec_pos)) {
    stop("ngram_predictor: dec_pos must contain an integer value.")
  }
  if (!is.numeric(min_count)) {
    stop("ngram_predictor: min_count must contain an integer value.")
  }
  if (min_count < 1) {
    stop("ngram_predictor: min_count must be a value of one or greater.")
  }

  # Coerce n, dec_pos and min_count parameters to integer value
  n <- as.integer(n)
  dec_pos <- as.integer(dec_pos)
  min_count <- as.integer(min_count)

  # Create corpus
  cp <- create_corpus(source)

  # Create ngrams
  ngrams <- create_ngrams(cp, n, dec_pos, min_count, simple)

  # Filter and arrange next word table
  ngrams %>%
    dplyr::filter(stringr::str_starts(ngram, input_phrase)) %>%
    dplyr::arrange(dplyr::desc(count), ngram) %>%
    dplyr::top_n(top_results)
}

#' Get Predicted Word
#'
#' \code{get_predicted_word} extracts the last word from the first ngram in a
#' prediction table.
#'
#' @param tbl A data frame containing ngram predictions, typically the output
#'   from \code{ngram_predictor}.
#'
#' @return A character string containing the predicted word (last word of the
#'   first ngram in the table).
#'
#' @importFrom stringr word
#'
#' @export
get_predicted_word <- function(tbl) {
  tryCatch({
    if (nrow(tbl) == 0) {
      return(NULL)
    }
    stringr::word(as.character(tbl[1, ]$ngram), -1)
  }, error = function(e) {
    stop(paste("get_predicted_word: ", e, sep = ""))
  })
}

#' Get Plot
#'
#' \code{get_plot} creates a horizontal bar chart of ngram counts from a
#' prediction data frame.
#'
#' @param df A data frame containing ngram predictions with columns 'ngram'
#'   and 'count'.
#'
#' @return A ggplot2 object containing a horizontal bar chart.
#'
#' @importFrom ggplot2 aes coord_flip geom_bar labs theme
#'
#' @export
get_plot <- function(df) {
  tryCatch({
    if (nrow(df) == 0) {
      return(NULL)
    }
    ggplot2::ggplot(df, ggplot2::aes(x = reorder(ngram, count), y = count)) +
      ggplot2::geom_bar(stat = "identity", fill = "#7698D4") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Predicted Words (n-grams)", y = "Count") +
      ggplot2::theme(legend.position = "none")
  }, error = function(e) {
    stop(paste("get_plot: ", e, sep = ""))
  })
}

#' Get Heatmap
#'
#' \code{get_heatmap} creates a heatmap visualization of ngram frequencies.
#'
#' @param df A data frame containing ngram predictions with columns 'ngram'
#'   and 'frequency'.
#'
#' @return A ggplot2 object containing a heatmap tile plot.
#'
#' @importFrom ggplot2 aes element_text geom_tile labs scale_fill_gradient theme
#'
#' @export
get_heatmap <- function(df) {
  tryCatch({
    if (nrow(df) == 0) {
      return(NULL)
    }
    ggplot2::ggplot(df, ggplot2::aes(x = ngram, y = frequency, fill = frequency)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
      ggplot2::labs(title = "Text Heatmap of Word Frequencies",
                    x = "Ngrams",
                    y = "Frequency")
  }, error = function(e) {
    stop(paste("get_heatmap: ", e, sep = ""))
  })
}
