
data(blogs)

source <- get_sample(blogs, sample_size = 0.10)

cp <- create_corpus(source)

trigrams <- create_ngrams(cp, n = 3, simple = FALSE)

View(trigrams)

next_word <- next_word_simple("i want",
                              source,
                              simple = FALSE)

