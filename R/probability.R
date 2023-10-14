
# Sample text

text <- "This is a sample sentence It contains some words"

# Convert to lowercase

text <- tolower(text)

# Remove punctuation

# text <- removePunctuation(text)

# Tokenize into words

words <- unlist(strsplit(text, " "))

n <- 2 # for bigrams

ngrams <- ngram(words, n = n)

target_ngram <- c("contains", "some")

next_word <- "word"

# Count of the target n-

target_ngram_count <- sum(ngrams == target_ngram)

# Count of the target n-gram followed by the next word

target_ngram_next_word_count <- sum(ngrams == c(target_ngram, next_word))

# Calculate the probability

probability <- target_ngram_next_word_count / target_ngram_count

cat("Probability of '", next_word, "' following '", paste(target_ngram, collapse = " "), "' is:", probability)

