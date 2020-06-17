context("create_ngrams")

# Load seuss data set.

data(seuss)

# Test create_corpus function.

cp <- create_corpus(seuss)

tokens <- unlist(quanteda::tokens(cp))

test_that("function generates a corpus class object", {

      expect_that(cp, is_a("character"))
      expect_that(length(cp), equals(1))
      expect_that(length(tokens), equals(964))

})

test_that("inputs generate errors", {

      expect_that(create_corpus(), throws_error())
      expect_that(create_corpus(abc), throws_error())

})

# Test create_ngrams function with unigrams.

un <- create_ngrams(cp)



test_that("frequency table has the proper structure", {

   expect_that(un, is_a("data.frame"))
   expect_that(colnames(un), equals(c("ngram",
                                      "count",
                                      "frequency",
                                      "cumulative_count",
                                      "cumulative_frequency")))
   expect_that(ncol(un), equals(5))
   expect_that(nrow(un), equals(67))
   expect_that(sum(un$count), equals(un$cumulative_count[nrow(un)]))
   expect_that(round(sum(un$frequency)), equals(1))
   expect_that(un$cumulative_frequency[nrow(un)], equals(1))
   expect_that(un$ngram, is_a("character"))
   expect_that(un$count, is_a("integer"))
   expect_that(un$frequency, is_a("numeric"))
   expect_that(un$cumulative_count, is_a("integer"))
   expect_that(un$cumulative_frequency, is_a("numeric"))

})

# Test create_ngrams function with trigrams.

tr <- create_ngrams(cp, 3)

test_that("frequency table has the proper structure", {

      expect_that(tr, is_a("data.frame"))
      expect_that(colnames(tr), equals(c("ngram",
                                            "count",
                                            "frequency",
                                            "cumulative_count",
                                            "cumulative_frequency")))
      expect_that(ncol(tr), equals(5))
      expect_that(nrow(tr), equals(314))
      expect_that(sum(tr$count), equals(tr$cumulative_count[nrow(tr)]))
      expect_that(round(sum(tr$frequency)), equals(1))
      expect_that(tr$cumulative_frequency[nrow(tr)], equals(1))
      expect_that(tr$ngram, is_a("character"))
      expect_that(tr$count, is_a("integer"))
      expect_that(tr$frequency, is_a("numeric"))
      expect_that(tr$cumulative_count, is_a("integer"))
      expect_that(tr$cumulative_frequency, is_a("numeric"))

})

# Test error handling.

test_that("inputs generate errors", {

      expect_that(create_ngrams(), throws_error())
      expect_that(create_ngrams(abc), throws_error())

})
