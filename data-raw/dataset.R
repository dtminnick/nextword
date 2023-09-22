
# Read source texts.

emerson <- readr::read_lines("./inst/extdata/samples/emerson.txt")

faulkner <- readr::read_lines("./inst/extdata/samples/faulkner.txt")

seuss <- readr::read_lines("./inst/extdata/samples/seuss.txt")

keywords <- readr::read_lines("./inst/extdata/samples/keywords.txt")

blogs <- readr::read_lines("./inst/extdata/swiftkey/en_US.blogs.txt")

news <- readr::read_lines("./inst/extdata/swiftkey/en_US.news.txt")

tweets <- readr::read_lines("./inst/extdata/swiftkey/en_US.twitter.txt")

# Save source texts.

usethis::use_data(emerson, overwrite = TRUE)

usethis::use_data(faulkner, overwrite = TRUE)

usethis::use_data(seuss, overwrite = TRUE)

usethis::use_data(keywords, overwrite = TRUE)

usethis::use_data(blogs, overwrite = TRUE)

usethis::use_data(news, overwrite = TRUE)

usethis::use_data(tweets, overwrite = TRUE)
