
# Read source texts.

emerson <- readr::read_lines("./inst/extdata/emerson.txt")

faulkner <- readr::read_lines("./inst/extdata/faulkner.txt")

seuss <- readr::read_lines("./inst/extdata/seuss.txt")

# Save source texts.

usethis::use_data(emerson, overwrite = TRUE)

usethis::use_data(faulkner, overwrite = TRUE)

usethis::use_data(seuss, overwrite = TRUE)
