
# Load libraries.

library("readr")

# Read source texts.

emerson <- read_lines("./inst/extdata/emerson.txt")

faulkner <- read_lines("./inst/extdata/faulkner.txt")

seuss <- read_lines("./inst/extdata/seuss.txt")

# Save source texts.

usethis::use_data(emerson, overwrite = TRUE)

usethis::use_data(faulkner, overwrite = TRUE)

usethis::use_data(seuss, overwrite = TRUE)