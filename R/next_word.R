
get_obs_trigrams <- function(phrase, trigrams) {

      tryCatch({

            obs_trigrams <- data.table(ngram = vector(mode = "character", length = 0),
                                       frequency = vector(mode = "integer", length = 0))

            # regex <- sprintf("%s%s%s", "^", phrase, "_")

            indices <- grep(phrase, trigrams$ngram)

            if (length(indices) > 0) {

                  obs_trigrams <- trigrams[indices, ]

            }

      }, warning = function(w) {

            warning(paste("get_obs_trigrams: ", w, sep = ""))

            return(NULL)

      }, error = function(e) {

            stop(paste("get_obs_trigrams: ", e, sep = ""))

            return(NULL)

      }, finally = {

      })

      return(obs_trigrams)

}
