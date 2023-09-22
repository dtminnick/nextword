
test_data <- data.frame(

    input_sequence = c("this is an", "example of a", "predict the"),

    ground_truth = c("example", "sentence", "next")
)

calculate_accuracy <- function(test_data, model) {

    correct_predictions <- 0

    total_predictions <- 0

    for (i in 1:nrow(test_data)) {

        input_sequence <- test_data$input_sequence[i]

        ground_truth <- test_data$ground_truth[i]

        input_tokens <- tokenize_hf(input_sequence, model_name = model_name)

        next_word <- predict_next_token_hf(model, input_tokens)

        if (next_word == ground_truth) {

            correct_predictions <- correct_predictions + 1

        }

        total_predictions <- total_predictions + 1
    }

    accuracy <- correct_predictions / total_predictions

    return(accuracy)

}

accuracy <- calculate_accuracy(test_data, model)

cat("Accuracy:", accuracy, "\n")
