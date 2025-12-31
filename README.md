# NextWord Predictor
This project powers the **Predict Next Word** Shiny app, an interactive tool that predicts the next word a user is likely to type based on an n-gram language model. It was developed as the capstone project for the [Data Science Specialization](https://www.coursera.org/specializations/jhu-data-science) by Johns Hopkins University on Coursera.

You can try the app here: [dtminnick.shinyapps.io/PredictionNextWord](https://dtminnick.shinyapps.io/PredictionNextWord/)

# Project Overview
The app uses a statistical natural language processing (NLP) model trained on a large corpus of English text drawn from blogs, news articles, and Twitter. Using this dataset, I created tokenized n-grams (unigrams through quadgrams), cleaned and optimized the model for fast, reactive prediction in a Shiny environment.

# How It Works
- **User Input:** The app accepts a partial sentence or phrase from the user.
- **Prediction Engine:** It uses a backoff strategy starting with quadgrams, then trigram and bigram models, to find the most likely next word.
- **Output:** Displays the top predicted word and an optional list of alternative suggestions.

The model relies on **Katz's Backoff** and **Good-Turing discounting** to balance prediction accuracy and coverage.

#n NLP Model Details
- **Corpus Sources:** HC Corpora (blogs, news, Twitter)
- **Total Corpus Size:** ~600 MB
- **Tokenization:** `tidytext`, `quanteda`
- **N-gram Levels:** 1- to 4-grams
- **Text Cleaning:** Lowercasing, profanity filtering, punctuation removal, whitespace trimming
