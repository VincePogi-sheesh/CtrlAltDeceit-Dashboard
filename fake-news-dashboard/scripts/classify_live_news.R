# classify_live_news.R
library(tidyverse)
library(tm)
library(SnowballC)
library(text2vec)
library(xgboost)

# Load scraped news
live_news <- read.csv("data/live_scraped_news.csv")

# Load training data
news_data <- read.csv("data/predicted_news.csv")

# Clean live news
clean_text <- function(text_vec) {
  text_vec %>%
    content_transformer(tolower)() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(stopwords("en")) %>%
    wordStem(language = "english")
}

live_news$cleaned_summary <- sapply(live_news$summary, clean_text)

# TF-IDF Vectorization
tokens <- itoken(news_data$cleaned_TEXT)
vocab <- create_vocabulary(tokens)
vectorizer <- vocab_vectorizer(vocab)

# Apply to live data
dtm <- create_dtm(itoken(live_news$cleaned_summary), vectorizer, weighting = weightTfIdf)
new_tfidf <- as.matrix(dtm)

# Load model
load("models/xgb_model.RData")

# Predict
dtest <- xgb.DMatrix(new_tfidf)
preds <- predict(xgb_model, dtest)
live_news$prediction <- ifelse(preds > 0.5, "FAKE", "REAL")
live_news$confidence <- round(ifelse(preds > 0.5, preds, 1 - preds), 2)

# Save prediction
write.csv(live_news, "data/live_scraped_news.csv", row.names = FALSE)

cat("✅ Classified", sum(live_news$prediction == "FAKE"), "fake articles.\n")