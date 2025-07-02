# clean_data.R
library(tidyverse)
library(tm)
library(SnowballC)

# Set working directory
setwd("C:/Users/Master Charles/Downloads/Phase5/fake-news-dashboard")  # Update if needed

# Load datasets with show_col_types = FALSE to suppress warnings
fake_news <- read_csv("data/Fake.csv", show_col_types = FALSE)
real_news <- read_csv("data/True.csv", show_col_types = FALSE)

# Add labels
fake_news <- fake_news %>% mutate(label = "FAKE")
real_news <- real_news %>% mutate(label = "REAL")

# Combine into one dataset
news_data <- bind_rows(fake_news, real_news)

# Clean text function (✅ Works directly on strings)
clean_text <- function(text_vec) {
  text_vec %>%
    tolower() %>%                     # Lowercase
    removePunctuation() %>%           # Remove punctuation
    removeNumbers() %>%               # Remove numbers
    stripWhitespace() %>%             # Trim whitespace
    removeWords(stopwords("en")) %>%   # Remove English stopwords
    wordStem(language = "english")    # Stem words
}

# Apply cleaning
news_data$cleaned_text <- sapply(news_data$text, clean_text)

# Feature Engineering
news_data$article_length <- nchar(news_data$text)
news_data$word_count <- sapply(strsplit(as.character(news_data$text), "\\s+"), length)

# Save cleaned dataset
write_csv(news_data, "data/cleaned_fake_news.csv")

cat("✅ Cleaned dataset saved with", nrow(news_data), "rows.\n")