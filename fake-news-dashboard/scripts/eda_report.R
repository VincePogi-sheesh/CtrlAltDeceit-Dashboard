# eda_report.R
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)

# Load cleaned data
news_data <- read_csv("data/cleaned_fake_news.csv")

# Label distribution
ggplot(news_data, aes(x = label, fill = label)) +
  geom_bar() +
  labs(title = "Fake vs Real News Count", x = "News Type", y = "Count") +
  theme_minimal()

# Subject-wise distribution
ggplot(news_data, aes(x = subject, fill = label)) +
  geom_bar(position = "dodge") +
  labs(title = "News by Subject and Type", x = "Subject", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Article length comparison
ggplot(news_data, aes(x = label, y = article_length, fill = label)) +
  geom_boxplot() +
  labs(title = "Article Length by News Type", y = "Length (characters)")

# Word cloud for fake news
fake_words <- paste(news_data$cleaned_text[news_data$label == "FAKE"], collapse = " ")
wordcloud(words = str_split(fake_words, " ")[[1]], freq = table(str_split(fake_words, " ")[[1]]),
          min.freq = 50, colors = brewer.pal(6, "Dark2"))

# Word cloud for real news
real_words <- paste(news_data$cleaned_text[news_data$label == "REAL"], collapse = " ")
wordcloud(words = str_split(real_words, " ")[[1]], freq = table(str_split(real_words, " ")[[1]]),
          min.freq = 50, colors = brewer.pal(6, "Set2")) 