# train_model.R
library(tidyverse)
library(caret)
library(text2vec)
library(xgboost)

# Load cleaned data
news_data <- read_csv("data/cleaned_fake_news.csv")

# Create TF-IDF features
tokens <- itoken(news_data$cleaned_text)
vocab <- create_vocabulary(tokens)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(tokens, vectorizer, weighting = weightTfIdf)

# Convert to matrix
tfidf_matrix <- as.matrix(dtm)

# Combine with metadata
final_data <- cbind(tfidf_matrix,
                    label = as.integer(factor(news_data$label)),
                    subject = as.integer(factor(news_data$subject)),
                    article_length = news_data$article_length)

# Split data
set.seed(123)
train_index <- createDataPartition(final_data$label, p = 0.8, list = FALSE)
train_x <- final_data[train_index, ]
test_x <- final_data[-train_index, ]

# Prepare for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(train_x[, -1]), label = train_x$label)
dtest <- xgb.DMatrix(data = as.matrix(test_x[, -1]))

# Train model
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)

xgb_model <- xgboost(params = params, data = dtrain, nrounds = 100)

# Predict
preds <- predict(xgb_model, dtest)
pred_class <- ifelse(preds > 0.5, 1, 0)

# Evaluate
confusionMatrix(factor(pred_class), factor(test_x$label))
roc_auc <- pROC::auc(pROC::roc(test_x$label, preds))
cat("ROC AUC:", roc_auc, "\n")

# Save model
dir.create("models", showWarnings = FALSE)
save(xgb_model, file = "models/xgb_model.RData") 