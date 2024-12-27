library(randomForest)
Dataset_clean <- na.omit(Dataset)
Dataset_clean <- Dataset_clean %>%
  mutate(Accident_Severity = ifelse(Accident_Severity == "", "Unknown", Accident_Severity))

# Make sure 'Accident_Severity' is a factor
Dataset_clean$Accident_Severity <- as.factor(Dataset_clean$Accident_Severity)

# Check if the empty values are replaced
table(Dataset_clean$Accident_Severity)
train_indices <- sample(1:nrow(Dataset_clean), size = 0.6 * nrow(Dataset_clean))  # 60% for training
train_data <- Dataset_clean[train_indices, ]
test_data <- Dataset_clean[-train_indices, ]

# Define features and target for the training set
X_train <- train_data[, -ncol(train_data)]
y_train <- as.factor(train_data$Accident_Severity)

# Define features and target for the testing set
X_test <- test_data[, -ncol(test_data)]
y_test <- as.factor(test_data$Accident_Severity)
rf_model <- randomForest(X_train, y_train, ntree = 10, importance = TRUE)
print(rf_model)
y_pred <- predict(rf_model, X_test)
accuracy <- sum(y_pred == y_test) / length(y_test)
accuracy
Dataset_filtered <- Dataset_clean %>%
  filter(Accident_Severity != "Unknown")
