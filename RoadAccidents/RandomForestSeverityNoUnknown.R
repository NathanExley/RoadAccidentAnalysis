Dataset_filtered <- Dataset_clean %>%
  filter(Accident_Severity != "Unknown")
Dataset_filtered$Accident_Severity <- as.factor(Dataset_filtered$Accident_Severity)
Dataset_filtered$Accident_Severity <- droplevels(Dataset_filtered$Accident_Severity)
levels(Dataset_filtered$Accident_Severity)
train_indices <- sample(1:nrow(Dataset_filtered), size = 0.8 * nrow(Dataset_filtered))  # 60% for training
train_data <- Dataset_filtered[train_indices, ]
test_data <- Dataset_filtered[-train_indices, ]

# Define features and target for the training set
# Exclude 'Accident_Severity' from the predictor variables
X_train2 <- train_data[, -which(names(train_data) == "Accident_Severity")]

# Define the target variable
y_train2 <- as.factor(train_data$Accident_Severity)
# Define features and target for the testing set
X_test2 <- test_data[, -ncol(test_data)]
X_test2 <- test_data[, -which(names(test_data) == "Accident_Severity")]
y_test2 <- as.factor(test_data$Accident_Severity)
rf_model2 <- randomForest(X_train2, y_train2, ntree = 100, importance = TRUE)
print(rf_model2)
y_pred2 <- predict(rf_model2, X_test2)
accuracy <- sum(y_pred2 == y_test2) / length(y_test2)
accuracy


Importance <- importance(rf_model2)
head(Importance)
Importance
Importance <- as.data.frame(Importance)
Importance <- Importance[order(-Importance$MeanDecreaseAccuracy), ]
head(Importance)
Importance$Low
par(mar = c(3, 8, 2, 10))  # Increase left margin (second number) for y-axis labels
barplot(
  Importance$Low,
  names.arg = summary_data$var,
  horiz = TRUE,              # Horizontal bars for better readability
  las = 1,                   # Make y-axis labels horizontal
  col = "lightgreen",           # Add some color to the bars
  main = "Factors influencing lower severity crashes",
  xlab = "Relative Influence",
  cex.names = 0.8            # Adjust label size to fit all labels
)
barplot(
  Importance$Moderate,
  names.arg = summary_data$var,
  horiz = TRUE,              # Horizontal bars for better readability
  las = 1,                   # Make y-axis labels horizontal
  col = "lightgreen",           # Add some color to the bars
  main = "Factors influencing Moderate severity crashes",
  xlab = "Relative Influence",
  cex.names = 0.8            # Adjust label size to fit all labels
)
barplot(
  Importance$High,
  names.arg = summary_data$var,
  horiz = TRUE,              # Horizontal bars for better readability
  las = 1,                   # Make y-axis labels horizontal
  col = "lightgreen",           # Add some color to the bars
  main = "Factors influencing higher severity crashes",
  xlab = "Relative Influence",
  cex.names = 0.8            # Adjust label size to fit all labels
)
