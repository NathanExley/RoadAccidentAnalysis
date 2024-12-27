install.packages("gbm")
library(gbm)
Dataset_filtered$Weather <- as.factor(Dataset_filtered$Weather)
class(Dataset_filtered$Weather)
str(Dataset_filtered$Weather)
str(train_data)
train_data$Weather <- as.factor(train_data$Weather)
train_data$Road_Type <- as.factor(train_data$Road_Type)
train_data$Time_of_Day <- as.factor(train_data$Time_of_Day)
train_data$Road_Condition <- as.factor(train_data$Road_Condition)
train_data$Vehicle_Type <- as.factor(train_data$Vehicle_Type)
train_data$Road_Light_Condition <- as.factor(train_data$Road_Light_Condition)

test_data$Weather <- as.factor(test_data$Weather)
test_data$Road_Type <- as.factor(test_data$Road_Type)
test_data$Time_of_Day <- as.factor(test_data$Time_of_Day)
test_data$Road_Condition <- as.factor(test_data$Road_Condition)
test_data$Vehicle_Type <- as.factor(test_data$Vehicle_Type)
test_data$Road_Light_Condition <- as.factor(test_data$Road_Light_Condition)
gbm_model <- gbm(
  formula = Accident_Severity ~ ., 
  data = train_data, 
  distribution = "multinomial", 
  n.trees = 100, 
  interaction.depth = 4, 
  shrinkage = 0.01, 
  cv.folds = 5
)
print(gbm_model)
summary(gbm_model)

summary(gbm_model, plotit = TRUE)
gbm_model$cv.fitted
y_pred <- predict(gbm_model, newdata = test_data, n.trees = gbm_model$n.trees, type = "response")


# The predicted probabilities are stored in y_pred (for multinomial, this will be a matrix)
# For multi-class classification, we need to select the class with the highest probability
y_pred_class <- colnames(y_pred)[apply(y_pred, 1, which.max)]

# Actual labels in the test set
y_test <- test_data$Accident_Severity

# Calculate accuracy: proportion of correct predictions
accuracy <- sum(y_pred_class == y_test) / length(y_test)

# Print the accuracy
print(paste("Accuracy:", round(accuracy, 4)))

summary_data <- summary(gbm_model, plotit = FALSE)
summary_data$rel.inf
# Adjust margins and font size for the plot
par(mar = c(3, 8, 2, 10))  # Increase left margin (second number) for y-axis labels
barplot(
  summary_data$rel.inf,
  names.arg = summary_data$var,
  horiz = TRUE,              # Horizontal bars for better readability
  las = 1,                   # Make y-axis labels horizontal
  col = "skyblue",           # Add some color to the bars
  main = "Variable Importance in GBM",
  xlab = "Relative Influence",
  cex.names = 0.8            # Adjust label size to fit all labels
)
