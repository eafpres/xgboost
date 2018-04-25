#
# replicate xgboost results from
# https://towardsdatascience.com/detect-parkinsons-with-10-lines-of-code-intro-to-xgboost-51a4bf76b2e6
# 
# credit Priansh Shah
# 
  rm(list = ls())
#
  library(xgboost)
#
  data <- read.csv("parkinsons.csv")
#
# manipulate data for xgboost
#
  status <- as.matrix(data$status)
  data$status <- NULL
  data <- as.matrix(data[, 2:ncol(data)])
  scales <- numeric()
  scale_factors <- c(0, 1)
  for (i in 1:ncol(data)) {
    scales[i] <- max(data[, i]) - min(data[, i])
    data[, i] <- 
      (((scale_factors[2] - scale_factors[1]) * (data[, i] - min(data[, i]))) / 
         scales[i]) + scale_factors[1]
  }
#
# use 14% of data for test data per original article
#
  train_indices <- sample(seq(1, nrow(data), 1), 0.86 * nrow(data), replace = FALSE)
  train_data <- data[train_indices, ]
  train_labels <- status[train_indices]
  test_data <- data[-train_indices, ]
  test_labels <- status[-train_indices]
#  
# train the model and store it
#
  model <- xgboost(data = train_data, label = train_labels, nrounds = 100)
#  
  train_predictions <- predict(model, newdata = train_data)
  train_predictions[train_predictions < 0.5] <- 0
  train_predictions[train_predictions > 0.5] <- 1
  train_accuracy <- 
    100 * length(which(train_predictions == train_labels)) / 
    length(train_labels)
#  
  test_predictions <- predict(model, newdata = test_data)
  test_predictions[test_predictions < 0.5] <- 0
  test_predictions[test_predictions > 0.5] <- 1
  test_accuracy <- 
    100 * length(which(test_predictions == test_labels)) / 
    length(test_labels)
#  
  cat("accuracy on test data = ", test_accuracy,
      "\naccuracy on train data = ", train_accuracy)  
  