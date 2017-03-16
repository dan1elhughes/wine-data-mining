library(plyr)
library(randomForest)
library(class)

# ======
# Load data

wine <- read.csv("C:/Users/dh/Code/datamining/data/wine.csv")
data <- wine
k <- 10
data$id <- sample(1:k, nrow(data), replace = TRUE)
list <- 1:k

# ======
# Random forest

predictionRF <- data.frame()
testsetCopyRF <- data.frame()

# Run cross validation across k folds, storing predictions and actual values for later comparison
for (i in 1:k){
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  RFModel <- randomForest(as.factor(trainingset$class) ~ ., data = trainingset, ntree = 100)
  predictionRF <- rbind(predictionRF, as.data.frame(predict(RFModel, testset[,-1])))
  testsetCopyRF <- rbind(testsetCopyRF, as.data.frame(testset[,1]))
}

# Calculate error rate from actual vs predicted
result <- cbind(predictionRF, testsetCopyRF[, 1])
names(result) <- c("Prediction", "Actual")
result$Difference <- abs(as.numeric(result$Actual) - as.numeric(result$Prediction))
result$Error <- as.numeric(result$Difference > 0)
totalErrorsRF <- sum(result$Error)
numTrainingRecords <- nrow(result)
errorRateRF <- totalErrorsRF / numTrainingRecords

predictionKNN <- data.frame()
testsetCopyKNN <- data.frame()

# Run cross validation across k folds, storing predictions and actual values for later comparison
for (i in 1:k){
  trainingset <- subset(data, id %in% list[-i])
  testset <- subset(data, id %in% c(i))
  KNNModel <- knn(trainingset, testset, trainingset$class, k = 3)
  predictionKNN <- rbind(predictionKNN, as.data.frame(KNNModel))
  testsetCopyKNN <- rbind(testsetCopyKNN, as.data.frame(testset[,1]))
}

# Calculate error rate from actual vs predicted
result <- cbind(predictionKNN, testsetCopyKNN[, 1])
names(result) <- c("Prediction", "Actual")
result$Difference <- abs(as.numeric(result$Actual) - as.numeric(result$Prediction))
result$Error <- as.numeric(result$Difference > 0)
totalErrorsKNN <- sum(result$Error)
numTrainingRecords <- nrow(result)
errorRateKNN <- totalErrorsKNN / numTrainingRecords
