library(randomForest)

test <- read.csv("C:/Users/dh/Code/datamining/data/test1K.csv", header = FALSE)
training <- read.csv("C:/Users/dh/Code/datamining/data/training100Ku.csv", header = FALSE)

prediction <- data.frame()
errorRates <- data.frame()

# Remove the computed high-level attributes from training data
subset <- training[,-c(22:28)]

# Split training data into training and validating sets
newTrainingData <- subset[1:80000,]
newTestingData <- subset[80001:100000,]

names(test) <- c(
  "ID",
  "V1", "V2", "V3", "V4", "V5",
  "V6", "V7", "V8", "V9", "V10",
  "V11", "V12", "V13", "V14", "V15",
  "V16", "V17", "V18", "V19", "V20", "V21"
)

# Train a random forest on the training data
randomForestModel <- randomForest(as.factor(newTrainingData$V29) ~ ., data = newTrainingData, ntree = 100)

# Use the model to predict the validation set
results <- as.data.frame(predict(randomForestModel, newTestingData))
predictions <- cbind(results, newTestingData$V29)
names(predictions) <- c("Prediction", "Actual")#

# Calculate various counts of positives
truePositives <- sum(as.numeric(predictions$Prediction == "signal" & predictions$Actual == "signal"))
falsePositives <- sum(as.numeric(predictions$Prediction == "signal" & predictions$Actual == "background"))
trueNegatives <- sum(as.numeric(predictions$Prediction == "background" & predictions$Actual == "background"))
falseNegatives <- sum(as.numeric(predictions$Prediction == "background" & predictions$Actual == "signal"))

predictedPositives <- sum(as.numeric(predictions$Prediction == "signal"))
actualPositives <- sum(as.numeric(predictions$Actual == "signal"))

correctPredictions <- sum(as.numeric(predictions$Prediction == predictions$Actual))

# Calculate required indices
precision <- truePositives / (truePositives + falsePositives)
recall <- truePositives / (truePositives + falseNegatives)
accuracy <- (truePositives + trueNegatives) / nrow(predictions)
fMeasure <- 2 * (precision * recall) / (precision + recall)

actualPredictions <- as.data.frame(predict(randomForestModel, test))
output <- cbind(test$ID, actualPredictions)
names(output) <- c("ID", "Class")
write.csv(output, file = "Task3-predictions.csv", row.names = FALSE)
