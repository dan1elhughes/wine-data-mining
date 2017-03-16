library(randomForest)

test <- read.csv("C:/Users/dh/Code/datamining/data/test1K.csv", header = FALSE)
training <- read.csv("C:/Users/dh/Code/datamining/data/training100Ku.csv", header = FALSE)

prediction <- data.frame()
errorRates <- data.frame()

subset <- training[,-c(22:28)]

newTrainingData <- subset[1:80000,]
newTestingData <- subset[80001:100000,]

names(test) <- c("ID",
                  "V1", "V2", "V3", "V4", "V5",
                 "V6", "V7", "V8", "V9", "V10",
                 "V11", "V12", "V13", "V14", "V15",
                 "V16", "V17", "V18", "V19", "V20",
                 "V21") # todo: fix this

pcaResult = prcomp(test[ , !(names(test) %in% c("ID"))])
plot(pcaResult, type = "l")

randomForestModel <- randomForest(as.factor(newTrainingData$V29) ~ ., data = newTrainingData, ntree = 100)
results <- as.data.frame(predict(randomForestModel, newTestingData))

predictions <- cbind(results, newTestingData$V29)
names(predictions) <- c("Prediction", "Actual")

foundPositives <- sum(as.numeric(predictions$Prediction == "signal"))
correctPositives <- sum(as.numeric(predictions$Prediction == "signal" & predictions$Actual == "signal"))
allPositives <- sum(as.numeric(predictions$Actual == "signal"))

precision <- correctPositives / foundPositives
recall <- correctPositives / allPositives
accuracy <- correctPositives / nrow(predictions)
fMeasure <- 2 * (precision * recall) / (precision + recall)
