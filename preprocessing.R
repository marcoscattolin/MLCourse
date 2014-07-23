library(caret)
library(kernlab)

#### PREPROCESS ####
data(spam)
inTrain <- createDataPartition(spam$type, p = 0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

preObj <- preProcess(training[,-58],method = c("center","scale"))
preProcessedData <- predict(preObj,training[,-58])

apply(preProcessedData,2,mean)
apply(preProcessedData,2,sd)


#### TRAIN WITH PREPROCESSING ####
modelFit <- train(type~., data=training, preProcess = c("center","scale"), method = "glm")


#### IMPUTING ####
data(spam)
inTrain <- createDataPartition(spam$type, p = 0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]


set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size = 1,prob = 0.05)==1
training$capAve[selectNA] <- NA
summary(training$capAve)

preObj <- preProcess(training[,-58],method = "knnImpute")
capAve <- predict(preObj,training[,-58])$capAve
summary(capAve)

capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth)
