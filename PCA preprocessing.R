library(caret)
library(Hmisc)
library(AppliedPredictiveModeling)


### LESSON ####
data(spam)
inTrain = createDataPartition(spam$type, p = .75,list = F)
training = spam[inTrain,]
testing = spam[-inTrain,]

preProc <- preProcess(log10(training[,-58]+1),method = "pca",pcaComp = 2)
trainPC <- predict(preProc,log10(training[,-58]+1))
plot(trainPC[,1],trainPC[,2],col=training$type)
modelFit <- train(training$type~.,method = "glm",data = trainPC)




##### QUIZ ####
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#### SUBSET ON RELEVANT PREDICTORS ####
training <- cbind(diagnosis=training$diagnosis,training[,grep("^IL",colnames(training))])
testing <- cbind(diagnosis=testing$diagnosis,testing[,grep("^IL",colnames(testing))])

#### TRAIN PCA MODEL RETAINING 80% VARIANCE ####
preProc <- preProcess(training[,-1],method = "pca",thresh = 0.8)
trainPC <- predict(preProc,training[,-1])
modelPCA <- train(training$diagnosis~.,method = "glm",data=trainPC)

#### TESTING PCA MODEL ####
testPC <- predict(preProc,testing[,-1])
confusionMatrix(testing$diagnosis,predict(modelPCA,testPC))



#### TRAIN NON PCA MODEL ####
model <- train(training$diagnosis~.,method = "glm",data=training)

#### TEST NON PCA MODEL ####
confusionMatrix(testing$diagnosis,predict(model,testing))

