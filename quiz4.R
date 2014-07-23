#### QUIZ 1 ####
library(ElemStatLearn)
library(caret)

training <- vowel.train
testing <- vowel.test
training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)

set.seed(33833)
mod1 <- train(y~.,data = training, method = "rf")
mod2 <- train(y~.,data = training, method = "gbm",verbose=F)

rf <- predict(mod1,testing)
gbm <- predict(mod2,testing)

sum(testing$y==rf)/length(testing$y)
sum(testing$y==gbm)/length(testing$y)

ix <- gbm==rf
sum(testing$y[ix] == rf[ix])/sum(ix)


#### QUIZ 2 ####
set.seed(3433)
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

modrf <- train(diagnosis~.,data = training, method = "rf")
modgbm <- train(diagnosis~.,data = training, method = "gbm",verbose=F)
modlda <- train(diagnosis~.,data = training, method = "lda")

predrf <- predict(modrf,testing)
predgbm <- predict(modgbm,testing)
predlda <- predict(modlda,testing)

predDF <- data.frame(predrf,predgbm,predlda,diagnosis=testing$diagnosis)
combMod <- train(diagnosis~.,data = predDF, method = "rf")
combPred <- predict(combMod,predDF)

sum(testing$diagnosis==combPred)/length(combPred)
sum(testing$diagnosis==predrf)/length(predrf)
sum(testing$diagnosis==predgbm)/length(predgbm)
sum(testing$diagnosis==predlda)/length(predlda)

#### QUIZ 3 ####
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modFit <- train(CompressiveStrength~.,data = training, method = "lasso")

?plot.enet
