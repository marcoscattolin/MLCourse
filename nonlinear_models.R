library(caret)
library(rattle)
library(rpart)
library(rpart.plot)



#### CLASSIFICATION TREE ####
data <- iris
inTrain <- createDataPartition(y= data$Species, p = 0.7,list = F)

training <- data[inTrain,]
testing <- data[-inTrain,]


#TRAIN MODEL WITH RPART AND PLOT IT
model <- train(Species~., data = training,method = "rpart")
fancyRpartPlot(model$finalModel)

#predict on test set
predict(model,testing)
confusionMatrix(testing$Species,predict(model,testing))



#### BAGGING ####

predictors <- data.frame(ozone = airquality$Ozone)
temperature <- airquality$Temp

treebag <- bag(predictors, temperature, B = 10, 
               bagControl = bagControl(fit = ctreeBag$fit, 
                                       predict = ctreeBag$pred, 
                                       aggregate = ctreeBag$aggregate))



plot(predictors$ozone,temperature,col="lightgrey",pch=19)
points(predictors$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(predictors$ozone,predict(treebag,predictors),pch=19,col="blue")

#### RANDOM FOREST ####

data(iris)
inTrain <- createDataPartition(y = iris$Species,p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modFit <- train(Species~., data = training,method = "rf",prox=T)
modFit
getTree(modFit$finalModel,k=2)  #inspect the second tree

# plot class centers
irisP <- classCenter(training[,c(3,4)],training$Species,modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- row.names(irisP)
qplot(Petal.Width,Petal.Length,col=Species,data = training) + geom_point(aes(x=Petal.Width,y=Petal.Length),size=5,shape=4,data = irisP)

#predict
pred <- predict(modFit,testing)
table(pred,testing$Species)


#### BOOSTING ####


library(ISLR)
data(Wage)
Wage <- subset(Wage,select = -c(logwage))
inTrain <- createDataPartition(y=Wage$wage,p = 0.7,list = F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage~.,method = "gbm",data=training,verbose=F)
print(modFit)

qplot(predict(modFit,testing),wage,data = testing)



#### LINEAR DISCRIMINANT ANALYSIS & NAIVE BAYES####
data(iris)
inTrain <- createDataPartition(y = iris$Species,p=0.7, list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]

modlda <- train(Species~.,data=training,method = "lda")
modnb <- train(Species~.,data=training,method = "nb")

plda <- predict(modlda,testing)
pnb <- predict(modnb,testing)

table(plda,pnb)



