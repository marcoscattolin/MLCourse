library(ISLR)
library(caret)
library(splines)

#### CREATE DUMMY VARS ###
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
result <- predict(dummies,newdata = training)
apply(result,2,sum)

#### IDENTIFY NEAR ZERO VARIANCE VARIABLES ####
nsv <- nearZeroVar(training,saveMetrics = T)


#### CRETE THIRD DEGREE VARS ####
bsBasis <- bs(training$age,df = 3)
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

predict(bsBasis,age=tesing$age)
