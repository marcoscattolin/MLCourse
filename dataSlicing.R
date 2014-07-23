library(caret)
library(kernlab)

#### SLICE DATA AT 75% IN TRAINING SET ####
data(spam)
inTrain <- createDataPartition(spam$type, p = 0.75, list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]


#### CROSS VALIDATION, CREATE 10 FOLDS ####
set.seed(32323)
folds <- createFolds(spam$type, k=10, list=T, returnTrain = T)
sapply(folds,length)


#### BOOTSTRAPPING, CREATE 10 FOLDS RESAMPLING ####
set.seed(32323)
folds <- createResample(spam$type, times=10, list=T)
sapply(folds,length)



#### SLIDING WINDOWS ####
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y = tme, initialWindow = 20,horizon = 10)
names(folds)
# first training and testing window
folds$train$Training001
folds$test$Testing001

# second training and testing window
folds$train$Training002
folds$test$Testing002
