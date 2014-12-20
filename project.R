library(caret)
library(ggplot2)
library(grid)
library(gridExtra)
setwd("E:/Course/Johns hopkins_Data Science/Practical Machine Learning/Project")
fileUrl<-c("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
           "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
fileName<-c("pml-training.csv","pml-testing.csv")
#for (i in 1 : length(fileUrl)){
#  download.file(fileUrl[i],fileName[i])
#}

trainingDB<-read.csv(fileName[1])
testingDB<-read.csv(fileName[2]) 

# split the cleaned testing data into training and cross validation
inTrain <- createDataPartition(y = trainingDB$classe, p = 0.7, list = FALSE)
train <- trainingDB[inTrain, ]
test <- trainingDB[-inTrain, ]



#choose the features for the model
#roll_variables <- grep("roll", names(train), value = TRUE)
#pitch_variables <- grep("pitch", names(train), value = TRUE)
#yaw_variables <- grep("yaw", names(train), value = TRUE)


train$feature_roll_belt<-(train$roll_belt-mean(train$roll_belt))/sd(train$roll_belt)
train$feature_roll_arm<-(train$roll_arm-mean(train$roll_arm))/sd(train$roll_arm)
train$feature_roll_dumbbell<-(train$roll_dumbbell-mean(train$roll_dumbbell))/sd(train$roll_dumbbell)
train$feature_roll_forearm<-(train$roll_forearm-mean(train$roll_forearm))/sd(train$roll_forearm)

train$feature_pitch_belt<-(train$pitch_belt-mean(train$pitch_belt))/sd(train$pitch_belt)
train$feature_pitch_arm<-(train$pitch_arm-mean(train$pitch_arm))/sd(train$pitch_arm)
train$feature_pitch_dumbbell<-(train$pitch_dumbbell-mean(train$pitch_dumbbell))/sd(train$pitch_dumbbell)
train$feature_pitch_forearm<-(train$pitch_forearm-mean(train$pitch_forearm))/sd(train$pitch_forearm)

train$feature_yaw_belt<-(train$yaw_belt-mean(train$yaw_belt))/sd(train$yaw_belt)
train$feature_yaw_arm<-(train$yaw_arm-mean(train$yaw_arm))/sd(train$yaw_arm)
train$feature_yaw_dumbbell<-(train$yaw_dumbbell-mean(train$yaw_dumbbell))/sd(train$yaw_dumbbell)
train$feature_yaw_forearm<-(train$yaw_forearm-mean(train$yaw_forearm))/sd(train$yaw_forearm)

train<-train[160:172]
# plot a correlation matrix

g<- ggplot(train, aes(classe,feature_roll_belt))
g1<-g + geom_point(aes(color = feature_roll_belt)) + theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_roll_arm))
g2<-g + geom_point(aes(color = feature_roll_arm))+ theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_roll_dumbbell))
g3<-g + geom_point(aes(color = feature_roll_dumbbell))+ theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_roll_forearm))
g4<-g + geom_point(aes(color = feature_roll_forearm))+ theme(legend.position="right")

g<- ggplot(train, aes(classe,feature_pitch_belt))
g5<-g + geom_point(aes(color = feature_pitch_belt)) + theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_pitch_arm))
g6<-g + geom_point(aes(color = feature_pitch_arm))+ theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_pitch_dumbbell))
g7<-g + geom_point(aes(color = feature_pitch_dumbbell))+ theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_pitch_forearm))
g8<-g + geom_point(aes(color = feature_pitch_forearm))+ theme(legend.position="right")


g<- ggplot(train, aes(classe,feature_yaw_belt))
g9<-g + geom_point(aes(color = feature_yaw_belt)) + theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_yaw_arm))
g10<-g + geom_point(aes(color = feature_yaw_arm))+ theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_yaw_dumbbell))
g11<-g + geom_point(aes(color = feature_yaw_dumbbell))+ theme(legend.position="none")
g <- ggplot(train, aes(classe,feature_yaw_forearm))
g12<-g + geom_point(aes(color = feature_yaw_forearm))+ theme(legend.position="right")
grid.arrange(g1, g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12, nrow=3, ncol=4)

# check if zero covariates
nearZeroVar(train,saveMetric=TRUE)

# fit a model to predict the classe using everything else as a predictor
library(randomForest)
model <- randomForest(classe ~ ., data = train,method='rf',prox=TRUE)
names(model)
summary(model )
model[19]
## test dataset
test$feature_roll_belt<-(test$roll_belt-mean(test$roll_belt))/sd(test$roll_belt)
test$feature_roll_arm<-(test$roll_arm-mean(test$roll_arm))/sd(test$roll_arm)
test$feature_roll_dumbbell<-(test$roll_dumbbell-mean(test$roll_dumbbell))/sd(test$roll_dumbbell)
test$feature_roll_forearm<-(test$roll_forearm-mean(test$roll_forearm))/sd(test$roll_forearm)

test$feature_pitch_belt<-(test$pitch_belt-mean(test$pitch_belt))/sd(test$pitch_belt)
test$feature_pitch_arm<-(test$pitch_arm-mean(test$pitch_arm))/sd(test$pitch_arm)
test$feature_pitch_dumbbell<-(test$pitch_dumbbell-mean(test$pitch_dumbbell))/sd(test$pitch_dumbbell)
test$feature_pitch_forearm<-(test$pitch_forearm-mean(test$pitch_forearm))/sd(test$pitch_forearm)

test$feature_yaw_belt<-(test$yaw_belt-mean(test$yaw_belt))/sd(test$yaw_belt)
test$feature_yaw_arm<-(test$yaw_arm-mean(test$yaw_arm))/sd(test$yaw_arm)
test$feature_yaw_dumbbell<-(test$yaw_dumbbell-mean(test$yaw_dumbbell))/sd(test$yaw_dumbbell)
test$feature_yaw_forearm<-(test$yaw_forearm-mean(test$yaw_forearm))/sd(test$yaw_forearm)

test<-test[160:172]

test$predictTest<- predict(model, test)
con<-confusionMatrix(test$classe, test$predictTest)
con$overall[1]


##plot
#test$predictTest <- predict(model, test[2:13])


g<- ggplot(test, aes(classe,feature_roll_belt))
g1<-g + geom_point(aes(color = predictTest)) + theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_roll_arm))
g2<-g + geom_point(aes(color = predictTest))+ theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_roll_dumbbell))
g3<-g + geom_point(aes(color = predictTest))+ theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_roll_forearm))
g4<-g + geom_point(aes(color = predictTest))+ theme(legend.position="right")

g<- ggplot(test, aes(classe,feature_pitch_belt))
g5<-g + geom_point(aes(color = predictTest)) + theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_pitch_arm))
g6<-g + geom_point(aes(color = predictTest))+ theme(legend.position="none")+ ggtitle("Pitch Belt")
g <- ggplot(test, aes(classe,feature_pitch_dumbbell))
g7<-g + geom_point(aes(color = predictTest))+ theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_pitch_forearm))
g8<-g + geom_point(aes(color = predictTest))+ theme(legend.position="right")


g<- ggplot(test, aes(classe,feature_yaw_belt))
g9<-g + geom_point(aes(color = predictTest)) + theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_yaw_arm))
g10<-g + geom_point(aes(color = predictTest))+ theme(legend.position="none")+ ggtitle("Yaw Belt")
g <- ggplot(test, aes(classe,feature_yaw_dumbbell))
g11<-g + geom_point(aes(color = predictTest))+ theme(legend.position="none")
g <- ggplot(test, aes(classe,feature_yaw_forearm))
g12<-g + geom_point(aes(color = predictTest))+ theme(legend.position="right")
grid.arrange(g1, g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12, nrow=3, ncol=4)

names(test)
##prediction
##testingDB


testingDB$feature_roll_belt<-(testingDB$roll_belt-mean(testingDB$roll_belt))/sd(testingDB$roll_belt)
testingDB$feature_roll_arm<-(testingDB$roll_arm-mean(testingDB$roll_arm))/sd(testingDB$roll_arm)
testingDB$feature_roll_dumbbell<-(testingDB$roll_dumbbell-mean(testingDB$roll_dumbbell))/sd(testingDB$roll_dumbbell)
testingDB$feature_roll_forearm<-(testingDB$roll_forearm-mean(testingDB$roll_forearm))/sd(testingDB$roll_forearm)

testingDB$feature_pitch_belt<-(testingDB$pitch_belt-mean(testingDB$pitch_belt))/sd(testingDB$pitch_belt)
testingDB$feature_pitch_arm<-(testingDB$pitch_arm-mean(testingDB$pitch_arm))/sd(testingDB$pitch_arm)
testingDB$feature_pitch_dumbbell<-(testingDB$pitch_dumbbell-mean(testingDB$pitch_dumbbell))/sd(testingDB$pitch_dumbbell)
testingDB$feature_pitch_forearm<-(testingDB$pitch_forearm-mean(testingDB$pitch_forearm))/sd(testingDB$pitch_forearm)

testingDB$feature_yaw_belt<-(testingDB$yaw_belt-mean(testingDB$yaw_belt))/sd(testingDB$yaw_belt)
testingDB$feature_yaw_arm<-(testingDB$yaw_arm-mean(testingDB$yaw_arm))/sd(testingDB$yaw_arm)
testingDB$feature_yaw_dumbbell<-(testingDB$yaw_dumbbell-mean(testingDB$yaw_dumbbell))/sd(testingDB$yaw_dumbbell)
testingDB$feature_yaw_forearm<-(testingDB$yaw_forearm-mean(testingDB$yaw_forearm))/sd(testingDB$yaw_forearm)

testingDB<-testingDB[160:172]

# predict the classes of the test set
predictTest <- predict(model, testingDB)
predictTest
