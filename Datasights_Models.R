
rubi1=read.csv("c:/rubi1.csv",header =TRUE)
predictor=read.csv("c:/Test_Predictor_Final.csv",header =TRUE)
predictorbag=read.csv("c:/Test_Predictor_Final.csv",header =TRUE)
predictorrandomforest=read.csv("c:/Test_Predictor_Final.csv",header =TRUE)
predictorgradientboosting=read.csv("c:/Test_Predictor_Final.csv",header =TRUE)
predictoradaboosting=read.csv("c:/Test_Predictor_Final.csv",header =TRUE)


#summary(rubi1)
#--------------------Datasplitting-------------------------------------

library(caTools)
set.seed(100)
split=sample.split(rubi1,SplitRatio = 0.70)
train=subset(rubi1,split==TRUE)
TEST=subset(rubi1,split==FALSE)

#--------------------Classification with Bagging---------------------

#Bagging
library (randomForest)
set.seed (0)
bagging =randomForest(TARGET~BALANCE+SCR+HOLDING.PERIOD+LENGTH.OF.RELATIONSHIP.IN.MONTHS+Total.Amount.Debited+Avg..Amount.per.Debit.Transaction+AGE+Number.of.Credit.Transactions+No..of.Debit.Transactions+OCCUPATION+Has.Credit.Card...1..Yes..0..No+GENDER, data = train ,method='class', 
                      mtry=12, importance =TRUE)

train$bagging <- predict(bagging,train,type='response')

TEST$bagging <- predict(bagging,TEST,type='response')

table(train$TARGET,train$bagging>0.5) 
table(TEST$TARGET,TEST$bagging>0.5)

summary(bagging)

#-------------Prediction on final dataset---------------------

#levels(predictorbag$GENDER)=levels(train$GENDER)

predictorbag$baggingProb <- predict(bagging,newdata = predictorbag,type='response')
#View(predictorbag)
predictorbag$baggingPred= ifelse(predictorbag$baggingProb>0.5,1,0)


#-------------Random Forest-----------------------------------

# load the library
library(caret)
# load the iris dataset
data(iris)

#--------For parameter hyperparameter tuning----------------

# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
model <- train(TARGET~BALANCE+SCR+HOLDING.PERIOD+LENGTH.OF.RELATIONSHIP.IN.MONTHS+Total.Amount.Debited+Avg..Amount.per.Debit.Transaction+AGE+Number.of.Credit.Transactions+No..of.Debit.Transactions+OCCUPATION+Has.Credit.Card...1..Yes..0..No+GENDER, data=train, 
               trControl=train_control, method="rf")

# summarize results
print(model)

--------------#Random Forest Model-------------------------------------

randomForest<- randomForest(TARGET~BALANCE+SCR+HOLDING.PERIOD+LENGTH.OF.RELATIONSHIP.IN.MONTHS+Total.Amount.Debited+Avg..Amount.per.Debit.Transaction+
                            AGE+Number.of.Credit.Transactions+No..of.Debit.Transactions+OCCUPATION+Has.Credit.Card...1..Yes..0..No+GENDER, data = train ,
                            method='class',mtry=12, ntree=500)


summary(randomForest)


#-------------------Train predictions

rfVar=predict(randomForest,train,type='class')
table(train$TARGET,rfVar>0.5)


#-------------------Test Predicitions

TEST$RandomForest <- predict(randomForest,TEST,type='class')
table(TEST$TARGET,TEST$RandomForest>0.5)

#-----------------------------------------------------------------------


#--------Predicting using Random forest on the final model--------------

#levels(predictorrandomforest$GENDER)=levels(train$GENDER)

predictorrandomforest$rfProb <- predict(randomForest,newdata = predictorrandomforest,type='response')
#View(predictorbag)

predictorrandomforest$rfPred= ifelse(predictorrandomforest$rfProb>0.5,1,0)
predictorrandomforest$ï..TARGET=predictorrandomforest$rfPred


#---------------------Gradient Boosting---------------------------------------------------


#install.packages('gbm')
library (gbm)

boosting = gbm(TARGET~BALANCE+SCR+HOLDING.PERIOD+LENGTH.OF.RELATIONSHIP.IN.MONTHS+Total.Amount.Debited+Avg..Amount.per.Debit.Transaction+AGE+Number.of.Credit.Transactions+
               No..of.Debit.Transactions+OCCUPATION+Has.Credit.Card...1..Yes..0..No+GENDER, data = train,
               distribution="bernoulli",n.trees =5000 , interaction.depth =4, shrinkage =0.2,verbose =F)

train$GradientBoostPred = predict (boosting, train,n.trees =5000,type = "response")
train$GradientBoostPred= ifelse(train$GradientBoostPred>0.5,1,0)

TEST$GradientBoostPred = predict (boosting, TEST,n.trees =5000,type = "response")
TEST$GradientBoostPred= ifelse(TEST$GradientBoostPred>0.5,1,0)
table(TEST$TARGET,TEST$GradientBoostPred)


#-------------Checking accuracy for gradient boosting model on final  dataset--------------
 
predictorgradientboosting$Pred= predict (boosting, newdata = predictorgradientboosting,n.trees =5000,type = "response")
predictorgradientboosting$Pred= ifelse(predictorgradientboosting$Pred>0.5,1,0)


#-------------------Ada Boosting-----------------------------------------------------------

#install.packages('adabag')
library(adabag)

set.seed(100)
split=sample.split(rubi1,SplitRatio = 0.70)
trainset=subset(rubi1,split==TRUE)
testset=subset(rubi1,split==FALSE)
trainset$TARGET <- as.factor(trainset$TARGET)

adaboost <- boosting(TARGET~BALANCE+SCR+HOLDING.PERIOD+LENGTH.OF.RELATIONSHIP.IN.MONTHS+Total.Amount.Debited+Avg..Amount.per.Debit.Transaction+AGE+Number.of.Credit.Transactions+
                       No..of.Debit.Transactions+OCCUPATION+
                       Has.Credit.Card...1..Yes..0..No+GENDER, 
                     data=trainset, boos=TRUE , mfinal=1000)

AdaBoostPred <- predict(adaboost,testset)

table(AdaBoostPred$class,testset$TARGET)

t1<-adaboost$trees[[1]]
#plot(t1)
#text(t1,pretty=50)

#-----------Predicting the final dataset----------------------------------------------------

PredictorAdaBoostPred <- predict(adaboost,newdata = predictoradaboosting)
predictoradaboosting$predictAdaPred=PredictorAdaBoostPred$class


#------------------------------XGMBoost------------------------------------------

------install.packages("xgboost")
library(xgboost)


set.seed(100)
split=sample.split(rubi1,SplitRatio = 0.60)
trainset=subset(rubi1,split==TRUE)
testset=subset(rubi1,split==FALSE)


trainY = trainset$TARGET == "1"

trainX <- model.matrix(TARGET ~ .-1, data = trainset)
trainX <- trainX[,-3]

testY = testset$TARGET == "1"
testX <- model.matrix(TARGET ~ .-1, data = testset)

testX <- testX[,-3]
#delete additional variable

Xmatrix <- xgb.DMatrix(data = trainX, label= trainY)
Xmatrix_t <- xgb.DMatrix(data = testX, label = testY)

Xgboosting <- xgboost(data = Xmatrix, # the data   
                      nround = 100, # max number of boosting iterations
                      objective = "multi:softmax",eta = 0.2, num_class = 2, max_depth = 100)


xgpred <- predict(Xgboosting, Xmatrix_t)
table(testY, xgpred)


----------------------------------------------------------------------------------------------------------------------------

