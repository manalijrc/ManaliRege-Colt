# Random forest 
# 2/1/22
# MRC
#------------------------------------------------------------------------

library(dplyr)
library(lattice)
library(randomForest)
library(caret)
library(e1071)
library(datasets)
# setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")

# Load dataframes
# Take random sample from coastal data frame
# BigCoastal<- read.csv('Coastal_Measurements_44.csv')
# Coastal<- sample_n(BigCoastal, 467)
# write.csv(Coastal, "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis/CoastalSubsample.csv")
# load dataframes
CoastalSub<- read.csv('CoastalSubsample.csv')
CoastalSub$X<- NULL
CoastalSub$Recording<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
Ocean$Recording<-NULL
# species data set
Species44<- rbind(CoastalSub, Ocean)

# Test for correlation between variables with Pearson's Test

pCor<- cor(Species44[,unlist(lapply(Species44,is.numeric))])

# no correlations greater than ±0.8, good to go

# take 75% and 25% of each ecotype dataset for training and testing dataset
set.seed(1234)
indC<- sample(2,nrow(CoastalSub), replace=TRUE, prob= c(0.75,0.25))
trainC<- CoastalSub[indC==1,] 
testC<- CoastalSub[indC==2,] 

set.seed(1234)
indO<- sample(2, nrow(Ocean), replace=TRUE, prob= c(0.75,0.25))
trainO<- Ocean[indO==1,]
testO<- Ocean[indO==2,]

train<- rbind(trainC,trainO) #700 whistles
test<- rbind(testC,testO) #232 whistles

# set ecotype as a factor
train$Ecotype<- as.factor(train$Ecotype)
str(train)

test$Ecotype<- as.factor(test$Ecotype)
str(test)


# Run the model with default values
set.seed(1234)
rf_default <- randomForest(Ecotype ~., data=train)
# Print the results
rf_default 

# Number of trees: 500
# No. of variables tried at each split: 2

# OOB estimate of  error rate: 19.86%
# Confusion matrix:
#   Coastal Oceanic class.error
# Coastal     284      66   0.1867089
# Oceanic      73     277   0.2197452

# Start Tuning
# for k-fold cross validation use trainControl() function
trControl <- trainControl(method = "repeatedcv",
                          number = 10,repeats=10,
                          search = "grid",
                          selectionFunction = "best",
                          savePredictions=TRUE,
                          classProbs=TRUE)
#tune mtry for a higher accuracy
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 7)) # test mtry from 2-7
rf_mtry <- train(Ecotype ~.,
                 data = train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl)
rf_mtry
plot(rf_mtry)

rf_mtry$bestTune$mtry #mtry=3 
max(rf_mtry$results$Accuracy) # accuracy of 79.91935%

best_mtry <- rf_mtry$bestTune$mtry 
best_mtry 

# tune max nodes by creating a loop
# store_maxnode <- list() # results of code stored in this list
# tuneGrid <- expand.grid(.mtry = best_mtry) #use best value of mtry
# for (maxnodes in c(5: 25)) { #compute the model with values of maxnodes 15-25
#  set.seed(1234)
#  rf_maxnode <- train(Ecotype ~.,
#                      data = data_train,
#                      method = "rf",
#                      metric = "Accuracy",
#                      tuneGrid = tuneGrid,
#                      trControl = trControl,
#                      importance = TRUE,
#                      nodesize = 14,
#                      maxnodes = maxnodes, # for each iteration, maxnodes = the current value of maxnodes
#                      ntree = 500)
#  current_iteration <- toString(maxnodes) # store the value of maxnode as a string variable
#  store_maxnode[[current_iteration]] <- rf_maxnode # save the result of the model in the list
# }
# results_mtry <- resamples(store_maxnode)# arrange the results of the model
# summary(results_mtry) # print summary of all combinations , try with higher values to see if you can get higher numbers if necessary

# my highest value was 8 @ 95.24% accuracy

# tune the number of trees, same method as maxnodes
store_maxtrees <- list()
tuneGrid<- expand.grid(.mtry=2)
for (ntree in c(100,500,1000,5000,10000)) {
set.seed(1234)
rf_maxtrees <- train(Ecotype ~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      ntree = ntree)
 key <- toString(ntree)
 store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) # ntree should be between 250 and 2000 (same accuracy between)

# run final model
set.seed(1234)
final_rf <- train(Ecotype ~.,
                data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 250,
                maxnodes = 18)

# evaluate model 
prediction <-predict(final_rf, data_test)

confusionMatrix(prediction, data_test$Ecotype)

# visualize the model
varImp(final_rf)

# Final accuracy = 0.7881