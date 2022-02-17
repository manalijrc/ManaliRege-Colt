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
CoastalSub<- read.csv('CoastalSubsample.csv')
CoastalSub$X<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
SpeciesSubDat44<- rbind(CoastalSub, Ocean)
SpeciesSubDat44$Recording<- NULL

data<- SpeciesSubDat44
data$Ecotype <- as.factor(data$Ecotype)
set.seed(1234)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.67, 0.33)) # splitting data
train <- data[ind==1,]
test <- data[ind==2,]

data_train<- train
data_test<- test


set.seed(1234)
# Run the model with default values
rf_default <- randomForest(Ecotype ~., data=train)
# Print the results
rf_default 

# Number of trees: 500
# No. of variables tried at each split: 2

# OOB estimate of  error rate: 20.32%
# Confusion matrix:
#   Coastal Oceanic class.error
# Coastal     257      59   0.1867089
# Oceanic      69     245   0.2197452

# Start Tuning
# for k-fold cross validation use trainControl() function
trControl <- trainControl(method = "repeatedcv",
                          number = 10,repeats=10,
                          search = "grid",
                          savePredictions=TRUE,
                          classProbs=TRUE)
#tune mtry for a higher accuracy
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(2: 7)) # test mtry from 2-7
rf_mtry <- train(Ecotype ~.,
                 data = data_train,
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
# store_maxtrees <- list()
# tuneGrid<- expand.grid(.mtry=3)
# for (ntree in c(250, 350, 450, 550, 800, 1000, 2000, 5000, 10000)) {
#  set.seed(1234)
#  rf_maxtrees <- train(Ecotype ~.,
#                       data = data_train,
#                       method = "rf",
#                       metric = "Accuracy",
#                       tuneGrid = tuneGrid,
#                       trControl = trControl,
#                       ntree = ntree)
#  key <- toString(ntree)
#  store_maxtrees[[key]] <- rf_maxtrees
#}
# results_tree <- resamples(store_maxtrees)
# summary(results_tree) # ntree should be between 250 and 2000 (same accuracy between)

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