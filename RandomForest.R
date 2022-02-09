# Random forest #

# https://www.guru99.com/r-random-forest-tutorial.html #

library(dplyr)
library(lattice)
library(randomForest)
library(caret)
library(e1071)
library(randomForest)
library(datasets)
library(caret)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")

# Load dataframes
# Take random sample from coastal data frame
BigCoastal<- read.csv('Coastal_Measurements_44.csv')
Coastal<- sample_n(BigCoastal, 467)
Coastal$Population<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
SpeciesSubDat44<- rbind(Coastal, Ocean)
SpeciesSubDat44$Recording<- NULL

data<- SpeciesSubDat44
data$Ecotype <- as.factor(data$Ecotype)
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.67, 0.33))
train <- data[ind==1,]
test <- data[ind==2,]

data_train<- train
data_test<- test

# Default setting, for k-fold cross validation use trainControl() function
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")



set.seed(1234)
# Run the model with default values
rf_default <- train(Ecotype ~., data = data_train, method = "rf", metric = "Accuracy", trControl = trControl)
# Print the results
print(rf_default) # algorithm used 500 trees and tested 3 values of mtry: 2, 4, 7. The final mtry was 4

#tune mtry for a higher accuracy
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10)) # test mtry from 1-10
rf_mtry <- train(Ecotype ~.,
                 data = data_train,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)


rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy)

best_mtry <- rf_mtry$bestTune$mtry 
best_mtry #best mtry=3

# tune max nodes by creating a loop
store_maxnode <- list() # results of code stored in this list
tuneGrid <- expand.grid(.mtry = best_mtry) #use best value of mtry
for (maxnodes in c(5: 25)) { #compute the model with values of maxnodes 15-25
  set.seed(1234)
  rf_maxnode <- train(Ecotype ~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes, # for each iteration, maxnodes = the current value of maxnodes
                      ntree = 300)
  current_iteration <- toString(maxnodes) # store the value of maxnode as a string variable
  store_maxnode[[current_iteration]] <- rf_maxnode # save the result of the model in the list
}
results_mtry <- resamples(store_maxnode)# arrange the results of the model
summary(results_mtry) # print summary of all combinations , try with higher values to see if you can get higher numbers if necessary

# my highest value was 17 and 18

# tune the number of trees, same method as maxnodes
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000, 10000, 15000)) {
  set.seed(5678)
  rf_maxtrees <- train(Ecotype ~.,
                       data = data_train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 29,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) # ntree should be 10,000 or 15,000 based on this

# run final model
fit_rf <- train(Ecotype ~.,
                data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 10000,
                maxnodes = 17)

# evaluate model 
prediction <-predict(fit_rf, data_test)

confusionMatrix(prediction, data_test$Ecotype)

# visualize the model
varImp(fit_rf)
