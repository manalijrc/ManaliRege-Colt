# Random Forest of Coastal and Oceanic Ecotypes based on Luscinia measurements
# MRC
#---------------------------------------------------------------------------
library(randomForest)
library(datasets)
library(caret)
library(dplyr)
library(lattice)
library(e1071)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")

# load dataframes
CoastalSub<- read.csv('CoastalSubsample.csv')
CoastalSub$Population<- NULL
CoastalSub$X<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
SpeciesSubDat44<- rbind(CoastalSub, Ocean)
SpeciesSubDat44$Recording<- NULL

data<- SpeciesSubDat44
data$Ecotype <- as.factor(data$Ecotype) # make ecotype a factor variable
table(data$Ecotype)
# Data Partition, set random seed 
set.seed(1234)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.67, 0.33))
train <- data[ind==1,] # 635 whistles
test <- data[ind==2,] # 299 whistles

# Default RF
set.seed(1234)
rf <- randomForest(Ecotype ~., data=train, proximity=TRUE) 
print(rf)

# Prediction and confusion matrix for train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$Ecotype)
#Train data accuracy is 100% that indicates all the values classified correctly
# RF on test data, default values
set.seed(1234)
rf2<- randomForest(Ecotype~., data=test, proximity=TRUE)
rf2
# Prediction and Confusion matrix for Test data 
p2 <- predict(rf, test)
confusionMatrix(p2, test$Ecotype) # test data accuracy is 78.48%

plot(rf2) # oob error rate of rf around 22.19% for 500 trees, mtry of 2

# Tune mtry
set.seed(1234)
mtry<- tuneRF(train[,-1], train[,1],
           stepFactor = 1.5,
           ntreeTry= 500,
           trace= TRUE,
           improve=0.01) # mtry=3 is best
best.m<- mtry[mtry[,2]==min(mtry[,2]),1]
print(mtry)
print(best.m)

set.seed(1234)
rf_mtry <- randomForest(Ecotype ~., data=train, mtry=3, proximity=TRUE) 
print(rf_mtry) # OOB error= 18.54%
predict(rf_mtry, train)
confusionMatrix(p1, train$Ecotype)


# tune maxnodes
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
tunegrid<- expand.grid(.mtry=3)
# tune max nodes by creating a loop
store_maxnode <- list() # results of code stored in this list
for (maxnodes in c(5: 25)) { #compute the model with values of maxnodes 15-25
  set.seed(1234)
  rf_maxnode <- train(Ecotype ~.,
                      data = train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tunegrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes, # for each iteration
                      ntree = 500)
  current_iteration <- toString(maxnodes) # store the value of maxnode as a string variable
  store_maxnode[[current_iteration]] <- rf_maxnode # save the result of the model in the list
}
results_mtry <- resamples(store_maxnode)# arrange the results of the model
summary(results_mtry) # print summary of all combinations , try with higher values to see if you can get higher numbers if necessary
# max nodes = 14

# # nodes in the tree, ntree

hist(treesize(rf),
     main = 'No. of Nodes for the Trees',
     col = 'green')

trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
tunegrid= expand.grid(.mtry=2)
store_maxtrees <- list()
set.seed(1234)
for (ntree in c(250, 350, 400, 500, 800, 1000, 5000, 10000, 15000)) {
  set.seed(1234)
  rf_maxtrees <- train(Ecotype ~.,
                       data = train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tunegrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 14,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree) 

# Run final RF
set.seed(1234)
final_rf<- randomForest(Ecotype~., data=train, ntree=800, mtry=3, proximity=TRUE)
print(final_rf)

pTrain <- predict(final_rf, train)
confusionMatrix(pTrain, train$Ecotype)


# Prediction and Confusion matrix for Test data 
pTest <- predict(final_rf, test)
confusionMatrix(pTest, test$Ecotype) # test data accuracy is 79.47%

plot(final_rf) # error rate of rf around 20.25% for 800 trees, mtry of 2

varImpPlot(final_rf,
           sort = T, # sort in decreasing order
           n.var = 7, # how many variables
           main = "Variable Importance")
importance(final_rf)

# Partial Dependence Plots
partialPlot(rf, as.data.frame(train), Minimum.Frequency..kHz., "Coastal")
partialPlot(rf, as.data.frame(train), Minimum.Frequency..kHz., "Oceanic")

partialPlot(rf, as.data.frame(train), Peak.Frequency..kHz., "Oceanic")
partialPlot(rf, as.data.frame(train), Peak.Frequency..kHz., "Coastal")

partialPlot(rf, as.data.frame(train), Duration..s., "Coastal")
partialPlot(rf, as.data.frame(train), Duration..s., "Oceanic")


