library(randomForest)
library(datasets)
library(caret)
library(dplyr)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")


# Take random sample from coastal data frame
BigCoastal<- read.csv('Coastal_Measurements_44.csv')
Coastal<- sample_n(BigCoastal, 467)
Coastal$Population<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
SpeciesSubDat44<- rbind(Coastal, Ocean)
SpeciesSubDat44$Recording<- NULL
# Random Forest Regression
data<- SpeciesSubDat44
data$Ecotype <- as.factor(data$Ecotype) # make ecotype a factor variable
table(data$Ecotype)
# Data Partition, set random seed 
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.67, 0.33))
train <- data[ind==1,] # 635 whistles
test <- data[ind==2,] # 299 whistles

rf <- randomForest(Ecotype ~., data=train, proximity=TRUE) 
print(rf)

# Prediction and confusion matrix for train data
p1 <- predict(rf, train)
confusionMatrix(p1, train$Ecotype)

#Train data accuracy is 100% that indicates all the values classified correctly
# Prediction and Confusion matrix for Test data 
p2 <- predict(rf, test)
confusionMatrix(p2, test$Ecotype) # test data accuracy is 82%

plot(rf) # error rate of rf around 22.36% for 500 trees, mtry of 2

# Tune mtry
t<- tuneRF(train[,-1], train[,1],
           stepFactor = 0.5,
           plot=TRUE,
           ntreeTry= 500,
           trace= TRUE,
           improve=.05) # mtry=2 is best
best.m<- t[t[,2]==min(t[,2]),1]
print(t)
print(best.m)
set.seed(222)
rf <- randomForest(Ecotype ~., data=train, ntree=1000, mtry=2, proximity=TRUE) 
print(rf)



# # nodes in the tree
hist(treesize(rf),
     main = 'No. of Nodes for the Trees',
     col = 'green')
varImpPlot(rf,
           sort = T,
           n.var = 8,
           main = "Variable Importance")
importance(rf)

# Partial Dependence Plots
partialPlot(rf, as.data.frame(train), Minimum.Frequency..kHz., "Coastal")
partialPlot(rf, as.data.frame(train), Minimum.Frequency..kHz., "Oceanic")

partialPlot(rf, as.data.frame(train), Peak.Frequency..kHz., "Oceanic")
partialPlot(rf, as.data.frame(train), Peak.Frequency..kHz., "Coastal")

partialPlot(rf, as.data.frame(train), Duration..s., "Coastal")
partialPlot(rf, as.data.frame(train), Duration..s., "Oceanic")


