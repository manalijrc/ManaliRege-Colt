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


p1 <- predict(rf, train)
confusionMatrix(p1, train$Ecotype)

#Train data accuracy is 100% that indicates all the values classified correctly

p2 <- predict(rf, test)
confusionMatrix(p2, test$Ecotype) # test data accuracy is 83%

plot(rf) # error rate of rf around 22.36%

# Tune mtry
t<- tuneRF(train[,-4], train[,4],
           stepFactor = 0.5,
           plot=TRUE,
           ntreeTry= 150,
           trace= TRUE,
           improve=0.5) # mtry=2 is best
# # nodes in the rree
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

# MDS of proximity matrix

MDSplot(rf, train$Ecotype)
