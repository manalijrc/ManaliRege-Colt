# RF https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
# MRC
# 2/17/22
#---------------------------------------------------------------
# load dataframes
CoastalSub<- read.csv('CoastalSubsample.csv')
CoastalSub$X<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
Species44<- rbind(CoastalSub, Ocean)
Species44$Recording<- NULL

data<- Species44
data$Ecotype<- as.factor(data$Ecotype)
sapply(data, class)
levels(data$Ecotype)

control<- trainControl(method = "cv", number = 10)
metric<- "Accuracy" # the ratio of number of correctly predicted instances divided by the total number of instances x 100

# Random Forest
set.seed(1234)
rf1<- train(Ecotype~., data=data, method="rf", metric=metric, trControl=control)
rf1
