# Random forest script based off of Maching Learning in R book
# 2/16/22
# MRC
#--------------------------------------------------------------

library(randomForest)
library(datasets)
library(caret)
library(dplyr)
library(lattice)
library(e1071)
library(vcd)
library(pROC)
# load dataframes
CoastalSub<- read.csv('CoastalSubsample.csv')
CoastalSub$X<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
Species44<- rbind(CoastalSub, Ocean)
Species44$Recording<- NULL

data<- Species44
data$Ecotype <- as.factor(data$Ecotype) # make ecotype a factor variable
table(data$Ecotype)
# Data Partition, set random seed 
set.seed(1234)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.67, 0.33))
train <- data[ind==1,] # 630 whistles
test <- data[ind==2,] # 302 whistles

# Run default rf runs with ntrees=500, mtry=sqrt(features)=2 in my case
set.seed(1234)
rf_default <- randomForest(Ecotype ~., data=train) 
rf_default 


# OOB estimate of  error rate: 20.48%
# Confusion matrix:
#           Coastal Oceanic class.error
# Coastal     259      57   0.1803797
# Oceanic      72     242   0.2292994

Kappa(rf_default$confusion[1:2,1:2])

#             value     ASE     z  Pr(>|z|)
# Unweighted 0.5904 0.03212 18.38 1.891e-75
# Weighted   0.5904 0.03212 18.38 1.891e-75

# Tuning: set control
ctrl<- trainControl(method='repeatedcv', 
                    number=10,repeats=10,
                    selectionFunction = "best",
                    savePredictions = TRUE, 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)
# Tuning: set tuning grid for mtry
grid_rf<- expand.grid(mtry= c(1, 2,4,7))

# Tune: with train() function using ctrl
set.seed(1234)
m_rf<- train(Ecotype~., data=train, method='rf',
             metric='ROC',
             trControl=ctrl,
             tuneGrid=grid_rf)
m_rf 

# Resampling results across tuning parameters:
  
#   mtry  ROC        Sens       Spec     
#   1     0.8839616  0.8014919  0.7815423
#   2     0.8850682  0.8056250  0.7898185
#   4     0.8826684  0.7951613  0.7894657
#   7     0.8783316  0.7910383  0.7907258

# ROC was used to select the optimal model
# using the largest value.
# The final value used for the model was mtry = 2.

# plot ROC curve

roc_rf<- roc(m_rf$pred$obs, m_rf$pred$Oceanic)
plot(roc_rf, col='blue', legacy.axes=TRUE)
plot(m_rf)

# optimal rf is same as default rf
pTest<- predict(rf_default, test)
confusionMatrix(pTest, test$Ecotype)

# Confusion Matrix and Statistics

# Reference
# Prediction Coastal Oceanic
# Coastal     120      25
# Oceanic      30     127

# Accuracy : 0.8179          
# 95% CI : (0.7696, 0.8598)
# No Information Rate : 0.5033          
# P-Value [Acc > NIR] : <2e-16          

# Kappa : 0.6357          

# Mcnemar's Test P-Value : 0.5896          
                                          
#             Sensitivity : 0.8000          
#            Specificity : 0.8355          
#          Pos Pred Value : 0.8276          
#          Neg Pred Value : 0.8089          
#              Prevalence : 0.4967          
#          Detection Rate : 0.3974          
#    Detection Prevalence : 0.4801          
#       Balanced Accuracy : 0.8178          
                                          
#        'Positive' Class : Coastal 

varImpPlot(rf_default,
           sort = T, # sort in decreasing order
           n.var = 7, # how many variables
           main = "Variable Importance")
VarImportance<- as.table<- importance(rf_default)
VarImportance
varImpPlot(rf_default, main='Variable Importance')

# Partial Dependence Plots
partialPlot(rf_default, as.data.frame(train), Minimum.Frequency..kHz., "Coastal",
            xlab='Minimum Frequency', 
            ylab='Partial Dependence', 
            main='')
partialPlot(rf_default, as.data.frame(train), Minimum.Frequency..kHz., "Oceanic",
            xlab='Minimum Frequency', 
            main='')

partialPlot(rf_default, as.data.frame(train), Peak.Frequency..kHz., "Coastal",
            xlab='Peak Frequency', 
            ylab='Partial Dependence', 
            main='')
partialPlot(rf_default, as.data.frame(train), Peak.Frequency..kHz., "Oceanic",
            xlab='Peak Frequency', 
            main='')

partialPlot(rf_default, as.data.frame(train), Duration..s., "Coastal",
            xlab='Duration', 
            ylab='Partial Dependence', 
            main='')
partialPlot(rf_default, as.data.frame(train), Duration..s., "Oceanic",
            xlab='Duration', 
            main='')
