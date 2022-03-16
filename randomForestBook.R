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
# Take random sample from coastal data frame
BigCoastal<- read.csv('Coastal_Measurements_44.csv')
BigCoastal$Population <- NULL
BigCoastal$Recording<- NULL
#Combine Coastal and Oceanic df
Ocean<- read.csv('Oceanic_Measurements_44.csv')
Ocean$Recording<-NULL
# species data set
Species44<- rbind(BigCoastal, Ocean)
str(Species44)
# Test for correlation between variables with Pearson's Test
pCor<- cor(Species44[,unlist(lapply(Species44,is.numeric))])
# Species44$Delta.Frequency..kHz. <- NULL
# no correlations greater than ±0.8 

# take 67% and 33% of each ecotype dataset for training and testing dataset
coastal_indices <- which(Species44$Ecotype=="Coastal")

# take random rows from species dataset 
set.seed(1234)
coastal_samples <- Species44[sample(x=coastal_indices, 466, replace=TRUE),]

species_final <- rbind(coastal_samples, Ocean)


# set ecotype as a factor
species_final$Ecotype<- as.factor(species_final$Ecotype)

# create training and testing
trainIndex <- createDataPartition(species_final$Ecotype, p = .67,
                                  list = FALSE,
                                  times = 1)
train <- species_final[ trainIndex,]
test <- species_final[-trainIndex,]
table(train$Ecotype)
table(test$Ecotype)
# Run default rf runs with ntrees=500, mtry=sqrt(features)=2 in my case
set.seed(1234)
rf_default <- randomForest(Ecotype ~., data=train, importance=TRUE) 
rf_default 
pTest<- predict(rf_default, test)
confusionMatrix(pTest, test$Ecotype)

# calculate kappa from confusion matrix
Kappa(rf_default$confusion[1:2,1:2]) 
# value     ASE     z   Pr(>|z|)
# Unweighted 0.6645 0.02982 22.28 5.372e-110
# Weighted   0.6645 0.02982 22.28 5.372e-110
          
# k stat of 0.66= substantial

# Tuning: set control
ctrl<- trainControl(method='repeatedcv', 
                    number=10,repeats=10,
                    search="grid",
                    selectionFunction = "best",
                    savePredictions = TRUE, 
                    classProbs = TRUE,
                    summaryFunction = twoClassSummary)

# Tuning: set tuning grid for mtry
grid_rf<- expand.grid(mtry= c(1, 2, 3, 4, 6, 7))

# Tune mtry with train() function using ctrl
set.seed(1234)
m_rf<- train(Ecotype~., data=train, method='rf',
             metric='ROC',
             trControl=ctrl,
             tuneGrid=grid_rf)
m_rf 

# Random Forest 
# 626 samples
# 7 predictor
# 2 classes: 'Coastal', 'Oceanic' 

# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 10 times) 
# Summary of sample sizes: 563, 564, 563, 562, 563, 564, ... 
# Resampling results across tuning parameters:
  
#   mtry  ROC        Sens       Spec     
# 1     0.9076629  0.8459677  0.8040020
# 2     0.9079206  0.8532460  0.8028427
# 3     0.9068904  0.8557863  0.8038609
# 4     0.9061878  0.8549093  0.8016734
# 6     0.9034235  0.8523286  0.8007157
# 7     0.9018900  0.8500504  0.8007258

# ROC was used to select the optimal
# model using the largest value.
# The final value used for the model was
# mtry = 2.

# plot ROC curve

roc_rf<- roc(m_rf$pred$obs, m_rf$pred$Coastal)
plot(roc_rf, col='blue', legacy.axes=TRUE)
plot(m_rf)

# tune number of trees (ntree)
grid_rf<- expand.grid(.mtry=2)
modellist <- list()
for (ntree in c(150,300,500,800)) {
  set.seed(1234)
  fit <- train(Ecotype~., data=train, method="rf", metric="ROC", tuneGrid=grid_rf, trControl=ctrl, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
ntree_results<- resamples(modellist)
summary(ntree_results)


# Call:
#   summary.resamples(object = ntree_results)

# Models: 150, 300, 500, 800 
# Number of resamples: 100 
# ROC 
#         Min.   1st Qu.    Median      Mean
# 150 0.8085328 0.8873610 0.9106481 0.9075726
# 300 0.8132154 0.8870968 0.9083474 0.9080679
# 500 0.8147763 0.8885276 0.9124447 0.9085764
# 800 0.8132154 0.8892999 0.9113416 0.9087492
#       3rd Qu.      Max. NA's
# 150 0.9305330 0.9758065    0
# 300 0.9296875 0.9763105    0
# 500 0.9294355 0.9737903    0
# 800 0.9295412 0.9737903    0
# best ntrees=300

# Run final RF
set.seed(1234)
final_rf<- randomForest(Ecotype ~., data=train,
                        mtry=2,
                        ntree=300, 
                        importance=TRUE)
final_rf
# Type of random forest: classification
# Number of trees: 300
# No. of variables tried at each split: 2

# OOB estimate of  error rate: 16.61%
# Confusion matrix:
#          Coastal Oceanic
# Coastal     271      42
# Oceanic      62     251
# class.error
# Coastal   0.1341853
# Oceanic   0.1980831
pTrain<- predict(final_rf, train)
confusionMatrix(pTrain, train$Ecotype)

Kappa(final_rf$confusion[1:2,1:2])
#           value     ASE
# Unweighted 0.6677 0.02969
# Weighted   0.6677 0.02969

# final rf on test data
set.seed(1234)
pTest<- predict(final_rf, test)
confusionMatrix(pTest, test$Ecotype)

# Confusion Matrix and Statistics
# Reference
# Prediction Coastal Oceanic
# Coastal     131      27
# Oceanic      22     126

# Accuracy : 0.8399          
# 95% CI : (0.7939, 0.8791)
# No Information Rate : 0.5             
# P-Value [Acc > NIR] : <2e-16          

# Kappa : 0.6797          

# Mcnemar's Test P-Value : 0.5677          
                                          
#             Sensitivity : 0.8562          
#             Specificity : 0.8235          
#          Pos Pred Value : 0.8291          
#          Neg Pred Value : 0.8514          
#              Prevalence : 0.5000          
#          Detection Rate : 0.4281          
#    Detection Prevalence : 0.5163          
#       Balanced Accuracy : 0.8399          
                                          
#        'Positive' Class : Coastal


varImpPlot(final_rf,
           sort = TRUE, # sort in decreasing order
           n.var = 7, # how many variables
           main = "Variable Importance")
VarImportance<- as.table<- importance(final_rf)
VarImportance
write.csv(VarImportance,"/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis/VarImportance.csv")

# Partial Dependence Plots
partialPlot(final_rf, as.data.frame(train), Minimum.Frequency..kHz., "Coastal",
            xlab='Minimum Frequency', 
            ylab='Partial Dependence', 
            main='')
partialPlot(final_rf, as.data.frame(train), Minimum.Frequency..kHz., "Oceanic",
            xlab='Minimum Frequency', 
            main='')

partialPlot(final_rf, as.data.frame(train), Peak.Frequency..kHz., "Coastal",
            xlab='Peak Frequency', 
            ylab='Partial Dependence', 
            main='')
partialPlot(final_rf, as.data.frame(train), Peak.Frequency..kHz., "Oceanic",
            xlab='Peak Frequency', 
            main='')

partialPlot(final_rf, as.data.frame(train), Duration..s., "Coastal",
            xlab='Duration', 
            ylab='Partial Dependence', 
            main='')
partialPlot(final_rf, as.data.frame(train), Duration..s., "Oceanic",
            xlab='Duration', 
            main='')

library(party)
mod<- ctree(Ecotype~., train)
plot(mod)
print(mod)
