library(randomForest)
library(datasets)
library(caret)
# Random Forest Regression
data<- CoastData44
data$Subspecies <- as.factor(data$Subspecies)
set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.67, 0.33))
train <- data[ind==1,]
test <- data[ind==2,]

rf <- randomForest(Subspecies ~., data=train, proximity=TRUE, ntree = 10000) 
print(rf)


p1 <- predict(rf, train)
confusionMatrix(p1, train$Subspecies)

#Train data accuracy is 100% that indicates all the values classified correctly.

p2 <- predict(rf, test)
confusionMatrix(p2, test$Subspecies)
plot(rf)

varImpPlot(rf,
           sort = T,
           n.var = 7,
           main = "Variable Importance")
importance(rf)

partialPlot(rf, as.data.frame(train), Maximum_frequency, "Coastal", main = "", xlab = "Maximum frequency (kHz)", ylab = "", cex.lab=2)
mtext("Partial dependence", side=2, line=2.8, cex=2)
partialPlot(rf, as.data.frame(train), Duration, "Coastal")
partialPlot(rf, as.data.frame(train), Duration, "Oceanic")

