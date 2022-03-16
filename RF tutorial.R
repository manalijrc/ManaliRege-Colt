# RF tutorial: Data science using open source tools decision trees and random forest using R
# 2/17/22
#MRC
#-----------------------------------------------------------------------------------
library(rattle.data)
library(rpart)
# Summary Functions
dim(ds)
head(ds)
tail(ds)
summary(ds)
str(ds)

# Example data prep
data(weather)
head(weather)
dsname<- "weather"
target<- "RainTomorrow"
risk<- "RISK_MM"
ds<- get(dsname)
vars<- colnames(ds)
(ignore<- vars[c(1,2, if(exists("risk")) which(risk==vars))])

vars<- setdiff(vars, ignore)
(inputs<- setdiff(vars,target))               
(nobs<- nrow(ds))

(form<- formula(paste(target, "~.")))
set.seed(1426)
length(train<- sample(nobs, 0.7*nobs))
length(test<- setdiff(seq_len(nobs), train))

# rpart tree exaple

model<- rpart(formula=form, data=ds[train,vars])
print(model)
summary(model)

printcp(model)
plotcp(model)
plot(model)
fancyRpartPlot(model)
prp(model)
