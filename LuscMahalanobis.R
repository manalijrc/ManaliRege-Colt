# Coastal Mahalanobis Distance
library(dplyr)
# set up
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")


# Import Species Data

CoastData44<- read.csv('Measurements_44.csv')

CoastData44$Recording<- NULL
CoastData44$Population<- NULL
CoastData44$Delta.Frequency..kHz.<- NULL
# Finding distances

Coastal.distances <- mahalanobis(x= CoastData44, center=colMeans(CoastData44), cov=cov(CoastData44))

CoastData44$Mahalanobis<- mahalanobis(x= CoastData44, center=colMeans(CoastData44), cov=cov(CoastData44))

# create p-values

CoastData44$pvalue<- pchisq(CoastData44$Mahalanobis, df=5, lower.tail = FALSE)

outliers<- CoastData44[CoastData44$pvalue< 0.001,]

# explore outliers
outliers.plot <- ggplot(data=outliers, mapping=aes(x=Minimum.Frequency..kHz.,y=Maximum.Frequency..kHz.)) + 
  geom_point()
print(outliers.plot)

LFoutliers.hist<- qplot(x=outliers$Minimum.Frequency..kHz.)
print(LFoutliers.hist)

HFoutliers.hist<- qplot(x=outliers$Maximum.Frequency..kHz.)
print(HFoutliers.hist)

PFoutliers.hist<- qplot(x=outliers$Peak.Frequency..kHz.)
print(PFoutliers.hist)


'Mean'<- sapply(outliers, mean)
as.data.frame(Mean)

'Standard Deviation'<- sapply(outliers, sd)
as.data.frame(`Standard Deviation`)

'Minimum'<- sapply(outliers, min)
as.data.frame(Minimum)

'Maximum'<- sapply(outliers, max)
as.data.frame(Maximum)

'Coefficients of Variation'<- sapply(outliers,function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
as.data.frame(`Coefficients of Variation`)

SumStats_outliers<- rbind(Mean, `Standard Deviation`,`Coefficients of Variation`,Maximum, Minimum)
print(SumStats_outliers)

