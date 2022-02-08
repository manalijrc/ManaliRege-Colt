# set up
library(dplyr)
library(ggplot2)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")
options(scipen=999)

# Import Nicaragua Data
NicData44 <- read.csv('Nic_measurements44.csv')
NicData96 <- read.csv('Nic_measurements96.csv')


NicData44$Recording<- NULL

NicData96$Recording<- NULL

# Import El Salvador Data
EsData96 <- read.csv('ES_measurements96.csv')

EsData96$Recording<- NULL

# Import Species Data
CoastData96<- read.csv('Coastal_Measurements_96.csv')
CoastData44<- read.csv('Coastal_Measurements_44.csv')


# Create Summary Stats for 96 sampling rate
CoastData96$Population<- NULL
CoastData44$Population<- NULL
EsData96$Population<- NULL
NicData96$Population<- NULL
NicData44$Population<- NULL

'Mean'<- sapply(CoastData96, mean)
as.data.frame(Mean)

'Standard Deviation'<- sapply(CoastData96, sd)
as.data.frame(`Standard Deviation`)

'Minimum'<- sapply(CoastData96, min)
as.data.frame(Minimum)

'Maximum'<- sapply(CoastData96, max)
as.data.frame(Maximum)

'Coefficients of Variation'<- sapply(CoastData96,function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
as.data.frame(`Coefficients of Variation`)

SumStats96<- rbind(Mean, `Standard Deviation`,`Coefficients of Variation`,Maximum, Minimum)
print(SumStats)

# Create Summary Stats for 44 sampling rate
'Mean'<- sapply(CoastData44, mean)
as.data.frame(Mean)

'Standard Deviation'<- sapply(CoastData44, sd)
as.data.frame(`Standard Deviation`)

'Minimum'<- sapply(CoastData44, min)
as.data.frame(Minimum)

'Maximum'<- sapply(CoastData44, max)
as.data.frame(Maximum)

'Coefficients of Variation'<- sapply(CoastData44,function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
as.data.frame(`Coefficients of Variation`)

SumStats44<- rbind(Mean, `Standard Deviation`,`Coefficients of Variation`,Maximum, Minimum)
print(SumStats44)

# Compare min vs. max freq scatter plot

ESfreq.plot <- ggplot(data=EsData96, mapping=aes(x=Minimum.Frequency..kHz.,y=Maximum.Frequency..kHz.)) + 
  geom_point()
print(ESfreq.plot)

NICfreq.plot <- ggplot(data=NicData44, mapping=aes(x=Minimum.Frequency..kHz.,y=Maximum.Frequency..kHz.)) + 
  geom_point()
print(NICfreq.plot)

NicESfreq.plot <- ggplot()+
  geom_point(data=NicData44, mapping=aes(x=Minimum.Frequency..kHz.,y=Maximum.Frequency..kHz.),color="blue") + 
  geom_point(data=EsData96, mapping=aes(x=Minimum.Frequency..kHz.,y=Maximum.Frequency..kHz.),color="red")
print(NicESfreq.plot)

# Compare start and end Freq scatterplot

ESstart_endfreq.plot <- ggplot(data=EsData96, mapping=aes(x=Fundamental.frequency.Start..kHz.,y=Fundamental.frequency.End..kHz.)) + 
  geom_point()
print(ESfreq.plot)

NICstart_endfreq.plot <- ggplot(data=NicData44, mapping=aes(x=Fundamental.frequency.Start..kHz.,y=Fundamental.frequency.End..kHz.)) + 
  geom_point()
print(NICfreq.plot)

NicESstart_endfreq.plot <- ggplot()+
  geom_point(data=NicData44, mapping=aes(x=Fundamental.frequency.Start..kHz.,y=Fundamental.frequency.End..kHz.),color="blue") + 
  geom_point(data=EsData96, mapping=aes(x=Fundamental.frequency.Start..kHz.,y=Fundamental.frequency.End..kHz.),color="red")
print(NicESstart_endfreq.plot)
