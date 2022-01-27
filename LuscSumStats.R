# set up
library(dplyr)
library(ggplot2)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Analysis")

# Import Nicaragua Data
NicData44 <- read.csv('Nic_measurements44.csv')
NicData96 <- read.csv('Nic_measurements96.csv')


NicData44$Recording<- NULL

NicData96$Recording<- NULL

# Import El Salvador Data
EsData96 <- read.csv('ES_measurements96.csv')

EsData96$Recording<- NULL

# Import Species Data
CoastData96<- read.csv('Measurements_96.csv')
CoastData44<- read.csv('Measurements_44.csv')


CoastData44$Recording<- NULL

CoastData96$Recording<- NULL

# Compare Nic and ES parameters in box plots
# Max Freq
MaxFreqBox.plot<-ggplot(data=CoastData44, 
                 mapping=aes(x=Population,y=Maximum.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(MaxFreqBox.plot)

t.test(NicData44$Maximum.Frequency..kHz.,EsData96$Maximum.Frequency..kHz.)

# Min Freq
MinFreqBox.plot<-ggplot(data=CoastData44, 
                        mapping=aes(x=Population,y=Minimum.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(MinFreqBox.plot)

t.test(NicData44$Minimum.Frequency..kHz.,EsData96$Minimum.Frequency..kHz.)

# Peak Freq
PFreqBox.plot<-ggplot(data=CoastData44, 
                        mapping=aes(x=Population,y=Peak.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(PFreqBox.plot)

t.test(NicData44$Peak.Frequency..kHz.,EsData96$Peak.Frequency..kHz.)


# Duration
DurBox.plot<-ggplot(data=CoastData44, 
                      mapping=aes(x=Population,y=Duration..s.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(DurBox.plot)

t.test(NicData44$Duration..s.,EsData96$Duration..s.)

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
