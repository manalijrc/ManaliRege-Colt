# Tables
library(dplyr)
setwd("Z:/Manali_Stenella/Analysis")

# Parameter Summary Stats Table
# Load Data
Coastal<- read.csv('Coastal_Measurements_44.csv')
Coastal$Population<- NULL
Coastal$Recording<- NULL
Coastal$Ecotype<- NULL

Oceanic.44kHz<- read.csv('Oceanic_Measurements_44.csv')
Oceanic.44kHz$Recording<- NULL
Oceanic.44kHz$Ecotype<- NULL

Oceanic.96kHz<- read.csv('Oceanic_Measurements_96.csv')
Oceanic.96kHz$Recording<- NULL
Oceanic.96kHz$Ecotype<- NULL

# Coastal stats
Mean<- sapply(Coastal, mean)


sd<- sapply(Coastal,sd)


CoV<- sapply(Coastal,function(x) sd(x,na.rm=T)/mean(x,na.rm=T)*100)


Maximum<- sapply(Coastal, max)


Minimum<- sapply(Coastal, min)


SummaryTable<- data.frame(Mean, sd, CoV, Maximum, Minimum)
CoastalSummary<-as.data.frame(t(SummaryTable))

#Oceanic44 stats

Mean<- sapply(Oceanic.44kHz, mean)


sd<- sapply(Oceanic.44kHz,sd)


CoV<- sapply(Oceanic.44kHz,function(x) sd(x,na.rm=T)/mean(x,na.rm=T)*100)


Maximum<- sapply(Oceanic.44kHz, max)


Minimum<- sapply(Oceanic.44kHz, min)


O44SummaryTable<- data.frame(Mean, sd, CoV, Maximum, Minimum)
Oceanic.44khz.Summary<-as.data.frame(t(O44SummaryTable))

# Oceanic96 stats

Mean<- sapply(Oceanic.96kHz, mean)


sd<- sapply(Oceanic.96kHz,sd)


CoV<- sapply(Oceanic.96kHz,function(x) sd(x,na.rm=T)/mean(x,na.rm=T)*100)


Maximum<- sapply(Oceanic.96kHz, max)


Minimum<- sapply(Oceanic.96kHz, min)


O96SummaryTable<- data.frame(Mean, sd, CoV, Maximum, Minimum)

Oceanic.96khz.Summary<-as.data.frame(t(O96SummaryTable))


# adjusting column names

colnames(CoastalSummary)<- c('Max Freq (kHz)', 'Min Freq (kHz)', 'Fund Freq Start (kHz)', 'Fund Freq End (kHz)', 'Delta Freq (kHz)', 'Peak Freq (kHz)', 'Duration (s)')

colnames(Oceanic.96khz.Summary)<- c('Max Freq (kHz)', 'Min Freq (kHz)', 'Fund Freq Start (kHz)', 'Fund Freq End (kHz)', 'Delta Freq (kHz)', 'Peak Freq (kHz)', 'Duration (s)')

colnames(Oceanic.44khz.Summary)<- c('Max Freq (kHz)', 'Min Freq (kHz)', 'Fund Freq Start (kHz)', 'Fund Freq End (kHz)', 'Delta Freq (kHz)', 'Peak Freq (kHz)', 'Duration (s)')


print(CoastalSummary)

print(Oceanic.44khz.Summary)

print(Oceanic.96khz.Summary)

SumStats<- rbind(CoastalSummary,Oceanic.44khz.Summary,Oceanic.96khz.Summary)

write.csv(SumStats,"C:/Manali" )





knitr::kable(CoastalSummary,format='html', caption='Coastal Ecotype (sampling rate = 44kHz') %>%
  kable_classic(full_width=FALSE, html_font = 'Cambria', font_size=16)

knitr::kable(Oceanic.44khz.Summary,format='html', caption='Oceanic Ecotype (sampling rate = 44 kHz') %>%
  kable_classic(full_width=FALSE, html_font = 'Cambria', font_size=16)

knitr::kable(Oceanic.96khz.Summary,format='html', caption='Oceanic Ecotype (sampling rate = 96 kHz') %>%
  kable_classic(full_width=FALSE, html_font = 'Cambria', font_size=16)






# ARTwarp Table