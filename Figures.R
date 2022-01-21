# Load Packages
library(Rraven)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(stringr)

setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables")

# Import El Salvador data tables
rvn.ESdat <- imp_raven(path = "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/El Salvador", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# add population column
rvn.ESdat$Population<-"El Salvador"

# Import Nicaragua North Data Tables
rvn.NNdat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic North Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# Import Nicaragua South Data Tables
rvn.NSdat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic South Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# Import Nicaragua (total) tables
rvn.NICdat<-imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# add population column
rvn.NICdat$Population<-"Nicaragua"

# Import combined table for species-wide 
rvn.Species_dat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Species Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

rvn.Species_dat$Population<-"Species-Wide"

# Clean NS datatable
rvn.NSdat$`Peak Power Density (dB FS)`<- NULL
rvn.NSdat$contour<- NULL
rvn.NSdat$`Peak Power (dB)`<- NULL

# Clean NN datatable 
rvn.NNdat$`Peak Power Density (dB FS)`<- NULL
rvn.NNdat$`Peak Power (dB)`<- NULL

# Clean ES datatable
rvn.ESdat$`Peak Power (dB)`<- NULL

# Clean Nic datatable 
rvn.NICdat$`Peak Power Density (dB FS)`<- NULL
rvn.NICdat$contour<- NULL
rvn.NICdat$`Peak Power (dB)`<- NULL

# Clean species dat
rvn.Species_dat$`Peak Power Density (dB FS)`<- NULL
rvn.Species_dat$contour<- NULL
rvn.Species_dat$`Peak Power (dB)`<- NULL
rvn.Species_dat$'Selection'<- NULL
rvn.Species_dat$'sound.files'<- NULL

# Compare min vs max freq

ElSalvador <- ggplot(data=rvn.ESdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(ElSalvador)

NicaNorth <- ggplot(data=rvn.NNdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(NicaNorth)

NicaSouth <- ggplot(data=rvn.NSdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(NicaSouth)

Nicaragua <- ggplot(data=rvn.NICdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(Nicaragua)


# Compare Nic vs ES min vs max freq scatterplot
Combine <- ggplot()+
  geom_point(data=rvn.NICdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`),color="blue") + 
  geom_point(data=rvn.ESdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`),color="red")
print(Combine)

Species <- ggplot(data=rvn.Species_dat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(Species)

# Compare Nic vs ES Max freq
# Combine Nic, ES and Species data tables
SpeciesWide<-rbind(rvn.ESdat,rvn.NICdat)
HFreqBox<-ggplot(data=SpeciesWide, 
                 mapping=aes(x=Population,y=`High Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(HFreqBox)

# Compare Nic vs ES min freq
LFreqBox<-ggplot(data=SpeciesWide, 
                 mapping=aes(x=Population,y=`Low Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(LFreqBox)

# Compare Nic vs ES peak freq
PFBox<-ggplot(data=SpeciesWide, 
              mapping=aes(x=Population,y=`Peak Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(PFBox)

# Compare Nic vs ES center freq
CFBox<-ggplot(data=SpeciesWide, 
              mapping=aes(x=Population,y=`Center Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(CFBox)

# Compare Nic vs ES duration
DurBox<-ggplot(data=SpeciesWide, 
              mapping=aes(x=Population,y=`Delta Time (s)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(DurBox)

# compare Nic vs ES delta freq
DFBox<-ggplot(data=SpeciesWide, 
               mapping=aes(x=Population,y=`Delta Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(DFBox)

# Whistle type dataframe, species wide
WhistleCounts<- rvn.Species_dat$Contour
sum(str_count(WhistleCounts, 'upsweep'))
sum(str_count(WhistleCounts, 'downsweep'))
sum(str_count(WhistleCounts, 'convex'))
sum(str_count(WhistleCounts, 'concave'))
sum(str_count(WhistleCounts, 'constant'))
sum(str_count(WhistleCounts, 'sine'))

WhistleType<- c('upsweep', 'sine', 'concave', 'convex', 'constant', 'downsweep')
Count<- c(456, 92, 58, 50, 32, 19)
Contour.type<- data.frame(WhistleType, Count)

# Whistle type bargraph by count
Type.plot<- ggplot(Contour.type, aes(x=WhistleType,y=Count)) +
  geom_col(color='black', fill='royalblue')
print(Type.plot)

# Whistle type bar graph by percentage

Percent<- c(64.497878, 13.012729, 8.203678, 7.072136, 4.526167, 2.687411)

Percent.type<- data.frame(WhistleType, Percent)

Percent.plot<- ggplot(Percent.type, aes(x=WhistleType,y=Percent)) +
  geom_col(color='black', fill='royalblue')
print(Percent.plot)

# dataframe of Whistle types

Whistle.Type<- data.frame(WhistleType, Count, Percent)

# Whistle types by location
# El salvador
EScount<- rvn.ESdat$Contour
sum(str_count(EScount, 'upsweep'))
sum(str_count(EScount, 'downsweep'))
sum(str_count(EScount, 'convex'))
sum(str_count(EScount, 'concave'))
sum(str_count(EScount, 'constant'))
sum(str_count(EScount, 'sine'))

EStype<- c('upsweep', 'sine', 'concave', 'constant', 'convex', 'downsweep')
ESocc<- c(229, 38, 19, 17, 14, 5)
ESpercent<- c(71.118012, 11.801242, 5.900621, 5.279503, 4.347826, 1.552795)
ES.Contour.type<- data.frame(EStype, ESocc, ESpercent)

ES.type.plot<- ggplot(ES.Contour.type, aes(x=EStype,y=ESocc)) +
  geom_col(color='black', fill='royalblue')
print(ES.type.plot)

# Nicaragua
NICcount<- rvn.NICdat$Contour
sum(str_count(NICcount, 'upsweep'))
sum(str_count(NICcount, 'downsweep'))
sum(str_count(NICcount, 'convex'))
sum(str_count(NICcount, 'concave'))
sum(str_count(NICcount, 'constant'))
sum(str_count(NICcount, 'sine'))

NICtype<- c('upsweep', 'sine', 'concave', 'convex', 'constant', 'downsweep')
NICocc<- c(227, 54, 39, 36, 15, 14)
NICpercent<- c(58.961039, 14.025974, 10.129870, 9.350649, 3.896104, 3.636364)
NIC.Contour.type<- data.frame(NICtype, NICocc, NICpercent)
print(NIC.Contour.type)
NIC.type.plot<- ggplot(NIC.Contour.type, aes(x=NICtype,y=NICocc)) +
  geom_col(color='black', fill='royalblue')
print(NIC.type.plot)
