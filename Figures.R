# Load Packages
library(Rraven)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(stringr)
library(tinytex)
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

WhistleType<- c('upsweep', 'downsweep', 'convex', 'concave', 'constant', 'sine')
Count<- c(456, 19, 50, 58, 32, 92)
Contour.type<- data.frame(WhistleType, Count)

# Whistle type bargraph by count
Type.plot<- ggplot(Contour.type, aes(x=WhistleType,y=Count)) +
  geom_col(color='black', fill='royalblue')
print(Type.plot)

# Whistle type bar graph by percentage

Percent<- c(64.497878, 2.687411, 7.072136, 8.203678, 4.526167, 13.012729)

Percent.type<- data.frame(WhistleType, Percent)

Percent.plot<- ggplot(Percent.type, aes(x=WhistleType,y=Percent)) +
  geom_col(color='black', fill='royalblue')
print(Percent.plot)

# dataframe of Whistle types

Whistle.Type<- data.frame(WhistleType, Count, Percent)


