# Exploring Raven Data 

# Load Packages
library(Rraven)
library(ggplot2)
library(ggthemes)
library(patchwork)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables")


# Import El Salvador data tables
rvn.ESdat <- imp_raven(path = "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/El Salvador", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")
print(rvn.ESdat)


rvn.ESdat$Population<-"El Salvador"
print(rvn.ESdat)

# Compare min vs max freq

ElSalvador <- ggplot(data=rvn.ESdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(ElSalvador)



# Import Nicaragua North Data Tables
rvn.NNdat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic North Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# Compare min vs max freq

NicaNorth <- ggplot(data=rvn.NNdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(NicaNorth)


# Import Nicaragua South Data Tables
rvn.NSdat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic South Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# Compare min vs max freq

NicaSouth <- ggplot(data=rvn.NSdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(NicaSouth)

# Import Nicaragua (total) tables
rvn.NICdat<-imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

rvn.NICdat$Population<-"Nicaragua"

# Compare min vs max freq

Nicaragua <- ggplot(data=rvn.NICdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(Nicaragua)

# Import combined table for species-wide 
rvn.Species_dat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Species Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

rvn.Species_dat$Population<-"Species-Wide"

# compare min vs max freq
Species_plot<- qplot(x=rvn.Species_dat$`Low Freq (Hz)`,y=rvn.Species_dat$`High Freq (Hz)`,geom=c("point"))
print(Species_plot)


# combine ElSalv and Nica plots
Combine <- ggplot()+
  geom_point(data=rvn.NICdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`),color="blue") + 
  geom_point(data=rvn.ESdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`),color="red")

print(Combine)


# compare NS vs NN
compare <- ggplot()+
  geom_point(data=rvn.NSdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`),color="blue") + 
  geom_point(data=rvn.NNdat, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`),color="green")

print(compare)

# Cleaning dataframes
rvn.NICdat$Contour<- NULL
rvn.NICdat$`Inband Power (dB)`<- NULL
rvn.NICdat$Column<- NULL
colnames(rvn.NICdat)[14]<- "Contour"



rvn.NSdat$contour<- NULL
rvn.NSdat$`Inband Power (dB)`<- NULL

rvn.Species_dat$contour<- NULL
rvn.Species_dat$`Inband Power (dB)`<- NULL


# Combine Nic, ES and Species data tables
SpeciesWide<-rbind(rvn.ESdat,rvn.NICdat)

# Max Freq box plot
HFreqBox<-ggplot(data=SpeciesWide, 
                 mapping=aes(x=Population,y=`High Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(HFreqBox)

# Peak Freq Box plot
PFBox<-ggplot(data=SpeciesWide, 
              mapping=aes(x=Population,y=`Peak Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(PFBox)


# Low Freq box
LFreqBox<-ggplot(data=SpeciesWide, 
                 mapping=aes(x=Population,y=`Low Freq (Hz)`,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(LFreqBox)




