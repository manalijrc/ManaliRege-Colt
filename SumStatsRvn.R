# Load Packages
library(Rraven)
library(dplyr)


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
rvn.Species_dat$'Channel'<- NULL
rvn.Species_dat$'View'<- NULL
rvn.Species_dat$'selec.file'<- NULL
rvn.Species_dat$'sound.files'<- NULL
rvn.Species_dat$'Population' <- NULL
rvn.Species_dat$Contour <- NULL
rvn.Species_dat$`Begin Time (s)`<- NULL
rvn.Species_dat$`End Time (s)`<- NULL


# create personalized summary stats table
'Mean'<- sapply(rvn.Species_dat, mean)
as.data.frame(Mean)

'Standard Deviation'<- sapply(rvn.Species_dat, sd)
as.data.frame(`Standard Deviation`)

'Minimum'<- sapply(rvn.Species_dat, min)
as.data.frame(Minimum)

'Maximum'<- sapply(rvn.Species_dat, max)
as.data.frame(Maximum)

'Coefficients of Variation'<- sapply(rvn.Species_dat,function(x) sd(x, na.rm=T) / mean(x, na.rm=T) * 100)
as.data.frame(`Coefficients of Variation`)

SumStats<- rbind(Mean, `Standard Deviation`,`Coefficients of Variation`,Maximum, Minimum)
SumStats
