# Load Packages
library(Rraven)
library(ggplot2)
library(ggthemes)
library(patchwork)
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
rvn.NICdat$`Peak Power Density (dB FS)`<- NULL
rvn.NICdat$contour<- NULL
rvn.NICdat$`Peak Power (dB)`<- NULL