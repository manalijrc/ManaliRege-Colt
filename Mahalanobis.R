library(Rraven)
library(ggplot2)
library(ggthemes)
library(patchwork)
library(TeachingDemos)
setwd("/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables")

# Import El Salvador data tables
rvn.ESdat <- imp_raven(path = "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/El Salvador", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# add population column
rvn.ESdat$Population<-"El Salvador"

# Import Nicaragua (total) tables
rvn.NICdat<-imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Nic Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")

# add population column
rvn.NICdat$Population<-"Nicaragua"

# Import combined table for species-wide 
rvn.Species_dat <- imp_raven(path= "/Volumes/Seagate Backup Plus Drive/Stenella Proj/Raven Data Tables/Species Tables", all.data=TRUE, only.spectro.view = TRUE, name.from.file = TRUE, ext.case="upper")



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
rvn.Species_dat$Selection<- NULL
rvn.Species_dat$View<- NULL
rvn.Species_dat$Channel<- NULL
rvn.Species_dat$`Begin Time (s)`<- NULL
rvn.Species_dat$`End Time (s)`<- NULL
rvn.Species_dat$Contour<- NULL
rvn.Species_dat$selec.file<- NULL
rvn.Species_dat$sound.files<- NULL

# Species Mahalanobis Distance

# Finding distances

species.distances <- mahalanobis(x= rvn.Species_dat, center=colMeans(rvn.Species_dat), cov=cov(rvn.Species_dat))

rvn.Species_dat$Mahalanobis<- mahalanobis(x= rvn.Species_dat, center=colMeans(rvn.Species_dat), cov=cov(rvn.Species_dat))

# create p-values

rvn.Species_dat$pvalue<- pchisq(rvn.Species_dat$Mahalanobis, df=6, lower.tail = FALSE)

outliers<- rvn.Species_dat[rvn.Species_dat$pvalue< 0.001,]

# explore outliers

outliers.plot <- ggplot(data=outliers, mapping=aes(x=`Low Freq (Hz)`,y=`High Freq (Hz)`)) + 
  geom_point()
print(outliers.plot)

LFoutliers.hist<- qplot(x=outliers$`Low Freq (Hz)`)
print(LFoutliers.hist)

HFoutliers.hist<- qplot(x=outliers$`High Freq (Hz)`)
print(HFoutliers.hist)

PFoutliers.hist<- qplot(x=outliers$`Peak Freq (Hz)`)
print(PFoutliers.hist)

CFoutliers.hist<- qplot(x=outliers$`Center Freq (Hz)`)
print(CFoutliers.hist)

DFoutliers.hist<- qplot(x=outliers$`Delta Freq (Hz)`)
print(DFoutliers.hist)

PFCoutliers.hist<- qplot(x=outliers$`PFC Num Inf Pts`)
print(PFCoutliers.hist)

(LFoutliers.hist|HFoutliers.hist)/(PFoutliers.hist|CFoutliers.hist)/(DFoutliers.hist|PFCoutliers.hist)

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

