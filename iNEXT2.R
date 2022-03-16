#Code applying iNEXT analyses to pre- and during-covid whistle repertoires

#Motivation: understand how change in human activity influences communication

#Start date: February 16 2022 by S.Walmsley

#Last modified March 10 2022 by S.Walmsley 

#Required packages
library(data.table)
library(ggplot2)
library(ggthemes)
library(stringr)
library(iNEXT)
library(ggpubr)
library(viridis)
library(patchwork)
library(BSDA)


#For reproducibility
set.seed(1234)




# Prepare data ------------------------------------------------------------

dt <- data.table(read.csv('AllContours96.csv')) # load in .csv file

dt[,phase:=ifelse(grepl("COASTAL", name),"Coastal","Oceanic"),by=name] # use whistle names to create column of observational "treatments"

dt[,.N,by=phase] # 526 coastal whistles, 444 covid whistles

dt[,length(unique(category)),] # 238 categories in total 

dt[,length(unique(category)),by=phase] # 158 coastal categories, 152 oceanic categories
# this means that there are 80 categories that are distinctly oceanic and 86 that are distinctly coastal??

dt[,numWhistlesPhase:=.N,by=c('phase','category')] # add column with number of each whistle type (per phase, using combined ARTwarp classifications)




# Format data for iNEXT ---------------------------------------------------

cats <- dt[dt[ , .I[sample(.N,1)] , by = c('phase','category')]$V1] # sub-sample one row per category & phase 

coastal <- sort(cats[phase=="Coastal",c(numWhistlesPhase),], decreasing = TRUE) # extract and sort number of whistles of each category for control phase

oceanic <- sort(cats[phase=="Oceanic",c(numWhistlesPhase),], decreasing = TRUE) # extract and sort number of whistles of each category for control phase

combined <- list('Coastal' = coastal, 'Oceanic' = oceanic)




# Run iNEXT for levels of Q 0,1,2 -----------------------------------------

# Q = 0

q0 <- iNEXT(combined, q=0, datatype="abundance")

print(q0) # print iNEXT results summary
# $AsyEst: asymptotic diversity estimates along with related statistics.
#   Site    Diversity         Observed Estimator   s.e.     LCL     UCL
# 1 Coastal  Species richness  158.000   295.262 45.012 231.370 414.795
# 2 Coastal Shannon diversity  100.191   133.842  8.238 117.695 149.989
# 3 Coastal Simpson diversity   71.345    82.384  5.122  72.344  92.424
# 4 Oceanic  Species richness  152.000   291.921 47.361 225.374 418.825
# 5 Oceanic Shannon diversity  103.661   144.923  9.356 126.585 163.261
# 6 Oceanic Simpson diversity   73.503    87.887  6.670  74.814 100.961

q0$DataInfo$SC # sample coverage: 0.8557; 0.8358 

# T-test q=0
tsum.test(mean.x=295.262,   s.x=39.844, n.x=526,
          mean.y=291.921, s.y=42.301, n.y=444)
# p-value= 0.2085

# Q = 1

q1 <- iNEXT(combined, q=1, datatype="abundance")

print(q1) # print iNEXT results summary
# $AsyEst: asymptotic diversity estimates along with related statistics.
#   Site      Diversity       Observed Estimator   s.e.     LCL     UCL
# 1 Coastal  Species richness  158.000   295.262 45.012 231.370 414.795
# 2 Coastal Shannon diversity  100.191   133.842  7.701 118.748 148.936
# 3 Coastal Simpson diversity   71.345    82.384  5.014  72.555  92.212
# 4 Oceanic  Species richness  152.000   291.921 47.361 225.374 418.825
# 5 Oceanic Shannon diversity  103.661   144.923  9.798 125.719 164.127
# 6 Oceanic Simpson diversity   73.503    87.887  6.937  74.290 101.484

# T-test q=1
tsum.test(mean.x=133.842,   s.x=5.031, n.x=526,
          mean.y=144.923, s.y=6.401, n.y=444)
# p-value< 2.2e-16

# Q = 2

q2 <- iNEXT(combined, q=2, datatype="abundance")

print(q2) # print iNEXT results summary

# $AsyEst: asymptotic diversity estimates along with related statistics.
# Site      Diversity         Observed Estimator   s.e.     LCL     UCL
# 1 Coastal  Species richness  158.000   295.262 45.012 231.370 414.795
# 2 Coastal Shannon diversity  100.191   133.842  7.902 118.354 149.330
# 3 Coastal Simpson diversity   71.345    82.384  4.513  73.537  91.230
# 4 Oceanic  Species richness  152.000   291.921 47.361 225.374 418.825
# 5 Oceanic Shannon diversity  103.661   144.923  9.579 126.148 163.698
# 6 Oceanic Simpson diversity   73.503    87.887  6.647  74.859 100.916

# T-test q=2
tsum.test(mean.x=82.384,   s.x=2.949, n.x=526,
          mean.y=87.887, s.y=4.343, n.y=444)
# p-value < 2.2e-16

# Estimate diversity at cMax ----------------------------------------------

coastalCoverage <- max(q0$iNextEst$Coastal$SC) #extract coverage at 2x observed sample size for coastal data 

oceanCoverage <- max(q0$iNextEst$Oceanic$SC) #extract coverage at 2x observed sample size for oceanic data

# (NOTE the above two lines work because maximum value will automatically be 2x reference sample)

cMax <- min(coastalCoverage, oceanCoverage) #lesser of the two above values is cMax, recommended coverage level for comparisons

SC <- estimateD(combined, datatype = "abundance", base="coverage", level=0.90256, conf=0.95) #estimated diversity for levels of Q at 90.256% coverage

#   site    m     method      order SC      qD  qD.LCL  qD.UCL
#1 Coastal 899 extrapolated     0 0.903 202.553 181.581 224.525
#2 Coastal 899 extrapolated     1 0.903 113.159 101.382 124.935
#3 Coastal 899 extrapolated     2 0.903  75.545  66.912  84.177
#4 Oceanic 888 extrapolated     0 0.903 208.853 184.099 233.607
#5 Oceanic 888 extrapolated     1 0.903 123.610 109.961 137.260
#6 Oceanic 888 extrapolated     2 0.903  80.054  69.358  90.751

# T-tests for significance

# Richness
tsum.test(mean.x=202.553,   s.x=7.324, n.x=526,
          mean.y=208.853, s.y=8.251, n.y=444) # p-value = < 2.2e-16
# Shannon
tsum.test(mean.x=113.293,   s.x=3.925, n.x=526,
          mean.y=123.737, s.y=4.55, n.y=444) # p-value < 2.2e-16
# Simpson
tsum.test(mean.x=75.580,   s.x=2.877, n.x=526,
          mean.y=80.086, s.y=3.566, n.y=444) # p-value = < 2.2e-16

# Sample-size-based RE-EX curves ------------------------------------------

g1 <- ggiNEXT(q0, type=1) +   
  labs(x="Number of Contours", y="Whistle diversity", title="q=0") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="none") +
  theme(legend.title = element_text(size = 10,
                                    family= "serif")) + 
  theme(legend.text = element_text(size = 10,
                                   family="serif"))+
  xlim(0,1150)
g11 <- g1+ scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))
g11

g2 <- ggiNEXT(q1, type=1) +   
  labs(x="Number of Contours", y="Whistle diversity", title="q=1") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="none") +
  theme(legend.title = element_text(size = 10,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 10,
                                   family="serif"))+
  xlim(0,1150)

g22 <- g2+ scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))
g22

g3 <- ggiNEXT(q2, type=1) +   
  labs(x="Number of Contours", y="Whistle diversity", title="q=2") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.title = element_text(size = 10,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 10,
                                   family="serif"))+
  xlim(0,1150)

g33 <- g3 + scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))

( g11 | g22 | g33)
# ggarrange(g1,g2,g3,nrow=1)


# Coverage-based RE-EX curves ---------------------------------------------

g4 <- ggiNEXT(q0, type=3) +
  labs(x="Sample Coverage", y="Whistle diversity", title="q=0") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="none") +
  theme(legend.title = element_text(size = 10,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 10,
                                   family="serif"))+
  xlim(0,1)
g44 <- g4+ scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))

g5 <- ggiNEXT(q1, type=3) + 
  labs(x="Sample Coverage", y="Whistle diversity", title="q=1") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="none") +
  theme(legend.title = element_text(size = 10,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 10,
                                   family="serif"))+
  xlim(0,1)
g55 <- g5+ scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))

g6 <- ggiNEXT(q2, type=3) + 
  labs(x="Sample Coverage", y="Whistle diversity", title="q=2") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="right") +
  theme(legend.title = element_text(size = 10,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 10,
                                   family="serif"))+
  xlim(0,1)
g66 <- g6+ scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))

#facet 
( g44 | g55 | g66)

# Sample Completeness Curve
g7 <- ggiNEXT(q2, type=2) +   
  labs(x="Number of Contours") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif")) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  xlim(0,1500)
g77 <- g7+ scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))
g77

# Species Richness Curve
g <- ggiNEXT(q0, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)
g

g8 <- g + scale_colour_manual(values=c("#DC3220", "#005AB5")) +
  scale_fill_manual(values=c("#DC3220", "#005AB5"))+ 
  labs(x="Number of Contours", y="Whistle Diversity") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif")) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14)) +
  xlim(0,1500)
g8
