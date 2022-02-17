# Diversity Estimator for Dolphin Repertoire
# 2/14/2022
# MRC
#--------------------------------------------------------------------------

# install iNEXT package from CRAN
#install.packages("iNEXT")
#install.packages("reactable")
#install.packages("ecolTest")
#install.packages("BSDA")
# import packages
library(iNEXT)
library(ggplot2)
library(ggthemes)
library(readr)
library(reactable)
library(ecolTest)
library(BSDA)
setwd("/Volumes/Seagate Backup Plus Drive/StenellaProj/Analysis/ARTwarp")
# import data sets -----------------------------------------------------------
# Coastal <- read_csv("96_200_100CoastalSubsample.csv")
# View(Coastal)

# ggplot(data=Coastal, aes(x=Coastal$`Category`, y= Coastal$`Number of Whistles`)) +
  # geom_bar(stat= "identity")
  

Oceanic <- read_csv("96_200_100Oceanic44.csv")
View(Oceanic)

# subset based on q=0 sample coverage
sc_coast <- read_csv("96_200_100CoastaliNEXT.csv")
View(sc_coast)

# subset based on q=1 sample coverage
# My sample size for each hill number ended up being the same, but it if it's
# not for you, then you'd upload that sample size here

# subset based on q=2 sample coverage
# Same as above


# restructure data frame into a list ------------------------------------------
# coast <- list("Coastal" = Coastal$`Number of Whistles`)
# coast

oc <- list("Oceanic" = Oceanic$`Number of Whistles`)
oc

sc_coast <- list("Coastal" = sc_coast$`Number of Whistles`)
sc_coast

# repertoire <- c(coast,oc)
# str(repertoire)

sample_coverage <- c(oc,sc_coast)
sample_coverage

# Sample coverage q=0 -------------------------------------------------------
m <- c(386, 400, 444, 500, 550)
iNEXT(repertoire, q=2, datatype="abundance", size=m) # change "q=" for each hill number
# Q=0: At 86.3% sample coverage, coast=386, dur=444
# Q=1: At 86.3% sample coverage, pre=386, dur=444
# Q=2: At 86.3% sample coverage, pre=386, dur=444


out <- iNEXT(repertoire, q=c(0, 1, 2), datatype="abundance", endpoint=600)


# This curve plots the diversity estimates with 
# confidence intervals (if se=TRUE) as a function of sample coverage up to the 
# maximum coverage obtained from the maximum (double the reference sample size)
g <- ggiNEXT(out, type=2)
g <- g +
  ggtitle("Sample Completeness Curve") +
  labs(x="Number of Whistles", y="Sample Coverage", ) +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif"))
g



# Sample coverage and whistle diversity by site
g1 <- ggiNEXT(out, type=3, facet.var="site")
g1 <- g1 +
  labs(x="Sample Coverage", y="Whistle Diversity", ) +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif"))
g1

# Sample coverage and whistle diversity by hill number
g2 <- ggiNEXT(out, type=3, facet.var="order")
g2 <- g2 +
  labs(x="Sample Coverage", y="Whistle Diversity", ) +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif"))
g2


# STATS BASED ON q=0 sample coverage ------------------------------------
iNEXT(sample_coverage, q=0, datatype="abundance")
# $AsyEst: asymptotic diversity estimates along with related statistics.
#     Site         Diversity Observed Estimator
# 1 Oceanic  Species richness  130.000   246.019
# 2 Oceanic Shannon diversity   84.438   111.992
# 3 Oceanic Simpson diversity   59.993    69.209
# 4 Coastal  Species richness  107.000   159.228
# 5 Coastal Shannon diversity   62.653    78.789
# 6 Coastal Simpson diversity   40.709    45.391
#     s.e.     LCL     UCL
# 1 42.895 187.524 363.998
# 2  6.290  99.664 124.320
# 3  5.195  59.993  79.391
# 4 20.088 132.220 215.161
# 5  4.185  70.586  86.992
# 6  3.709  40.709  52.661

# Statistical tests --------------------------------------------------------
# ChaoRichness: estimation of species richness (q=0)
ChaoRichness(sample_coverage, datatype = "abundance", conf = 0.95)

#           Observed Estimator Est_s.e. 95% Lower 
# Oceanic      130   246.019   42.895   187.524
# Coastal      107   159.228   20.088   132.220
# 95% Upper
# Oceanic   363.998
# Coastal   215.161

# Test for significance
tsum.test(mean.x=246.019,   s.x=42.895, n.x=444,
          mean.y=159.228, s.y=20.088, n.y=386)
# t = 38.099, df = 647.3, p-value = 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  82.31774 91.26426
# sample estimates:
#   mean of x mean of y 
#       246.019   159.228 


# ChaoEntropy: estimation of shannon diversity (q=1)
ChaoShannon(sample_coverage, datatype = "abundance", 
            transform = FALSE, conf = 0.95, B = 1000)

#           Observed Estimator Est_s.e 95% Lower
# Oceanic    4.436     4.718   0.069     4.583
# Coastal    4.138     4.367   0.072     4.226
# 95% Upper
# Oceanic     4.853
# Coastal     4.508

# Hutcheson T-test to test for significance (just for shannon)
Hutcheson_t_test(x=sample_coverage$Oceanic,
                 y=sample_coverage$Coastal)
# Hutcheson t-statistic = 4.4342, df = 773.88, p-value = 1.058e-05

# alternative hypothesis: true difference in H' is not equal to 0
# sample estimates:
#     x        y 
# 4.436020 4.137613


# ChaoSimpson: estimation of simpson diversity (q=2)
EstSimpson(sample_coverage, datatype = "abundance", transform = FALSE,
           conf = 0.95, B = 1000)

#           Observed Estimator Est_s.e. 95% Lower
# Oceanic    0.983     0.986    0.001     0.983
# Coastal    0.975     0.978    0.002     0.975
# 95% Upper
# Oceanic     0.988
# Coastal     0.982


# test for significance
tsum.test(mean.x=0.983,   s.x=0.001, n.x=444,
          mean.y=0.975, s.y=0.002, n.y=386)
# t = 71.227, df = 548.04, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.007779377 0.008220623
# sample estimates:
#   mean of x mean of y 
#         0.983     0.975 


# iNEXT graphics --------------------------------------------------------------
# the following commands display the sample species abundances and 
# run the iNEXT() function for q=0.
x <- iNEXT(sample_coverage, q=0, se=TRUE, conf=0.95, nboot=1000, datatype="abundance")

# Built in graphics displays
# Abundance rarefaction curve
g <- ggiNEXT(x, type=1, se=TRUE, facet.var="none", color.var="site", grey=FALSE)
g3 <- g +
  labs(x="Sampling Effort", y="Whistle Diversity") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif")) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.title.x = element_text(size = 14))
g3


# Graphic incorporating hill numbers 0,1,2
# Hill q=0: diversity of all species. the abundances of individual species 
# do not contribute to the sum. Rather, only presences are counted.
# Hill q=1: diversity of "typical" species. (Shannon index)
# Hill q=2: diversity of dominant species
out <- iNEXT(sample_coverage, q=c(0, 1, 2), datatype="abundance", endpoint=800,
             se=TRUE, conf=0.95, nboot=1000)

# Sample-size-based R/E curves, separating plots by "site"
g <- ggiNEXT(out, type=1, facet.var="site")
g4 <- g +
  labs(fill='Hill Number') +
  labs(x="Sampling Effort", y="Whistle diversity") +
  theme_clean(base_size=14,
              base_family="serif") +
  theme(legend.position="bottom") +
  theme(legend.title = element_text(size = 12,
                                    family= "serif")) +
  theme(legend.text = element_text(size = 12,
                                   family="serif"))
g4



