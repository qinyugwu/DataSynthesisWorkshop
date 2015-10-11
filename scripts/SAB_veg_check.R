#Set working directory
setwd("~/DataSynthesisWorkshop")

#Load data
plotData <- read.csv("Data/plotData.csv", header = TRUE)
plotData2<-plotData

#Load libraries
library(ggplot2)
library(plyr)

#Define functions
minfunction<-function(x){
  min(x, na.rm=T)
}
maxfunction<-function(x){
  max(x, na.rm=T)
}

#Check that total veg cover is always 100
unique(plotData2$total)
#It's not! 

#make a summary table of those 
veg_problems <- plotData2[plotData2$total != 100, ] 
veg_problems <- veg_problems[veg_problems$site != "Canol Trail, NWT", ]
veg_problems <- veg_problems[veg_problems$site != "Churchill, MB", ]
veg_problems <- veg_problems[veg_problems$site != "Wolf Creek, YK", ]
unique(veg_problems$site)

#Craigieburn T1 P8 : mineral should be 29 instead of 229
#"Canol Trail, NWT", "Wolf Creek, YK" and "Churchill, MB": category "other" = 100 in all cases: any details?
#Cover > 100%: overlap between veg layers (Printers, Mt Hotham)
#Cover < 100% ??? e.g. Davos (bare ground/rock/litter?)


BPnon.vascular <- boxplot(plotData2$non.vascular ~ plotData2$site)
BPmoss <- boxplot(plotData2$moss ~ plotData2$site)
BPlichen <- boxplot(plotData2$lichen ~ plotData2$site)
BPmineral <- boxplot(plotData2$mineral ~ plotData2$site)
BPorganic <- boxplot(plotData2$organic ~ plotData2$site)
BProck <- boxplot(plotData2$rock ~ plotData2$site)
BPshrub <- boxplot(plotData2$shrub ~ plotData2$site)
BPgraminoid <- boxplot(plotData2$graminoid ~ plotData2$site)
BPlitter <- boxplot(plotData2$litter ~ plotData2$site)
BPexcrement <- boxplot(plotData2$excrement ~ plotData2$site)
BPother <- boxplot(plotData2$other ~ plotData2$site)
BPtotal <- boxplot(plotData2$total ~ plotData2$site)

