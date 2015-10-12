# Script to read raw data from Dropbox 10/6/2015 and convert to csv
# rerun ONLY when/if raw data is updated
# Script by SCE 10/8/2015
# assumes working directory is set to DataSynthesisWorkshop repo


#ISSUES - can't read the new plotData

library (xlsx)

#something in the 10/11/2015 file won't read, manually converted to csv
# siteData<-read.xlsx('Background/MASTER_GTREE_11Oct2015.xlsx',sheetName='SiteData',
#                     fileEncoding='latin1')
siteData<-read.csv('Background/MASTER_GTREE_11Oct2015_site.csv',
                                         fileEncoding='latin1')                    

#not always necessary but keeping these because sometimes Andrew's files read with blanks
siteData<-siteData[siteData$site!='',]
siteData<-siteData[!is.na(siteData$site),]

# 
# plotData<-read.xlsx('Background/MASTER_GTREE_11Oct2015.xlsx',sheetName='PlotData',
#                     fileEncoding='latin1')
plotData<-read.csv('Background/MASTER_GTREE_11Oct2015_plot.csv',
                   fileEncoding='latin1')  
plotData<-plotData[plotData$site!='',]
plotData<-plotData[!is.na(plotData$site),]

unique(plotData$nat.seedling.sp.y0)  # some weirdness here still

source('scripts/rem_specialChars.R')
siteData<-rem_specialChars(siteData, cols=names(siteData))
plotData<-rem_specialChars(plotData, cols=names(plotData))

write.csv (siteData, 'Data/siteData.csv', row.names=F, fileEncoding='UTF-8')
write.csv (plotData, 'Data/plotData.csv', row.names=F, fileEncoding='UTF-8')
