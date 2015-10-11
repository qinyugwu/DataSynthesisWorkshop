# Script to read raw data from Dropbox 10/6/2015 and convert to csv
# rerun ONLY when/if raw data is updated
# Script by SCE 10/8/2015
# assumes working directory is set to DataSynthesisWorkshop repo


library (xlsx)
siteData<-read.xlsx('Background/GTREE_Master_30Sept2015.xlsx',sheetName='SiteData')
plotData<-read.xlsx('Background/GTREE_Master_30Sept2015.xlsx',sheetName='PlotData')


source('scripts/rem_specialChars.R')
siteData<-rem_specialChars(siteData, cols=names(siteData))
plotData<-rem_specialChars(plotData, cols=names(plotData))

write.csv (siteData, 'Data/siteData.csv', row.names=F, fileEncoding='UTF-8')
write.csv (plotData, 'Data/plotData.csv', row.names=F, fileEncoding='UTF-8')
