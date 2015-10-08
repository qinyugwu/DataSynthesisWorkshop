# Script to read raw data from Dropbox 10/6/2015 and convert to csv
# rerun ONLY when/if raw data is updated
# Script by SCE 10/8/2015
# assumes working directory is set to DataSynthesisWorkshop repo


library (xlsx)
siteData<-read.xlsx('Background/GTREE_Master_30Sept2015.xlsx',sheetName='SiteData')
plotData<-read.xlsx('Background/GTREE_Master_30Sept2015.xlsx',sheetName='PlotData')

write.csv (siteData, 'Data/siteData.csv', row.names=F)
write.csv (plotData, 'Data/siteData.csv', row.names=F)
