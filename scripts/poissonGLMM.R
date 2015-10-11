library (lme4)
#write out files
plotData <-read.csv ('Data/cleanData/plotDataClean.csv', row.names=F)
write.csv (siteData, 'Data/cleanData/siteDataClean.csv', row.names=F)
