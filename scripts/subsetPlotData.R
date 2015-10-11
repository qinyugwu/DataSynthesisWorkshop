#PREPARE DATA TO PLAY WITH FOR ANALYSIS, CUTTING OUT COMPLICATIONS
#TO MAKE A TESTING DATASET
#ABJ 10/11/2015

#ISSUES - nearly all needs to be redone once the final data are prepared,
#we threw out a lot.

plotData <-read.csv ('Data/cleanData/plotDataClean.csv')
siteData <-read.csv ('Data/cleanData/siteDataClean.csv')

plotDatasub<-plotData[plotData$site %in% c("Churchill, MB","Canol Trail, NWT","Wolf Creek, YK")==FALSE,]

#extract only ONE SPECIES for now
myspp<-as.data.frame(unique(siteData[,c("site","sp.seeded")]))
myspp<-myspp[!duplicated(myspp$site),]
#this will be called exp.seedling.sp
plotDatasub<-merge(plotDatasub,myspp,by.x=c("site","exp.seedling.sp"),by.y=c("site","sp.seeded")) #!!!

#Unique plot names per plot
plotDatasub$uniquePlot<-paste(plotDatasub$site,plotDatasub$zone,plotDatasub$transect,plotDatasub$plot,sep="_")

#check this
tempor<-as.data.frame(table(plotDatasub$uniquePlot))
tempor[tempor$Freq>1,]
#CHURCHILL IS WEIRD! LEAVE OUT FOR NOW

plotDatasub$seedT<-0
plotDatasub$seedT[plotDatasub$treatment %in% c("SD","SS","PSS")]<-1 #need this to be per plot

plotDatasub$scarT<-0
plotDatasub$scarT[plotDatasub$treatment %in% c("SC","SS","PSS","PSC")]<-1 #per plot

#seeds added
unique(siteData[,c("site","seeds.per.plot")])
plotDatasub$seeds.per.plot<-as.numeric(siteData$seeds.per.plot[match(plotDatasub$site,siteData$site)]) #MAKE SURE TO FIX THIS PER SPECIES WHEN WE DO THIS FOR REAL!

#take only the first year seeded and only non-herbivory treatment for Davos
plotDatasub<-plotDatasub[c(plotDatasub$site=="Davos" & c(plotDatasub$start.year==2014 | plotDatasub$provenance=="low" | plotDatasub$herb.treat=="ex"))==FALSE,]

#AT only
plotDatasub<-plotDatasub[plotDatasub$zone=="AT",]


#list of unique plots with the associated site number also with scarT 

#make up germination rate data but replace this will real data later
germRate<-setNames(as.data.frame(cbind(c(seq(1,length(unique(plotDatasub$site)),by=1)),c(runif(length(unique(plotDatasub$site)),0.8,1)))),c("siteNum","germRate"))


write.csv(plotDatasub, 'data/subsets/plotDatasub.csv', row.names=F)