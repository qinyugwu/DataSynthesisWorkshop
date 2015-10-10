# Script to cleanup csvs 
# rerun ONLY when/if csv export of excel is updated
# assumes working directory is set to DataSynthesisWorkshop repo
# Script by SCE 10/8/2015

siteData<-read.csv('Data/siteData.csv', fileEncoding='UTF-8',stringsAsFactors=F,
                   strip.white=T)
plotData<-read.csv('Data/plotData.csv', fileEncoding='UTF-8',stringsAsFactors=F,
                   strip.white=T)

#Convert all genus species to the same format

sort(unique(siteData$sp.seeded)) #site sheet is good

sort(unique(plotData$exp.seedling.sp))
plotData$exp.seedling.sp<-gsub("."," ",plotData$exp.seedling.sp,fixed=TRUE)
plotData$exp.seedling.sp<-gsub("_"," ",plotData$exp.seedling.sp,fixed=TRUE)

#cleanup the dates
#checking against original files, these are an excel problem
"41918"   #10/6/2014
"41919"   #10/7/2014
plotData$date.seeded[plotData$date.seeded=="41918"]<-"20141006"
plotData$date.seeded[plotData$date.seeded=="41919"]<-"20141007"

plotData$date.seeded[plotData$date.seeded=='22.5.2014']<-'20140522'
plotData$date.seeded[plotData$date.seeded=='3.6.2014']<-'20140306'
plotData$date.seeded[plotData$date.seeded=='25.6.2014']<-'20140625'
plotData$date.seeded[plotData$date.seeded=='16.7.2013']<-'20130716'


plotData$date.counted[plotData$]
"2015 08 29"
unique (plotData$date.counted)

#check start year against date




#convert zones to standard codes desynonymizing
sort(unique(siteData$zone)) #site codes are fine

sort(unique(plotData$zone))
plotData$zone[plotData$zone=="Alpine" & !is.na(plotData$zone)]<-"AT"
plotData$zone[plotData$zone=="Forest" & !is.na(plotData$zone)]<-"F"
plotData$zone[plotData$zone=="Treeline" & !is.na(plotData$zone)]<-"T"
plotData$zone[plotData$zone=="FT" & !is.na(plotData$zone)]<-"T" #???????????????? (sites are TexasCreek and Blowdown)

#change CTL to CN

plotData$treatment[plotData$treatment=="CTL" & !is.na(plotData$treatment)]<-"CN"

#greater than signs in the organic depth (also in "organic"), no data instead of NA in organic depth

siteData$closest.seed.tree[siteData$closest.seed.tree==">500" & !is.na(siteData$closest.seed.tree)] # <-????????????
siteData$closest.stand.tree[siteData$closest.stand.tree==">200m" & !is.na(siteData$closest.stand.tree)] # <-????????????

plotData$organic[plotData$organic=="<1" & !is.na(plotData$organic)]<-"0.5"

sort(unique(plotData$organic.depth))
plotData$organic.depth[plotData$organic.depth==">30" & !is.na(plotData$organic.depth)]<-50 #?????????? this is apparently very deep - Becca says probably 50 and definitely more than 50 (max value of other sites is 50)
plotData$organic.depth[plotData$organic.depth==">5" & !is.na(plotData$organic.depth)] #<- ?????????
plotData$organic.depth[plotData$organic.depth=="no data" & !is.na(plotData$organic.depth)]<-NA

plotData<-within(plotData,organic.depth<-as.numeric(organic.depth))
plotData<-within(plotData,organic<-as.numeric(organic))

#check to make sure all fields that should be numeric are coded as such

str(siteData)
str(plotData)

# make a unique plot column - CHECK WITH ANDREW WHAT UNIQUE NAMES ARE

#plotData$unique.plot<-paste(plotData$site,plotData$plot,sep="_")

# closest.seed.tree and closest.stand.tree have questions marks

sort(unique(siteData$closest.seed.tree))

siteData$closest.seed.tree[siteData$closest.seed.tree=="?" & !is.na(siteData$closest.seed.tree)]<-NA
siteData$closest.stand.tree[siteData$closest.stand.tree=="?" & !is.na(siteData$closest.stand.tree)]<-NA

siteData<-within(siteData,closest.seed.tree<-as.numeric(closest.seed.tree))
siteData<-within(siteData,closest.stand.tree<-as.numeric(closest.stand.tree))

#make sure the site join works so there one and only row row of site data per
# site within the plot data.



# other column has a lot of columns and then 'herbfield'
                   
plotData$other[plotData$other=="herbfield" & !is.na(plotData$other)] #<-???????????


# natsp.y0 has a '.' in it in many rows - should this be NA or something else

plotData$nat.seedling.sp.y0[plotData$nat.seedling.sp.y0=="." & !is.na(plotData$nat.seedling.sp.y0)]<-0 #????????


#natseedling ct.yo has '1,1' in it line 350, 544
plotData$nat.seedling.count.y0[plotData$nat.seedling.count.y0=="1,1" & !is.na(plotData$nat.seedling.count.y0)] #<-???? THIS MEANS ONE OF EACH SPECIES

#seeds.per.plot has "50, 50" and "100, 100"

siteData$seeds.per.plot[siteData$seeds.per.plot=="50, 50" & !is.na(siteData$seeds.per.plot)]<-50
siteData$seeds.per.plot[siteData$seeds.per.plot=="100, 100" & !is.na(siteData$seeds.per.plot)]<-100


#put the lat/long/elev, utmZone INTO the plotData and make sure there is a value for each

#make a column of subplots for the Davos site
unique(plotData[plotData$site=="Davos",c("site","zone","transect","plot","treatment","herb.treat","date.seeded")])
unique(plotData[plotData$site=="Davos" & plotData$plot=="p142",c("site","zone","transect","plot","treatment","herb.treat","date.seeded")])

#DAVOS site - treatment is at the plot level but herb.treat and date.seeded are subplots within plot
plotData$subplot[plotData$site=="Davos"]<-paste(plotData$plot[plotData$site=="Davos"],plotData$date.seeded[plotData$site=="Davos"],sep="_")

#Some sites have different plot names for the different species - remove this - BUT CHECK THAT THESE REALLY ARE THE SAME PLOT!

unique(plotData$site[is.na(as.numeric(plotData$plot))]) #Canol Trail, Churchill, Davos and Wolf Creek have letters in plot names - Davos doesn't matter

plotData$plot[plotData$site %in% c("Canol Trail, NWT","Churchill, MB", "Wolf Creek, YK")]<-gsub("[[:alpha:]]","",plotData$plot[plotData$site %in% c("Canol Trail, NWT","Churchill, MB", "Wolf Creek, YK")])


# final check
#visual scan of all categorical columns for unique values that are sensible
#visual scan of all numeric columns for min/max values that are sensible
i <- which(sapply(plotData
            , is.character)==TRUE)

sapply(plotData[,i], unique, 1)

rm(i)#cleanup

i <- which(sapply(plotData
                  , is.numeric)==TRUE)

minfunction<-function(x){
  min(x, na.rm=T)
}
maxfunction<-function(x){
  max(x, na.rm=T)
}

sapply(plotData[,i], minfunction)
sapply(plotData[,i], maxfunction)

rm(i)#cleanup

#repeat for sites

i <- which(sapply(siteData
                  , is.character)==TRUE)

sapply(siteData[,i], unique, 1)

rm(i)#cleanup

i <- which(sapply(siteData
                  , is.numeric)==TRUE)

sapply(siteData[,i], minfunction)
sapply(siteData[,i], maxfunction)

rm(i)#cleanup

#head (plotData)

unique(plotData$site[!is.na(plotData$exp.seedling.count.y2)])
#put the total count back into a single column?


#run nodata only on Sarah's computer
if (getwd()=="C:/Users/selmendorf/Documents/GTREE/DataSynthesisWorkshop"){
library (doParallel) #to speed up the loops

#figure out your computer
myNumCores<-detectCores()

#assign half of your cores to the cluster
cl <- makeCluster(round(myNumCores/2))
registerDoParallel(cl)


plotData[plotData=='']<-NA
plotData[plotData=='nodata']<-NA
plotData[plotData=='.']<-NA
plotData[plotData==' ']<-NA

r<-foreach(i=1:nrow(tosFile))

r<-foreach(i=1:length(unique(plotData$site)))%dopar% {
#for (i in unique (plotData$site)){
  site<-unique(plotData$site)[i]
  outvals<-NULL
  dat1<-plotData[plotData$site==site,]
  trtmts<-unique(dat1$treatment)
    for (j in trtmts){
      dat<-dat1[dat1$treatment==j,]    
      for (k in 1:nrow(dat)){
        for (m in 1:ncol(dat)){
          if (is.na(dat[k,m])){
            vals<-c(site, j, names(dat)[m], dat[k,m])
            outvals<-rbind(outvals, vals)
          }      
        }
      }
    }
  outvals<-data.frame(outvals)
  return (outvals)
}


noData<-plyr::rbind.fill(r)
names(noData)<-c('site', 'trtmt', 'var')
noData<-unique(noData)

}



