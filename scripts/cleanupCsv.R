# Script to cleanup csvs 
# rerun ONLY when/if csv export of excel is updated
# assumes working directory is set to DataSynthesisWorkshop repo
# Script by SCE 10/8/2015

siteData<-read.csv('Data/siteData.csv', fileEncoding='UTF-8',stringsAsFactors=F)
plotData<-read.csv('Data/plotData.csv', fileEncoding='UTF-8',stringsAsFactors=F)

#Convert all genus species to the same format

sort(unique(siteData$sp.seeded)) #site sheet is good

sort(unique(plotData$exp.seedling.sp))
plotData$exp.seedling.sp<-gsub("."," ",plotData$exp.seedling.sp,fixed=TRUE)
plotData$exp.seedling.sp<-gsub("_"," ",plotData$exp.seedling.sp,fixed=TRUE)

#cleanup the dates




#check start year against date




#convert zones to standard codes desynonymizing
sort(unique(siteData$zone)) #site codes are fine

sort(unique(plotData$zone))
plotData$zone[plotData$zone=="Alpine" & !is.na(plotData$zone)]<-"AT"
plotData$zone[plotData$zone=="Forest" & !is.na(plotData$zone)]<-"F"
plotData$zone[plotData$zone=="Treeline" & !is.na(plotData$zone)]<-"T"
plotData$zone[plotData$zone=="FT" & !is.na(plotData$zone)] #<-???????????????? (sites are TexasCreek and Blowdown)

#greater than signs in the organic depth (also in "organic"), no data instead of NA in organic depth

siteData$closest.seed.tree[siteData$closest.seed.tree==">500" & !is.na(siteData$closest.seed.tree)] # <-????????????
siteData$closest.stand.tree[siteData$closest.stand.tree==">200m" & !is.na(siteData$closest.stand.tree)] # <-????????????

plotData$organic[plotData$organic=="<1" & !is.na(plotData$organic)]<-"0.5"

sort(unique(plotData$organic.depth))
plotData$organic.depth[plotData$organic.depth==">30" & !is.na(plotData$organic.depth)] #<- ??????????
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

plotData$nat.seedling.sp.y0[plotData$nat.seedling.sp.y0=="." & !is.na(plotData$nat.seedling.sp.y0)] #<-????????


#natseedling ct.yo has '1,1' in it line 350, 544
plotData$nat.seedling.count.y0[plotData$nat.seedling.count.y0=="1,1" & !is.na(plotData$nat.seedling.count.y0)] #<-????

#seeds.per.plot has "50, 50" and "100, 100"

siteData$seeds.per.plot[siteData$seeds.per.plot=="50, 50" & !is.na(siteData$seeds.per.plot)] #<-?????????
siteData$seeds.per.plot[siteData$seeds.per.plot=="100, 100" & !is.na(siteData$seeds.per.plot)] #<-?????????


#put the lat/long/elev, utmZone INTO the plotData and make sure there is a value for each


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


