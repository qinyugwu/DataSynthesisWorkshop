# Script to cleanup csvs 
# rerun ONLY when/if csv export of excel is updated
# assumes working directory is set to DataSynthesisWorkshop repo
# Script by SCE 10/8/2015 & ADB


#TODO:
#cleanup date counted
#check start year against date
#decipher the multiseeded species data
# add in and otherwise rectify the emergence vs survival data
# check the veg cover data
# sort out what the F FT is for zones
# work out what to do with the NAs by checking everyone's spreadsheets
# work with y2 data - that is totally ignored thus far, and is currently reading as a logical
# still a bunch of NAs in the nat seedling cts and expt seedling cts - what is this?


siteData<-read.csv('Data/siteData.csv', fileEncoding='UTF-8',stringsAsFactors=F,
                   strip.white=T)
#changed to read the plotData directly
plotData<-read.csv('Data/plotData.csv', fileEncoding='latin1',stringsAsFactors=F,
                   strip.white=T)
plotData<-plotData[plotData$site!='',]

#replace blanks with NA in exp.seedling.sp
plotData$exp.seedling.sp[plotData$exp.seedling.sp==""]<-NA

#Convert all genus species to the same format

sort(unique(siteData$sp.seeded)) #site sheet is good

#plot needs fixing, so we fixed it
sort(unique(plotData$exp.seedling.sp))
plotData$exp.seedling.sp<-gsub("."," ",plotData$exp.seedling.sp,fixed=TRUE)
plotData$exp.seedling.sp<-gsub("_"," ",plotData$exp.seedling.sp,fixed=TRUE)

#cleanup the dates
#checking against original files, these are an excel problem
#"41918"   #10/6/2014
#"41919"   #10/7/2014
plotData$date.seeded[plotData$date.seeded=="41918"]<-"20141006"
plotData$date.seeded[plotData$date.seeded=="41919"]<-"20141007"

plotData$date.seeded[plotData$date.seeded=='22.5.2014']<-'20140522'
plotData$date.seeded[plotData$date.seeded=='3.6.2014']<-'20140306'
plotData$date.seeded[plotData$date.seeded=='25.6.2014']<-'20140625'
plotData$date.seeded[plotData$date.seeded=='16.7.2013']<-'20130716'

#plotData$date.counted is still all fucked up, but we didn't dealwith it
unique (plotData$date.counted)


#convert zones to standard codes desynonymizing
sort(unique(siteData$zone)) #site codes are fine

sort(unique(plotData$zone))
plotData$zone[plotData$zone=="Alpine" & !is.na(plotData$zone)]<-"AT"
plotData$zone[plotData$zone=="Forest" & !is.na(plotData$zone)]<-"F"
plotData$zone[plotData$zone=="Treeline" & !is.na(plotData$zone)]<-"T"
plotData$zone[plotData$zone=="FT" & !is.na(plotData$zone)]<-"T" #???????????????? (sites are TexasCreek and Blowdown)

#add provenence data for 12 Mile site (GET THIS INFO FROM BECKY AND TERESA!)
plotData$provenance[plotData$site=="12 Mile" & plotData$transect %in% c(1,2)]<-"high"
plotData$provenance[plotData$site=="12 Mile" & plotData$transect %in% c(3,4)]<-"low"

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

#convert seedling counts to integers
plotData$nat.seedling.count.y0<-suppressWarnings(as.integer(
  plotData$nat.seedling.count.y0))
plotData$nat.seedling.count.y1<-suppressWarnings(as.integer(
  plotData$nat.seedling.count.y1))

# make a unique plot column - CHECK WITH ANDREW WHAT UNIQUE NAMES ARE

#plotData$unique.plot<-paste(plotData$site,plotData$plot,sep="_")

# closest.seed.tree and closest.stand.tree have questions marks

sort(unique(siteData$closest.seed.tree))

siteData$closest.seed.tree[siteData$closest.seed.tree=="?" & !is.na(siteData$closest.seed.tree)]<-NA
siteData$closest.stand.tree[siteData$closest.stand.tree=="?" & !is.na(siteData$closest.stand.tree)]<-NA

siteData<-within(siteData,closest.seed.tree<-as.numeric(closest.seed.tree))
siteData<-within(siteData,closest.stand.tree<-as.numeric(closest.stand.tree))


# other column has a lot of columns and then 'herbfield'
                   
plotData$other[plotData$other=="herbfield" & !is.na(plotData$other)] #<-???????????

plotData$nat.seedling.sp.y0[plotData$nat.seedling.sp.y0=='ABLA']<-'Abies lasiocarpa'

# natsp.y0 has a '.' in it in many rows - should this be NA or something else

#plotData$nat.seedling.sp.y0[plotData$nat.seedling.sp.y0=="." & !is.na(plotData$nat.seedling.sp.y0)]<-0 #????????

#assume if theres no count, we don't care about the species.
plotData$nat.seedling.sp.y0[plotData$nat.seedling.count.y0==0]<-NA
plotData$nat.seedling.sp.y1[plotData$nat.seedling.count.y1==0]<-NA
plotData$nat.seedling.sp.y2[plotData$nat.seedling.count.y2==0]<-NA

#natseedling ct.yo has '1,1' in it line 350, 544
plotData$nat.seedling.count.y0[plotData$nat.seedling.count.y0=="1,1" &
                                 !is.na(plotData$nat.seedling.count.y0)] #<-???? THIS MEANS ONE OF EACH SPECIES

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

#THIS PART NO LONGER NECESSARY WITH ANDREW'S COUNTS??
# #head (plotData)
# 
# unique(plotData$site[!is.na(plotData$exp.seedling.count.y2)])
# #put the total count back into a single column?
# 
# #Fill in the seeded spp to plotData where not there already
# siteSpecies<-unique(siteData[,c('site', 'sp.seeded')])
# 
# #save full copy incase you want full set back
# bu<-plotData
# 
# plotData<-merge(bu, siteSpecies, all.x=T)
# 
# plotData$exp.seedling.sp[is.na(plotData$exp.seedling.sp)]<-plotData$sp.seeded[is.na(plotData$exp.seedling.sp)]
# 
# #check all filled
# #unique (plotData$exp.seedling.sp)
# 
# #HIGH RISK, WE SHOULD DOUBLE CHECK THE NA SPREADSHEET
# #THIS IS MORE COMPLICATED FOR YEAR 2 - some did NOT SURVEY
# plotData$nat.seedling.count.y0[is.na(plotData$nat.seedling.count.y0)]<-0
# plotData$nat.seedling.count.y1[is.na(plotData$nat.seedling.count.y1)]<-0
# 
# #plotData$exp.seedling.count.y0[is.na(plotData$exp.seedling.count.y0)]<-0
# plotData$exp.seedling.count.y1[is.na(plotData$exp.seedling.count.y1)]<-0
# 
# #make a total column which is the sum of expseedling cts plus natural 
# #seedling counts OF THE CORRECT species
# for (i in 1:nrow(plotData)){
#   plotData$tot.emerge.y1[i]<-plotData$exp.seedling.count.y1[i]
#   #assume that nonseeded plots with NA in expt seedling didn't write them in here
# #   if (plotData$treatment[i]%in%c('SC', 'CN', 'PSC')&is.na(plotData$tot.emerge.y1[i])){
# #     plotData$tot.emerge.y1[i]<-0    
# #   }
#   #add in the nat.seedling.count if the species matches that which was added
#   if (!is.na(plotData$nat.seedling.count.y1[i])&!is.na(plotData$nat.seedling.sp.y1[i])){
#     toadd<-0 # to make sure
#     toadd<-plotData$nat.seedling.count.y1[i]*
#       ifelse(plotData$nat.seedling.sp.y1[i]==plotData$exp.seedling.sp[i],1,0)
#     plotData$tot.emerge.y1[i]<-plotData$tot.emerge.y1[i]+toadd
#   }
# }
# 
# unique(plotData$nat.seedling.count.y1[plotData$treatment%in%c('SC','CN')&
#                                         plotData$nat.seedling.sp.y1==plotData$exp.seedling.sp])

#write out files
write.csv (plotData, 'Data/cleanData/plotDataClean.csv', row.names=F)
write.csv (siteData, 'Data/cleanData/siteDataClean.csv', row.names=F)


#more checking

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

