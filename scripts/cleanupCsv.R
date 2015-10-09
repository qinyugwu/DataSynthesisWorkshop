# Script to cleanup csvs 
# rerun ONLY when/if csv export of excel is updated
# assumes working directory is set to DataSynthesisWorkshop repo
# Script by SCE 10/8/2015

siteData<-read.csv('Data/siteData.csv', fileEncoding='UTF-8',stringsAsFactors=F)
plotData<-read.csv('Data/plotData.csv', fileEncoding='UTF-8',stringsAsFactors=F)

#Convert all genus species to the same format


#cleanup the dates




#check start year against date




#convert zones to standard codes desynonymizing





#greater than signs in the organic depth (also in "organic"), no data instead of NA in organic depth



#check to make sure all fields that should be numeric are coded as such




# make a unique plot column



#make sure the site join works so there one and only row row of site data per
# site within the plot data.



# other column has a lot of columns and then 'herbfield'
                   



# natsp.y0 has a '.' in it in many rows - should this be NA or something else




#natseedling ct.yo has '1,1' in it line 350, 544





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


