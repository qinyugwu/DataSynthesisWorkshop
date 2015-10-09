# Script to cleanup csvs 
# rerun ONLY when/if csv export of excel is updated
# assumes working directory is set to DataSynthesisWorkshop repo
# Script by SCE 10/8/2015

siteData<-read.csv('Data/siteData.csv', fileEncoding='UTF-8',stringsAsFactors=F)
plotData<-read.csv('Data/plotData.csv', fileEncoding='UTF-8',stringsAsFactors=F)

#Convert all genus species to the same format


#cleanup the dates -



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


# run sapply (unique) against all the categorical columns at the end to make sure
# they look nice



# run sapply (min, max) against all the numeric columns at the end to make sure
# the ranges are appropriate



