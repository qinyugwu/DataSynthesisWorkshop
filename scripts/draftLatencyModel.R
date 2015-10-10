#PREPARE DATA

plotDatasub<-plotData[plotData$site != "Churchill, MB",]

#extract only ONE SPECIES for now
myspp<-as.data.frame(unique(siteData[,c("site","sp.seeded")]))
myspp<-myspp[!duplicated(myspp$site),]
plotDatasub$spp.number<-as.numeric(as.factor(paste(plotData$site,plotData$exp.seedling.sp)))

#Unique plot names per plot
plotDatasub$uniquePlot<-paste(plotDatasub$site,plotDatasub$zone,plotDatasub$transect,plotDatasub$plot,sep="_")

#check this
tempor<-as.data.frame(table(plotDatasub$uniquePlot))
tempor[tempor$Freq>1,]
#CHURCHILL IS WEIRD! LEAVE OUT FOR NOW

numSeeded

plotDatasub$seedT<-0
plotDatasub$seedT[plotDatasub$treatment %in% c("SD","SS","PSS")]<-1 #need this to be per plot

plotDatasub$scarT<-0
plotDatasub$scarT[plotDatasub$treatment %in% c("SC","SS","PSS","PSC")]<-1 #per plot

#list of unique plots with the associated site number also with scarT 
#take only the first year seeded and only non-herbivory treatment for Davos

jags.dat<-list(
  #y = number of naturally occuring + germinated for species of interest only (in that plot)
  #germRate = make up a number between zero and 1 for each site (length of the number of sites)
  #siteData = one value indicating site to go with germ Rate
  #sitePlot = vector indicating site, length of the total number of unique plots
  #nsite
  #n = total rows
  #nplot
  
  )




#numAdded = 100
#numNatOccuring = 10

#y=rpois(1,100) + numNatOccuring

#log(y) = Bprop*log(numAdded + numNatOccuring)

#need n = number of data points
#need a vector one per row of data y ->total#seedlings of species of interest
#need a vector one per row of data seedT ->seeded or not?
#need a vector one per row of plot ->ids per row
#need a vector one per row of numseeded ->ids
#need a vector one per row of site -> ids

for (i in 1:n){ #i indexes over data rows
  y[i] <-numTrtRecruitALL[i] + numBackground[i] #mixture
  numTrtRecruitALL[i]<-numTrtRecruit[i]*seedT[i] #this will cause the numTrtRecruit not to enter the likelihood for the nonseeded plots
  numTrtRecruit[i]~binomial (emergRate[plot[i]], trials[i]) 
  trials[i]<-numSeeded[i]*germRate[siteData[i]]
}

#need a vector sitePlot of each sitenumber for each plot
#need a vector of scarT (1,0) for each plot
for (j in 1:nplot){
  emergRate[j]<-invlogit(aSite[sitePlot[j]]+bscarT[sitePlot[j]*scarT[j]])
  numBackground[j] ~ poisson (muBackground[sitePlot[j])
}


for (k in 1:nsite){
# #option for if we have raw data for each site
  #germRate[k] ~ binomial (germinants[k], seeds[k])
  #need to keep the next line positive, either lognormal or log the vals
  logMuBackground[k]~normal (muBackground, taoBackground) #mean background emergence rate, varisLogclace
  muBackground[k]<-exp(logMuBackground[k])
  bscarT[k]~normal (muScarT, taoScarT)) #mean scarTvarislogscale
  aSite[k]~normal(muEmerge, tauEmerge) # baseline success rate, logscale
}


#priors
tauEmerge<-1/(sigmaEmerge*sigmaEmerge)
sigmaEmerge~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

taoScarT<-1/(sigmaScarT*sigmaScarT)
sigmaScarT~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

taoBackground<-1/sigmaBackground*sigmaBackground
sigmaBackground~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

muEmerge~normal (0,100)
muScarT~normal (0,100)
muBackground~normal (0,100)



# #poisson option, trials same as below
# numTrtRecruit[trt==seeded]~poisson (muRecruit)
# muRecruit=exp(log(trials)+aSite[site[i]+bscarifcation[site[i]*scarifiation[plot[i]]]]) # could add in extra poisson resid variation here if wanted



