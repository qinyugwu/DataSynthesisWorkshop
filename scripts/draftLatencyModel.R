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

#
for (i in 1:n){
  y<-numTrtRecruit + numBackground  
}

numTrtRecruit[seedtrt==ctl]<-0

#binomial option
numTrtRecruit[trt==seeded]~binomial (success rate, trials)

success rate <- invlogit(aSite[site[i]] + bscarification[site[i]]*scarification[plot[i]])
trials<-numseeded*germination rate

germination rate[site] ~ binomial (germinantsInGreenhouse, SeedsInGreenhouse)


#poisson option
numTrtRecruit[trt==seeded]~poisson (muRecruit)
muRecruit=exp(log(trials)+aSite[site[i]+bscarifcation[site[i]*scarifiation[plot[i]]]]) # could add in extra poisson resid variation here if wanted

#trials follows as above

numBackground[plot] ~ poisson (muBackground[site])

#need to keep the next line positive, either lognormal or log the vals
muBackground[site]~exp(normal (loggrandmeanbackground, precisionbackground))

#site variables
aSite~normal(musite, precisionemergenceintercept) # baseline success rate
bscarification~normal(mubetsite, precisionscarification)

#priors
precisionemergenceintercept=1/sigma_emergence*sigma_emergence
sigma_emergence<-dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

precisionscarification=1/sigma_scarification*sigma_scarification
sigma_scarification<-dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

precisionbackground=1/sigma_background*sigma_background
sigma_background<-dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma



musite~normal (0,100)
mubetsite~normal (0,100)
loggrandmeanbackground ~normal (0,100)





numBackground~poisson(sitex)
