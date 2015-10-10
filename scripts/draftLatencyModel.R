#PREPARE DATA

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
plotDatasub$seeds.per.plot<-siteData$seeds.per.plot[match(plotDatasub$site,siteData$site)] #MAKE SURE TO FIX THIS PER SPECIES WHEN WE DO THIS FOR REAL!

#take only the first year seeded and only non-herbivory treatment for Davos
plotDatasub<-plotDatasub[c(plotDatasub$site=="Davos" & c(plotDatasub$start.year==2014 | plotDatasub$provenance=="low" | plotDatasub$herb.treat=="ex"))==FALSE,]

#AT only
plotDatasub<-plotDatasub[plotDatasub$zone=="AT",]


#list of unique plots with the associated site number also with scarT 

#make up germination rate data but replace this will real data later
germRate<-setNames(as.data.frame(cbind(c(seq(1,length(unique(plotDatasub$site)),by=1)),c(runif(length(unique(plotDatasub$site)),0,1)))),c("siteNum","germRate"))


#For JAGS

plotDatasub$siteNum<-as.numeric(as.factor(plotDatasub$site))
plotDatasub$uniquePlotNum<-as.numeric(as.factor(plotDatasub$uniquePlot))

#plots<-unique(plotDatasub[,c("siteNum","uniquePlotNum","seedT","scarT")])

jags.dat<-list(
  #y=plotDatasub$tot.emerge.y1, # number of naturally occuring + germinated for species of interest only (in that plot)
  y=ifelse(is.na(plotDatasub$exp.seedling.count.y1),0,plotDatasub$exp.seedling.count.y1),
  nplot=length(unique(plotDatasub$uniquePlotNum)),
  nsite=length(unique(plotDatasub$site)),
  n=nrow(plotDatasub),
  germRate=germRate$germRate, # make up a number between zero and 1 for each site (length of the number of sites)
  siteData=germRate$siteNum, #one value indicating site to go with germ Rate
  seedT=plotDatasub$seedT,
  scarT=plotDatasub$scarT,
  sitePlot=plotDatasub$siteNum, #vector indicating site, length of the total number of unique plots
  numSeeded=plotDatasub$seeds.per.plot, #fix this to be the proper number PER SPECIES/PROVENANCE
  plot=plotDatasub$uniquePlotNum
  )

str(jags.dat)


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

write("
      model{

for (i in 1:n){ #i indexes over data rows
  y[i] <-numTrtRecruitALL[i] + numBackground[i] #mixture
  numTrtRecruitALL[i]<-numTrtRecruit[i]*seedT[i] #this will cause the numTrtRecruit not to enter the likelihood for the nonseeded plots
  numTrtRecruit[i]~dbinom(emergRate[plot[i]], trials[i]) 
  trials[i]<-numSeeded[i]*germRate[siteData[i]]
}

#need a vector sitePlot of each sitenumber for each plot
#need a vector of scarT (1,0) for each plot
for (j in 1:nplot){
  emergRate[j]<-invlogit(aSite[sitePlot[j]]+bscarT[sitePlot[j]*scarT[j]])
  numBackground[j] ~ dpois(muBackground[sitePlot[j]])
}


for (k in 1:nsite){
# #option for if we have raw data for each site
  #germRate[k] ~ dbinom(germinants[k], seeds[k])
  #need to keep the next line positive, either lognormal or log the vals
  logMuBackground[k]~dnorm(muBackground, taoBackground) #mean background emergence rate, varisLogclace
  muBackground[k]<-exp(logMuBackground[k])
  bscarT[k]~dnorm(muScarT, taoScarT) #mean scarTvarislogscale
  aSite[k]~dnorm(muEmerge, tauEmerge) # baseline success rate, logscale
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

      }
      ","gtree_y1germ.jags")

initsA<-list(muBackground=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))

initsB<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))

initsC<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))

inits<-list(initsA,initsB,initsC)


inits<-NA
list()
  
params<-c('muBackground')
library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, model.file="gtree_y1germ.jags", n.chains=3,n.iter=1000,n.burnin=500, n.thin=2, DIC=FALSE, working.directory=NULL, progress.bar = "text") 
