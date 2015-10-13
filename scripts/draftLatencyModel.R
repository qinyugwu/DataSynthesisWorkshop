#READ DATA

plotDatasub<-read.csv('data/subsets/plotDatasub.csv')



#For JAGS

plotDatasub$siteNum<-as.numeric(as.factor(plotDatasub$site))
plotDatasub$uniquePlotNum<-as.numeric(as.factor(plotDatasub$uniquePlot))

#plots<-unique(plotDatasub[,c("siteNum","uniquePlotNum","seedT","scarT")])

jags.dat<-list(
  #y=plotDatasub$tot.emerge.y1, # number of naturally occuring + germinated for species of interest only (in that plot)
  #y=ifelse(is.na(plotDatasub$exp.seedling.count.y1),0,plotDatasub$exp.seedling.count.y1),
  y=plotDatasub$surv.yr1,
  nplot=length(unique(plotDatasub$uniquePlotNum)),
  nsite=length(unique(plotDatasub$site)),
  n=nrow(plotDatasub),
  #fake germ rate make up a number between zero and 1 for each site
  germRate=runif (length(unique(plotDatasub$site)),0.6, 0.8),
  siteNum=c(1:length(unique(plotDatasub$site))), 
  #germRate=germRate$germRate, # make up a number between zero and 1 for each site (length of the number of sites)
  #siteData=germRate$siteNum, #one value indicating site to go with germ Rate
  seedT=plotDatasub$seedT,
  scarT=plotDatasub$scarT,
  sitePlot=plotDatasub$siteNum, #vector indicating site, length of the total number of unique plots
  numSeeded=plotDatasub$seeds.per.plot, #fix this to be the proper number PER SPECIES/PROVENANCE
  #plot=plotDatasub$uniquePlotNum,
  siteData=plotDatasub$siteNum
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
  #data level - SCE note could make this overdispersed if necessary
  y[i]~dpois(y.exp[i]) 
  #predicted number is the sum of those recruited from added seeds +
  #the background rate from nonexperimentally added
  y.exp[i]<-numTrtRecruit[i] + muBackgroundSiteScar[siteData[i], [scarT[i]+1]]#mixture

  #no longer used - binomial option that caused jags unhappiness
  #when we modeled y as a sum
  #numTrtRecruitALL[i]<-numTrtRecruit[i]*seedT[i] #this will cause the numTrtRecruit not to enter the likelihood for the nonseeded plots
  #numTrtRecruit[i]~dbinom(emergRate[plot[i]], trials[i]) 

}

#need a vector sitePlot of each sitenumber for each plot
#need a vector of scarT (1,0) for each plot
for (j in 1:nplot){
  #num that recruited from the seed treatment addition is a function of
  #the number of trials (viableseeds added - so result is in per viable seed added)
  #plus asite which is the intercept for seed addition (log scale)
  #plus bscarT which is the addition amt of recruitment per seed added by doing scarification
  # the multiplication by seedT will make this term 0 out for nonseeded plots.
  numTrtRecruit[j]<-(exp(log(trials[j])+aSite[sitePlot[j]]+bscarT[sitePlot[j]]*scarT[j]))*seedT[j]
  
  #estimate of viable seed intensity
  trials[j]<-numSeeded[j]*germRate[sitePlot[j]]#make this not fixed? 
  #numBackground[j]<-muBackgroundsite[sitePlot[j]]
}


for (k in 1:nsite){
  for (m in 1:2){# for both notscarified and scarified
# #option for if we have raw data for each site
  #germRate[k] ~ dbinom(germinants[k], seeds[k])
  #need to keep the next line positive, either lognormal or log the vals
  #mean background emergence rate in each scarifed treatment, here 1 indexes for nonscarified
  #2 indexes for scarified, #1 fornonscarifiedg
     #difference between muBackgroundsites is the difference in background between scarified and nonscarified
    #drawn from hierarchicial model hyperparameter across sites
    logmuBackgroundSiteScar[k, m]~dnorm(muBackgroundScarT[m], taoBackgroundScarT[m]) 
    muBackgroundSiteScar[k, m]<-exp(logmuBackgroundSiteScar[k, m])
  }
  #these never enter the likelihood for seeded treatments
  bscarT[k]~dnorm(muScarT, taoScarT) #the trtmt effect of scarification in seeded
  aSite[k]~dnorm(muEmerge, tauEmerge) # baseline success rate w seeding, logscale
}


#priors
tauEmerge<-1/(sigmaEmerge*sigmaEmerge)
sigmaEmerge~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

taoScarT<-1/(sigmaScarT*sigmaScarT)
sigmaScarT~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma

for (m in 1:2){
  taoBackgroundScarT[m]<-1/sigmaBackgroundScarT[m]*sigmaBackgroundScarT[m]
  sigmaBackgroundScarT[m]~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
  muBackgroundScarT[m]~dnorm(0,0.001)
}



muEmerge~dnorm(0,0.001)
muScarT~dnorm(0,0.001)


# #poisson option, trials same as below
# numTrtRecruit[trt==seeded]~poisson (muRecruit)
# muRecruit=exp(log(trials)+aSite[site[i]+bscarifcation[site[i]*scarifiation[plot[i]]]]) # could add in extra poisson resid variation here if wanted

#Derived

for (i in 1:nsite){
  bscarTBT[i]<-exp(bscarT[i])
}

muBackgroundBT<-exp(muBackground)
muEmergeBT<-exp(muEmerge)
muScarTBT<-exp(muScarT)

      }
      ","gtree_y1germ.jags")

# initsA<-list(muBackground=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# initsB<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# initsC<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# inits<-list(initsA,initsB,initsC)

  
params<-c("muEmerge", "sigmaEmerge","muScarT", "sigmaScarT","muBackground", "sigmaBackground","bscarT","aSite","muBackgroundsite","muEmergeBT","muBackgroundBT","muScarTBT")

library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, model.file="gtree_y1germ.jags", n.chains=3,n.iter=10000,n.burnin=2000, n.thin=2, DIC=FALSE, working.directory=NULL, progress.bar = "text")

print(modout.gtree)
plot(modout.gtree)


coefsout<-as.data.frame(modout.gtree$BUGSoutput$summary[,c('mean','sd','2.5%','97.5%')])
coefsout$Type<-as.vector(sapply(strsplit(rownames(coefsout),"[[]",fixed=FALSE), "[", 1))
coefsout$siteNum<-as.vector(c(rep(1:10,2),NA,NA,rep(1:10,1),rep(NA,7)))
coefsout$site<-plotDatasub$site[match(coefsout$siteNum,plotDatasub$siteNum)]

head(coefsout)

ggplot(coefsout[coefsout$Type %in% c("bscarT"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`),width=0.18,size=1.8)+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=0.19676482,ymin=-0.18679486),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(coefsout[coefsout$Type %in% c("muBackgroundsite"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`),width=0.18,size=1.8)+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=1.03232375,ymin=0.70167283),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Background Germination Number\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggplot(coefsout[coefsout$Type %in% c("aSite"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`),width=0.18,size=1.8)+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=1.03232375,ymin=0.70167283),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Background Germination Number\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())


# FARM - original (not-working) model

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
      
      muEmerge~dnorm(0,100)
      muScarT~dnorm(0,100)
      muBackground~dnorm(0,100)
      
      
      
      # #poisson option, trials same as below
      # numTrtRecruit[trt==seeded]~poisson (muRecruit)
      # muRecruit=exp(log(trials)+aSite[site[i]+bscarifcation[site[i]*scarifiation[plot[i]]]]) # could add in extra poisson resid variation here if wanted
      
      }
      ","gtree_y1germ.jags")

count<-unique(plotData[,c("site","zone")])
count$count<-1
count<-aggregate(count$count,by=list(count$site),FUN=sum)
colnames(count)<-c("site","zoneCount")

count2<-as.data.frame(unique(plotData$site[plotData$exp.seedling.count.y2>0 & !is.na(plotData$exp.seedling.count.y2)]))
colnames(count2)<-"site"
count2$hasY2<-1

count$hasY2<-count2$hasY2[match(count$site,count2$site)]
