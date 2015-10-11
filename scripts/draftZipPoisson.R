#READ DATA

plotDatasub<-read.csv('data/subsets/plotDatasub.csv')



#For JAGS

plotDatasub$siteNum<-as.numeric(as.factor(plotDatasub$site))
plotDatasub$uniquePlotNum<-as.numeric(as.factor(plotDatasub$uniquePlot))

#plots<-unique(plotDatasub[,c("siteNum","uniquePlotNum","seedT","scarT")])

jags.dat<-list(
  y=plotDatasub$tot.emerge.y1, # number of naturally occuring + germinated for species of interest only 
  #y=ifelse(is.na(plotDatasub$exp.seedling.count.y1),0,plotDatasub$exp.seedling.count.y1),
  #nplot=length(unique(plotDatasub$uniquePlotNum)),
  nsite=length(unique(plotDatasub$site)),
  n=nrow(plotDatasub),
  germRate=runif (length(unique(plotDatasub$site)),0.6, 0.8), # make up a number between zero and 1 for each site (length of the number of sites)
  seedT=plotDatasub$seedT,
  scarT=plotDatasub$scarT,
  #sitePlot=plotDatasub$siteNum, #vector indicating site, length of the total number of unique plots
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

#Model based on Zuur ZI Models book p. 55

write("
      model{
#need different background rates for scarified and not scarified treatments
#difference in background rate for scarified and not scarified plots (both not seeded)
      for (i in 1:n){ #i indexes over data rows
      y[i]~dpois(mu[i])
      log(mu[i])<-max(-20,min(20,eta[i]))
      eta[i]<-log(trials[i])+aSite[siteData[i]]+bscarT[siteData[i]]*scarT[i]+bseedT[siteData[i]]*seedT[i] #+a[i]
      trials[i]<-numSeeded[i]*germRate[siteData[i]] #make this not fixed? 
      
      #Discrepancy measures
      yNew[i]~dpois(mu[i])
      PRes[i]<-(y[i]-mu[i])/sqrt(mu[i])
      PResNew[i]<-(yNew[i]-mu[i])/sqrt(mu[i])
      D[i]<-(PRes[i]*PRes[i])
      DNew[i]<-(PResNew[i]*PResNew[i])
      ExpY[i]<-exp(eta[i])*exp(tauEmerge/2)
      
      VarY[i]<-exp(eta[i])*(exp(eta[i])*(exp(tauEmerge)-1)*exp(tauEmerge)+exp(tauEmerge/2))
      PResEQ[i]<-(y[i]-ExpY[i])/sqrt(VarY[i])
      Disp1[i]<-(PResEQ[i]*PResEQ[i])
      
      }
      
      Dispersion<-sum(Disp1) #[]?
      Fit<-sum(D)
      FitNew<-sum(DNew)
      
      
      for (k in 1:nsite){
      # #option for if we have raw data for each site
      #germRate[k] ~ dbinom(germinants[k], seeds[k])
      #need to keep the next line positive, either lognormal or log the vals
      bscarT[k]~dnorm(muScarT, tauScarT) #mean scarTvarislogscale
      aSite[k]~dnorm(muEmerge, tauEmerge) # baseline success rate, logscale
      bseedT[k]~dnorm(muSeedT,tauSeedT)
      }
      
      
      #priors
      tauEmerge<-1/(sigmaEmerge*sigmaEmerge)
      sigmaEmerge~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      tauScarT<-1/(sigmaScarT*sigmaScarT)
      sigmaScarT~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      tauSeedT<-1/(sigmaSeedT*sigmaSeedT)
      sigmaSeedT~dunif (0,10) 
      
      #tauBackground<-1/sigmaBackground*sigmaBackground
      #sigmaBackground~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      muEmerge~dnorm(0,0.0001)
      muScarT~dnorm(0,0.0001)
      #muBackground~dnorm(0,0.0001)
      muSeedT~dnorm(0,0.0001)
      
      # #poisson option, trials same as below
      # numTrtRecruit[trt==seeded]~poisson (muRecruit)
      # muRecruit=exp(log(trials)+aSite[site[i]+bscarifcation[site[i]*scarifiation[plot[i]]]]) # could add in extra poisson resid variation here if wanted
      
      #Derived
      
      for (i in 1:nsite){
      bscarTBT[i]<-exp(bscarT[i])
      bseedTBT[i]<-exp(bseedT[i])
      }
      
      #muBackgroundBT<-exp(muBackground)
      muEmergeBT<-exp(muEmerge)
      muScarTBT<-exp(muScarT)
      muSeedTBT<-exp(muSeedT)
      
      }
      ","gtree_y1germ.jags")

initsA<-list(muBackground=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))

initsB<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))

initsC<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))

inits<-list(initsA,initsB,initsC)

  
params<-c("muEmerge", "sigmaEmerge","muScarT", "sigmaScarT","bscarT","bseedT","aSite","muEmergeBT","muScarTBT","muSeedT","muSeedTBT")

library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, model.file="gtree_y1germ.jags", n.chains=3,n.iter=1000,n.burnin=20, n.thin=2, DIC=FALSE, working.directory=NULL, progress.bar = "text")

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
