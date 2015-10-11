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

#NO INTERACTION

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
            
      muEmerge~dnorm(0,0.0001)
      muScarT~dnorm(0,0.0001)
      muSeedT~dnorm(0,0.0001)
      
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

  
params<-c("muEmerge", "sigmaEmerge","muScarT", "sigmaScarT","bscarT","bseedT",
          "aSite","muEmergeBT","muScarTBT","muSeedT","muSeedTBT", "Disp1",
          "Dispersion","Fit", "FitNew")

library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, 
                   model.file="gtree_y1germ.jags",
                   n.chains=3,n.iter=1000,n.burnin=100,
                   n.thin=10, DIC=TRUE, working.directory=NULL,
                   progress.bar = "text")

print(modout.gtree)
plot(modout.gtree)


#Baesiant p value suggests that this is a craptastic model
#You want it to be close to 0.50
mean(modout.gtree$BUGSoutput$sims.list$FitNew>modout.gtree$BUGSoutput$sims.list$Fit)

coefsout<-as.data.frame(modout.gtree$BUGSoutput$summary[,c('mean','sd','2.5%','50%','97.5%')])
overDisp<-coefsout$`50%`[rownames(coefsout)=="Dispersion"]/(jags.dat$n-3) #Dispersion/N-k where k= # of reg params and N is sample size
coefsout$Type<-as.vector(sapply(strsplit(rownames(coefsout),"[[]",fixed=FALSE), "[", 1))
coefsout$siteNum<-gsub("\\D","",  rownames(coefsout)) #add in sitenumber
coefsout$site<-plotDatasub$site[match(coefsout$siteNum,plotDatasub$siteNum)]


plot.Teff<-ggplot(coefsout[coefsout$Type %in% c("bscarT","bseedT"),])+
  geom_point(aes(x=site,y=mean,colour=Type),size=6,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=2.582,ymin=-2.101),alpha=0.2,fill="orange")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=8.834,ymin=2.360),alpha=0.2,fill="blue")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect Coefficient\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggsave(plot.Teff,filename = "figures/treatment_effect_poisson.pdf")

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

#WITH INTERACTION


write("
      model{
      #need different background rates for scarified and not scarified treatments
      #difference in background rate for scarified and not scarified plots (both not seeded)
      for (i in 1:n){ #i indexes over data rows
      y[i]~dpois(mu[i])
      log(mu[i])<-max(-20,min(20,eta[i]))
      eta[i]<-log(trials[i])+aSite[siteData[i]]+bscarT[siteData[i]]*scarT[i]+bseedT[siteData[i]]*seedT[i]+
      bSeedTscarT[siteData[i]]*seedT[i]*scarT[i]

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
      bSeedTscarT[k]~dnorm(muSeedTscarT,tauSeedTScarT)
      }
      
      
      #priors
      tauEmerge<-1/(sigmaEmerge*sigmaEmerge)
      sigmaEmerge~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      tauScarT<-1/(sigmaScarT*sigmaScarT)
      sigmaScarT~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      tauSeedT<-1/(sigmaSeedT*sigmaSeedT)
      sigmaSeedT~dunif (0,10) 

      tauSeedTScarT<-1/(sigmaSeedTScarT*sigmaSeedTScarT)
      sigmaSeedTScarT~dunif (0,10)
      
      muEmerge~dnorm(0,0.0001)
      muScarT~dnorm(0,0.0001)
      muSeedT~dnorm(0,0.0001)
      muSeedTscarT~dnorm(0,0.0001)
      
      #Derived
      #estimates at each site
      for (k in 1:nsite){
        ctlSite[k]<-exp(aSite[k])
        seedSite[k]<-exp(aSite[k]+bseedT[k])
        scarSite[k]<-exp(aSite[k]+bscarT[k])
        seedscarSite[k]<-exp(aSite[k]+bscarT[k]+bseedT[k]+bSeedTscarT[k])
      }
      
      ctl<-exp(muEmerge)
      scar<-exp(muEmerge+muScarT)
      seed<-exp(muEmerge+muSeedT)
      both<-exp(muEmerge+muSeedT+muScarT+muSeedTscarT)

      #muBackgroundBT<-exp(muBackground)
      muEmergeBT<-exp(muEmerge)
      muScarTBT<-exp(muScarT)
      muSeedTBT<-exp(muSeedT)
      
      }
      ","gtree_y1germ_interact.jags")

# initsA<-list(muBackground=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# initsB<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# initsC<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# inits<-list(initsA,initsB,initsC)

#This works:
params<-c("muEmerge", "sigmaEmerge","muScarT", "sigmaScarT","bscarT","bseedT",
          "bSeedTscarT","muSeedTscarT",
          "aSite","muEmergeBT","muScarTBT","muSeedT","muSeedTBT", "Dispersion",
          "Fit", "FitNew", "ctl", "seed","scar", "both", "ctlSite", "seedSite",
          "scarSite", "seedscarSite")

#This doesn't work'
params<-c("muEmerge", "sigmaEmerge","muScarT", "sigmaScarT","bscarT","bseedT",
          "bSeedTscarT","muSeedTscarT",
          "aSite","muEmergeBT","muScarTBT","muSeedT","muSeedTBT", "Dispersion",
          "Fit", "FitNew", "ctl", "seed","scar", "both", "ctlSite", "seedSite",
          "scarSite", "seedscarSite", "sigmaSeedTScarT")

library(rjags)
library(R2jags)
modout.gtree.interaction<-jags(jags.dat,inits=NULL, params, 
                   model.file="gtree_y1germ_interact.jags",
                   n.chains=3,n.iter=10000,n.burnin=1000,
                   n.thin=10, DIC=TRUE, working.directory=NULL,
                   progress.bar = "text")

print(modout.gtree.interaction)
plot(modout.gtree.interaction)


#Baesiant p value suggests that this is a craptastic model
#You want it to be close to 0.50
mean(modout.gtree.interaction$BUGSoutput$sims.list$FitNew>modout.gtree.interaction$BUGSoutput$sims.list$Fit)

coefsout<-as.data.frame(modout.gtree.interaction$BUGSoutput$summary[,c('mean','sd','2.5%','50%','97.5%')])
overDisp<-coefsout$`50%`[rownames(coefsout)=="Dispersion"]/(jags.dat$n-3) #Dispersion/N-k where k= # of reg params and N is sample size
coefsout$Type<-as.vector(sapply(strsplit(rownames(coefsout),"[[]",fixed=FALSE), "[", 1))
coefsout$siteNum<-gsub("\\D","",  rownames(coefsout)) #add in sitenumber
coefsout$site<-plotDatasub$site[match(coefsout$siteNum,plotDatasub$siteNum)]


plot.Pred<-ggplot(coefsout[coefsout$Type %in% c("ctlSite","scarSite", "seedSite", "seedscarSite"),])+
  geom_point(aes(x=site,y=mean,colour=Type),size=6,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  #geom_ribbon(aes(x=as.numeric(factor(site)),ymax=2.582,ymin=-2.101),alpha=0.2,fill="orange")+
  #geom_ribbon(aes(x=as.numeric(factor(site)),ymax=8.834,ymin=2.360),alpha=0.2,fill="blue")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect Coefficient\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggsave(plot.Pred,filename = "figures/treatment_effect_poisson.pdf")




plot.Teff<-ggplot(coefsout[coefsout$Type %in% c("bscarT","bseedT"),])+
  geom_point(aes(x=site,y=mean,colour=Type),size=6,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=2.582,ymin=-2.101),alpha=0.2,fill="orange")+
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=8.834,ymin=2.360),alpha=0.2,fill="blue")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect Coefficient\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggsave(plot.Teff,filename = "figures/treatment_effect_poisson.pdf")


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

