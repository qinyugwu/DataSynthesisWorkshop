#READ DATA

plotDatasub<-read.csv('data/subsets/plotDatasub.csv')



#For JAGS

plotDatasub$siteNum<-as.numeric(as.factor(plotDatasub$site))
plotDatasub$uniquePlotNum<-as.numeric(as.factor(plotDatasub$uniquePlot))

#plots<-unique(plotDatasub[,c("siteNum","uniquePlotNum","seedT","scarT")])

jags.dat<-list(
  y=plotDatasub$germ.y1, # number of naturally occuring + germinated for species of interest only 
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


#view data
str(jags.dat)


#data description
#y = response variable, such as count of germinants in a particular year <vector,length =n>
#nsite = number of sites included in analysis <scalar>
#n = total number of data points (rows in data) <scalar>
#germrate = estimate of seed viability in lab.
#seedT = seeding treatment per record, 1= seeded, 0 = not seeded.<vector,length =n>
#scarT = seeding treatment per record, 1= seeded, 0 = not seeded.<vector,length =n>
#numSeeded -><vector,length =n> number of seeds added in each plot of each species
#siteData = <vector,length =n> siteID for each record

# OVERDISPERSED, NO INTERACTION ---------

#Model based on Zuur ZI Models book p. 55
# 10/14/2015 the offset is in the wrong place here, but this one runs

write("
      model{
#need different background rates for scarified and not scarified treatments
#difference in background rate for scarified and not scarified plots (both not seeded)
      for (i in 1:n){ #i indexes over data rows
      y[i]~dpois(mu[i])
      log(mu[i])<-max(-20,min(20,eta[i]))
      eta[i]<-log(trials[i])+aSite[siteData[i]]+bscarT[siteData[i]]*scarT[i]+bseedT[siteData[i]]*seedT[i]+e[i]
      trials[i]<-numSeeded[i]*germRate[siteData[i]] #make this not fixed? 

      e[i]~dnorm(0,tau.resid)
      
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

      tau.resid<-1/(sigmaResid*sigmaResid)
      sigmaResid~dunif(0,10)

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
      ","gtree_y1germ_overdisp.jags")

# initsA<-list(muBackground=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# initsB<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# initsC<-list(aplot=c(rnorm(jags.dat$nplot-1,0,2),NA),t=as.vector(apply(jags.dat$lim,1,mean)),sigma.plot=runif(1,0,1),sigma=rlnorm(1))
# 
# inits<-list(initsA,initsB,initsC)

  
params<-c("muEmerge", "sigmaEmerge","muScarT", "sigmaScarT","bscarT","bseedT",
          "aSite","muEmergeBT","muScarTBT","muSeedT","muSeedTBT", "Disp1",
          "sigmaResid","Dispersion","Fit", "FitNew")


library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, 
                   model.file="gtree_y1germ_overdisp.jags",
                   n.chains=3,n.iter=10000,n.burnin=1000, n.thin=10,
                   DIC=TRUE, working.directory=NULL, progress.bar = "text")

print(modout.gtree)
plot(modout.gtree)

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

ggsave(plot.Teff,filename = "figures/treatment_effect_poisson_overdisp.pdf")

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


# OVERDISPERSED, WITH INTERACTION ---------

plotDatasub$seeds.per.plot[plotDatasub$site=="Craigieburn"]<-150
plotDatasub$seeds.per.plot[plotDatasub$site=="Tesso"]<-300
plotDatasub$seeds.per.plot[plotDatasub$site=="Texas Creek"]<-100

plotDatasub$siteNum<-as.numeric(as.factor(as.character(plotDatasub$site)))
plotDatasub$uniquePlotNum<-as.numeric(as.factor(as.character(plotDatasub$uniquePlot)))

jags.dat<-list(
  y=plotDatasub$germ.y1, # number of naturally occuring + germinated for species of interest only 
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

write("
      model{
      #need different background rates for scarified and not scarified treatments
      #difference in background rate for scarified and not scarified plots (both not seeded)
      for (i in 1:n){ #i indexes over data rows
      y[i]~dpois(mu[i])
      #this keeps mu from going crazy places
      log(mu[i])<-max(-20,min(20,eta[i]))

      #expected values for each datapoint
      #if trtmt = c -> aSite
      #if trtmt = scarified -> aSite +bScar
      #if trtmt = seeded ->aSite + bSeed +offset(log(numSeeded))
      #if trtmt = seed+scar -> aSite +bscar + offset (log(numseeded))+cinteract

      eta[i]<-aSite[siteData[i]]+ bScar[siteData[i]]*scarT[i]+
        log(trials[i])*seedT[i]+
        bSeed[siteData[i]]*seedT[i]+
        cInteract[siteData[i]]*seedT[i]*scarT[i]
      # if we had measurements of uncertainty on the germRates we could incorporate it here
      trials[i]<-numSeeded[i]*germRate[siteData[i]] #make this not fixed? 
      
      e[i]~dnorm(0,tau.resid)
      
      #Discrepancy measures
      
      yNew[i]~dpois(mu[i])
      PRes[i]<-(y[i]-mu[i])/sqrt(mu[i])
      PResNew[i]<-(yNew[i]-mu[i])/sqrt(mu[i])
      D[i]<-(PRes[i]*PRes[i])
      DNew[i]<-(PResNew[i]*PResNew[i])

      #SCE is not sure how you calculate expY now that the
      # overdispersion term is in there, this might not be quite right
      # the line is copied from the example Zuur model WITHOUT overdispersion
      # we might want something other than tauEmerge here??
      ExpY[i]<-exp(eta[i])*exp(tauEmerge/2)
      
      VarY[i]<-exp(eta[i])*(exp(eta[i])*(exp(tauEmerge)-1)*exp(tauEmerge)+exp(tauEmerge/2))
      PResEQ[i]<-(y[i]-ExpY[i])/sqrt(VarY[i])
      Disp1[i]<-(PResEQ[i]*PResEQ[i])
      
      }
      
      Dispersion<-sum(Disp1) 
      Fit<-sum(D)
      FitNew<-sum(DNew)
      
      
      for (k in 1:nsite){
        #for (m in 1:2){ #m indexes over seed trtmts
        # baseline success rate in unscarified treatments (log scale)
        #log scale
        aSite[k]~dnorm(muEmerge, tauEmerge)
        #effect of seeding per viable seed added
        bScar[k]~dnorm(muScar, tauScar)
        bSeed[k]~dnorm(muSeed, tauSeed)
        cInteract[k]~dnorm(muInteract, tauInteract)
      }

      
      #priors
      #grand means for emergence, and betas of scar, seed, and interaction
      muEmerge~dnorm(0,0.0001)
      muScar~dnorm(0,0.0001)
      muSeed~dnorm(0,0.0001)
      muInteract~dnorm(0,0.0001)
  
      #overdispersion
      tau.resid<-1/(sigmaResid*sigmaResid)
      sigmaResid~dunif(0,10)
      
      tauEmerge<-1/(sigmaEmerge*sigmaEmerge)
      sigmaEmerge~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      tauScar<-1/(sigmaScar*sigmaScar)
      sigmaScar~dunif (0,10) #check this makes sens with the vals and posterior or coudl change to gamma
      
      tauSeed<-1/(sigmaSeed*sigmaSeed)
      sigmaSeed~dunif (0,10) 
      tauInteract<-1/(sigmaInteract*sigmaInteract)
      sigmaInteract~dunif (0,10) 
      
      #predictions 
      for (k in 1:nsite){
        for (l in 1:2){#seeding
          for (m in 1:2){#scarification
            predVals[k,l,m]<-exp(aSite[k]+bSeed[k]*(l-1)+bScar[k]*(m-1)+
            cInteract[k]*(l-1)*(m-1))
          }
        }
      }
      
      }
      ","gtree_y1germ_overdisp_interact.jags")

#2-way interaction, Back-Transformed
paramsBT<-NULL
for (k in 1:jags.dat$nsite){
  for (l in 1:2){
    for (m in 1:2){
    out<-as.vector(paste("aSiteBT[",k,",",l,",",m,"]",sep=""))
    paramsBT<-c(paramsBT,out)
  }}}

params<-c("muEmerge", "sigmaEmerge","aSite", "bSeed", "bScar", "cInteract",
"sigmaResid","Dispersion","Fit",
          "FitNew","muScar", "muInteract", "muSeed", "sigmaSeed", "sigmaInteract",
"sigmaScar", paramsBT)


library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, 
                   model.file="gtree_y1germ_overdisp_interact.jags",
                   n.chains=3,n.iter=20000,n.burnin=5000, n.thin=10,
                   DIC=TRUE, working.directory=NULL, progress.bar = "text")

print(modout.gtree)
plot(modout.gtree)

mean(modout.gtree$BUGSoutput$sims.list$FitNew>modout.gtree$BUGSoutput$sims.list$Fit)

coefsout<-as.data.frame(modout.gtree$BUGSoutput$summary[,c('mean','sd','2.5%','50%','97.5%')])
overDisp<-coefsout$`50%`[rownames(coefsout)=="Dispersion"]/(jags.dat$n-3) #Dispersion/N-k where k= # of reg params and N is sample size

coefsout$Type<-as.vector(sapply(strsplit(rownames(coefsout),"[[]",fixed=FALSE), "[", 1))
#coefsout$siteNum<-gsub("\\D","",  rownames(coefsout)) #add in sitenumber
#coefsout$site<-plotDatasub$site[match(coefsout$siteNum,plotDatasub$siteNum)]
as.vector(sapply(strsplit(rownames(coefsout),"\\D",fixed=FALSE), "[", 1))

mysplit<-function(x,maxLength){
  xtemp<-as.numeric(unlist(strsplit(gsub('[A-z]', '', x),split=',')))
  tofill<-maxLength-length(xtemp)
  xtemp<-c(rep(NA, tofill), xtemp)
  return(xtemp)
}

cbind.data.frame(coefsout, t(sapply(rownames(coefsout), function(x) mysplit(x, 4))))

# coefs.b<-coefsout
# coefs.b$a<-as.vector(sapply(strsplit(rownames(coefsout),"[[]",fixed=FALSE), "[", 2))
# coefs.b$b1<-as.vector(sapply(strsplit(coefs.b$a,"\\D",fixed=FALSE), "[", 1))
# coefs.b$b2<-coefs.b$a[,1]
# coefs.b$b3<-as.vector(sapply(strsplit(coefs.b$a,"\\D",fixed=FALSE), "[", 1))

coefsout.asite<-coefsout[coefsout$Type=="aSite",]
coefsout.asite$siteNum<-c(rep(1:jags.dat$nsite,4))
coefsout.asite$seedT<-c(rep(1,jags.dat$nsite),rep(2,jags.dat$nsite),rep(1,jags.dat$nsite),rep(2,jags.dat$nsite))
coefsout.asite$scarT<-c(rep(1,jags.dat$nsite*2),rep(2,jags.dat$nsite*2))
coefsout.asite$site<-plotDatasub$site[match(coefsout.asite$siteNum,plotDatasub$siteNum)]

coefsout.asiteBT<-coefsout[coefsout$Type=="aSiteBT",]
#coefsout.asiteBT$siteNum<-c(rep(1:jags.dat$nsite,each=4))
coefsout.asiteBT$siteNum<-c(rep(1,4),rep(10:15,each=4),rep(2:9,each=4))
coefsout.asiteBT$seedT<-c(rep(1:2,each=2,times=jags.dat$nsite))
coefsout.asiteBT$scarT<-c(rep(1:2,each=1,times=(jags.dat$nsite*2)))
coefsout.asiteBT$site<-plotDatasub$site[match(coefsout.asiteBT$siteNum,plotDatasub$siteNum)]

coefsout.muEmergeBT<-coefsout[coefsout$Type=="muEmergeBT",]
coefsout.muEmergeBT$siteNum<-999
coefsout.muEmergeBT$seedT<-c(rep(1:2,each=2))
coefsout.muEmergeBT$scarT<-c(rep(1:2,each=1,times=2))
coefsout.muEmergeBT$site<-"All"

coefsout.asiteBT<-rbind(coefsout.asiteBT,coefsout.muEmergeBT)

plot.Teff<-ggplot(coefsout.asiteBT)+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_point(aes(x=site,y=`50%`,colour=factor(seedT):factor(scarT)),size=6,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=factor(seedT):factor(scarT)),width=0.18,size=1.8,position=position_dodge(width=0.5))+scale_colour_manual(values=c("black","orange","blue","darkgreen"),name="Treatment",breaks=c("1:1","1:2","2:1","2:2"),labels=c("control","scarified","seeded","seeded+scarified"))+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect Coefficient\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggsave(plot.Teff,filename = "figures/treatment_effect_poisson_overdisp_interaction_bysite.pdf")

plot.Teff2<-ggplot(coefsout.muEmergeBT)+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_point(aes(x=site,y=mean,colour=factor(seedT):factor(scarT)),size=6,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=factor(seedT):factor(scarT)),width=0.18,size=1.8,position=position_dodge(width=0.5))+scale_colour_manual(values=c("black","orange","blue","darkgreen"),name="Treatment",breaks=c("1:1","1:2","2:1","2:2"),labels=c("control","scarified","seeded","seeded+scarified"))+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect Coefficient\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())

ggsave(plot.Teff,filename = "figures/treatment_effect_poisson_overdisp_interaction_overall.pdf")