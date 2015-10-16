#READ DATA

plotDatasub<-read.csv('data/subsets/plotDatasub.csv')

plotDatasub$seeds.per.plot[plotDatasub$site=="Craigieburn"]<-150
plotDatasub$seeds.per.plot[plotDatasub$site=="Tesso"]<-300
plotDatasub$seeds.per.plot[plotDatasub$site=="Texas Creek"]<-100

#get rid of any sites missing data entirely - this will exclude Churchill
# for now

plotDatasub<-plotDatasub[!is.na(plotDatasub$germ.y1),]

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

#data description
#y = response variable, such as count of germinants in a particular year <vector,length =n>
#nsite = number of sites included in analysis <scalar>
#n = total number of data points (rows in data) <scalar>
#germrate = estimate of seed viability in lab.
#seedT = seeding treatment per record, 1= seeded, 0 = not seeded.<vector,length =n>
#scarT = seeding treatment per record, 1= seeded, 0 = not seeded.<vector,length =n>
#numSeeded -><vector,length =n> number of seeds added in each plot of each species
#siteData = <vector,length =n> siteID for each record


#SCE need to move the indenting
write("
  model{
    for (i in 1:n){ #i indexes over data rows
    #data level - SCE note could make this overdispersed if necessary
    y[i]~dpois(mu[i]) 
    #log(mu[i])<-max(-20,min(20,eta[i]))
    #predicted number is the sum of those recruited from added seeds +
    #the background rate from nonexperimentally added
    #mixture
    mu[i]<-exp(numTrtRecruit_trunc[i]) + exp(numBackground_trunc[i] + e[i])
    e[i]~dnorm(0,tau.resid)#mixture
  
    #expectation for the number coming up from seeding
    #help with convergence
    numTrtRecruit_trunc[i]<-max(-20, min(20, numTrtRecruit[i]))
    numTrtRecruit[i]<-bSeed[siteData[i]]*seedT[i]+
    cInteract[siteData[i]]*scarT[i]*seedT[i]+
        log(trials[i])*seedT[i]
    numBackground_trunc[i]<-max(-20, min(20, numBackground[i]))
    #expectation for the number coming up w/o seeding
    numBackground[i]<-aSite[siteData[i]]+ bScar[siteData[i]]*scarT[i]
  
    #estimate of viable seed intensity
    trials[i]<-numSeeded[i]*germRate[siteData[i]]#make this not fixed? 
    #numBackground[j]<-muBackgroundsite[siteData[j]]

    #no longer used - binomial option that caused jags unhappiness
    #when we modeled y as a sum
  #numTrtRecruitALL[i]<-numTrtRecruit[i]*seedT[i] #this will cause the numTrtRecruit not to enter the likelihood for the nonseeded plots
    #numTrtRecruit[i]~dbinom(emergRate[plot[i]], trials[i]) 
    }

    
    #num that recruited from the seed treatment addition is a function of
    #the number of trials (viableseeds added - so result is in per viable seed added)
    #note you can have numSeeded in here for ctls - it must be nonzero for the calculation
    # to run but doesn't enter the likelihood
    #plus asite which is the intercept for seed addition (log scale)
    #plus bscarT which is the addition amt of recruitment per seed added by doing scarification
    # the multiplication by seedT will make this term 0 out for nonseeded plots.

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

#Derived
#Discrepancy measures
      for (i in 1:n){
      yNew[i]~dpois(mu[i])
      PRes[i]<-(y[i]-mu[i])/sqrt(mu[i])
      PResNew[i]<-(yNew[i]-mu[i])/sqrt(mu[i])
      D[i]<-(PRes[i]*PRes[i])
      DNew[i]<-(PResNew[i]*PResNew[i])
      
      #SCE is not sure how you calculate expY now that the
      # overdispersion term is in there, this might not be quite right
      # the line is copied from the example Zuur model WITHOUT overdispersion
      # we might want something other than tauEmerge here??
      ExpY[i]<-exp(mu[i])*exp(tauEmerge/2)
      
      VarY[i]<-exp(mu[i])*(exp(mu[i])*(exp(tauEmerge)-1)*exp(tauEmerge)+exp(tauEmerge/2))
      PResEQ[i]<-(y[i]-ExpY[i])/sqrt(VarY[i])
      Disp1[i]<-(PResEQ[i]*PResEQ[i])
      
      }
      
      Dispersion<-sum(Disp1) 
      Fit<-sum(D)
      FitNew<-sum(DNew)
      
      #predictions scaled to an avg treatment of 100 viable seeds
     for (k in 1:nsite){
        for (l in 1:2){#seeding
          for (m in 1:2){#scarification
            predVals[k,l,m]<-exp(aSite[k]+bScar[k]*(m-1))+exp(bSeed[k]*(l-1)+
    cInteract[k]*(m-1)*(l-1)+
        log(100)*(l-1))
          }
        }
      }

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


paramsBT<-NULL
for (k in 1:jags.dat$nsite){
  for (l in 1:2){
    for (m in 1:2){
      out<-as.vector(paste("predVals[",k,",",l,",",m,"]",sep=""))
      paramsBT<-c(paramsBT,out)
    }}}

params<-c("muEmerge", "sigmaEmerge","aSite", "bSeed", "bScar", "cInteract",
          "sigmaResid",#"Dispersion",
          "Fit",
          "FitNew","muScar", "muInteract", "muSeed", "sigmaSeed", "sigmaInteract",
          "sigmaScar", paramsBT)





library(rjags)
library(R2jags)
modout.gtree<-jags(jags.dat,inits=NULL, params, model.file="gtree_y1germ.jags", n.chains=3,n.iter=10000,n.burnin=2000, n.thin=2, DIC=TRUE, working.directory=NULL, progress.bar = "text")

print(modout.gtree)
plot(modout.gtree)

#bayesian p value not super excellent, but not as bad as some?
mean(modout.gtree$BUGSoutput$sims.list$FitNew>modout.gtree$BUGSoutput$sims.list$Fit)

coefsout<-as.data.frame(modout.gtree$BUGSoutput$summary[,c('mean','sd','2.5%','97.5%')])

#get coefficient name (before the square brackets)
coefsout$Type<-as.vector(sapply(strsplit(rownames(coefsout),"[[]",fixed=FALSE), "[", 1))

source ('scripts/split_function.R')

coefsout<-cbind.data.frame(coefsout, t(sapply(rownames(coefsout), function(x) mysplit(x, 3, F))))
names(coefsout)[names(coefsout)=='1']<-'siteNum'
names(coefsout)[names(coefsout)=='2']<-'scarT'
names(coefsout)[names(coefsout)=='3']<-'seedT'

coefsout$site<-plotDatasub$site[match(coefsout$siteNum,plotDatasub$siteNum)]
library (ggplot2)
scarTplot<-ggplot(coefsout[coefsout$Type %in% c("bScar"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  #this is the coefsout mean effects
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=coefsout$`97.5%`[coefsout$Type=="muScar"],
                  ymin=coefsout$`2.5%`[coefsout$Type=="muScar"]),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment Effect\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())
ggsave(scarTplot, filename = "figures/scarT_effect_poisson_latency_interaction_bysite.pdf")

#
natGermplot<-ggplot(coefsout[coefsout$Type %in% c("aSite"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  #this is the coefsout mean effects
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=coefsout$`97.5%`[coefsout$Type=="muEmerge"],
                  ymin=coefsout$`2.5%`[coefsout$Type=="muEmerge"]),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Natural Germination\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())
ggsave(natGermplot, filename = "figures/naturalGermination_est_poisson_latency_interaction_bysite.pdf")

#
seedTplot<-ggplot(coefsout[coefsout$Type %in% c("bSeed"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  #this is the coefsout mean effects
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=coefsout$`97.5%`[coefsout$Type=="muSeed"],
                  ymin=coefsout$`2.5%`[coefsout$Type=="muSeed"]),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment effect\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())
ggsave(seedTplot, filename = "figures/sweedTT_effect_poisson_latency_interaction_bysite.pdf")

InteractionPlot<-ggplot(coefsout[coefsout$Type %in% c("cInteract"),])+
  geom_point(aes(x=site,y=mean),size=6)+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=Type),width=0.18,size=1.8,position=position_dodge(width=0.5))+
  geom_hline(yintercept=0,linetype="dotted")+
  #this is the coefsout mean effects
  geom_ribbon(aes(x=as.numeric(factor(site)),ymax=coefsout$`97.5%`[coefsout$Type=="muInteract"],
                  ymin=coefsout$`2.5%`[coefsout$Type=="muInteract"]),alpha=0.2,fill="red")+
  theme_bw()+xlab("\nSITE")+ylab("Treatment effect\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=24,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())
ggsave(InteractionPlot, filename = "figures/Interaction_effect_poisson_latency_interaction_bysite.pdf")


#plots of expected values on the raw data scale
#but make log y axis
base_breaks <- function(n = 10){
  function(x) {
    axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
  }
}

plot.Teff<-ggplot(coefsout[coefsout$Type=="predVals",])+
  geom_hline(yintercept=0,linetype="dotted")+
  geom_point(aes(x=site,y=mean,colour=factor(seedT):factor(scarT)),size=6,position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=site,ymin=`2.5%`,ymax=`97.5%`,colour=factor(seedT):factor(scarT)),width=0.18,size=1.8,position=position_dodge(width=0.5))+scale_colour_manual(values=c("black","orange","blue","darkgreen"),name="Treatment",breaks=c("1:1","1:2","2:1","2:2"),labels=c("control","scarified","seeded","seeded+scarified"))+
  theme_bw()+xlab("\nSITE")+ylab("Estimated # seedlings \n w/100 added seeds for seeded plots)\n")+theme(legend.title=element_text(size=24,face="bold"),legend.text=element_text(size=20),legend.position="right",legend.key = element_rect(colour = "white"),axis.text.x=element_text(size=22,angle=45,hjust=1),axis.text.y=element_text(hjust=0,size=22),axis.title.x=element_text(size=24,face="bold"),axis.title.y=element_text(angle=90,size=20,face="bold",vjust=0.3),axis.ticks = element_blank(),panel.grid.minor=element_blank(), panel.grid.major=element_blank())+
  scale_y_continuous(trans = 'log', breaks = base_breaks(),labels = prettyNum) + 
  theme(panel.grid.minor = element_blank())






###########################################################
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
