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



