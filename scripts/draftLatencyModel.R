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
