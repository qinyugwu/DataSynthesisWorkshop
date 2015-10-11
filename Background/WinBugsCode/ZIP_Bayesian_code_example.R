#
# Last revised on January 18, 2013.
# Site-specific random effects in Poisson part of the model.

setwd("C:\\Users\\Carissa.Cervus-PC\\Dropbox\\seed_substrate_synth\\")
# Load in data.
plot=read.csv("plot_BS_CBJJ_v2.csv")
site=read.csv("site_BS_CBJJ_v2.csv")

# Combine data: append latitude, elevation, moist, TSLF, BA_stdg, 
#                      resid.org, resid.CV, BS.nstand of "site" 
#               to "plot".

# dtfm for "DaTa FraMe".
dtfm<-data.frame(plot[1,], site[1,3:10])
for (i in 1:nrow(plot)){
  cond<-site$burn==plot[i,"burn"] & site$site==plot[i,"site"]
  dtfm[i,1:9]<-plot[i,1:9]
  dtfm[i,10:17]<-site[cond,3:10]
}
rm(cond, plot, site)

# burn, block, plot are factors.

# Add 100 to sites of "EP".
dtfm$site[dtfm$burn=="EP"]<-100+dtfm$site[dtfm$burn=="EP"]%%100
dtfm$site<-as.factor(dtfm$site)

# write.table(dtfm, "plotsite.txt")
# dtfm<-read.table("plotsite.txt")

# Number of observations.
nObs<-nrow(dtfm)

# Reformat "site" to 1, 2, 3, ..., 55.
siteRename<-function(site){
  site<-as.numeric(as.character(site))
  a<-unique(site); newname<-site
  for (i in 1:length(a)){
    newname[site==a[i]]<-i
  }
  return(newname)
}
site<-siteRename(dtfm$site); rm(siteRename)

# Number of sites =55.
nsite<-length(unique(site))

BSsown<-dtfm$BS.sown
BScount<-dtfm$BS.count

# Scale variables to "z score": z=(x-mean(x))/sd(x).
tmp<-as.data.frame(scale(dtfm[, 8:17]))
zlivemoss<-tmp$live.moss
zbareorg<-tmp$bare.org
zlatitude<-tmp$latitude
zelevation<-tmp$elevation
zmoist<-tmp$moist
zTSLF<-tmp$TSLF
zBAstdg<-tmp$BA_stdg
zresidorg<-tmp$resid.org
zresidCV<-tmp$resid.CV # 20 NA's.
zBSnstand<-tmp$BS.nstand


#Set-up for bugs
library(R2OpenBUGS)
model.file<-"model_zip_example.txt"
cat("
# Zip model: site-specific normal random effects in the Poisson part;
#            variables in the zero-inflation part without random effects.
# response variable BScount
# reference :Zhang et al Forest Ecology and Management(2012)
# 
model{
for (j in 1:nObs){ # j is observation index.
  zeros[j]<-0
  zeros[j]~dpois(mu[j])
  mu[j]<- -ll[j]
  z[j]<-equals(BScount[j],0)

  #log-likelihood
  ll[j]<-z[j]*log( p[j]+(1-p[j])*exp(-lambda[j]) )+(1-z[j])*(log(1-p[j])+fd[j]) 
  fd[j]<- -lambda[j]+BScount[j]*log(lambda[j])-logfact(BScount[j]) 

  # response model for count data.
  log(lambda[j])<-bb[site[j]]+beta0+beta[1]*zresidorg[j]+beta[2]*zmoist[j]+beta[3]*zlivemoss[j]+
                  beta[4]*BSsown[j]

  # zero-inflation model: p[i,j] is the prob P(extra zero)
  logit(p[j])<-alpha0+alpha[1]*zelevation[j]+alpha[2]*zmoist[j]+alpha[3]*zTSLF[j]+
               alpha[4]*BSsown[j]+alpha[5]*zlivemoss[j]
} # end of j

for (i in 1:nsite){ # i is site index.
  bb[i]~dnorm(0, taub)
} # end of i.


#priors 
beta0~dnorm(0,0.01)
alpha0~dnorm(0,0.01)
 
beta[1:4]~dmnorm(mub[1:4],Tb[1:4,1:4])
alpha[1:5]~dmnorm(mua[1:5],Ta[1:5,1:5])

taub<-pow(1/sigb,2)

sigb~dunif(0,10)
}# end of model
",file=model.file)

mua<-rep(0,5); mub<-rep(0,4)
Ta<-diag(rep(0.01, 5)); Tb<-diag(rep(0.01, 4))

data<-list("nObs", "nsite", "site", "BScount","BSsown", "zmoist","zresidorg",
           "zlivemoss", "zelevation", "zTSLF","mua", "mub", "Ta", "Tb")
parameters<-c("beta0","beta","alpha0","alpha","sigb")

inits<-function(){
  list(beta0=0,beta=rep(0,4),alpha0=0, alpha=rep(0, 5))
}
#find burn-in, thinning rate
begin<-Sys.time()
zip.b<-bugs(data,inits,parameters,model.file,n.chains=1,n.iter=20000,n.burn=10000,n.thin=50,
     debug=TRUE,codaPkg=TRUE)
print(Sys.time()-begin)
obj<-read.bugs(zip.b)
require(coda)
# codamenu()#choose 2: Use an mcmc object-> Enter obj
            #choose 2: Diagnostics
            #choose 3: Raftery and Lewis





