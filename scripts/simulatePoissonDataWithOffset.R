#quick simulation on how offset works
# to convince ourselves this is a reasonable alternative to the binomial model

#if your have a treatment that makes 10% more of the added seeds germinate
#expt 1=100 seeds added
#expt2 = 200 seeds added

dat1=data.frame(trtmt=c(rep('CTL', 10000), rep('TRT', 10000)),
                val=c(rpois(10000,100), rpois(10000, 100*1.1)), nsds=rep(100, 20000))

dat2=data.frame(trtmt=c(rep('CTL', 10000), rep('TRT', 10000)),
                val=c(rpois(10000,50), rpois(10000, 50*1.1)), nsds=rep(50, 20000))

#model and compare fits to get back true estimate
#puts the treatment on the same scale of percentage increase per 100 seeds
#added
summary(glm(val~offset(log(nsds))+trtmt, data=dat1, family='poisson'))
exp(coef(glm(val~offset(log(nsds))+trtmt, data=dat1, family='poisson'))[2])


summary(glm(val~offset(log(nsds))+trtmt, data=dat2, family='poisson'))
exp(coef(glm(val~offset(log(nsds))+trtmt, data=dat2, family='poisson'))[2])


#predictions in terms of # of seeds gained per 100 added
# to get the percentage, need to exponentiate the coefficient




