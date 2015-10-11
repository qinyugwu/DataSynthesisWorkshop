# Mixed models

head(plotDatasub,1)

library(lme4)

m1<-glmer(exp.seedling.count.y1~seedT*scarT+(1|site),family="poisson",data=plotDatasub)
summary(m1)
