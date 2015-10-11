# Mixed models

head(plotDatasub,1)

library(lme4)

m1.graph<-glmer(exp.seedling.count.y1~as.factor(seedT)+as.factor(scarT)+(1|site),family="poisson",data=plotDatasub,control=glmerControl(optimizer="bobyqa"))

summary(m1.graph)
plot(m1.graph)

pred.m1<-expand.grid(seedT=c("0","1"),scarT=c("0","1"),exp.seedling.count.y1=0)

matrix.m1=model.matrix(terms(m1.graph),pred.m1)
pred.m1$exp.seedling.count.y1=matrix.m1 %*% fixef(m1.graph)
head(pred.m1)

pvar.m1<-diag(matrix.m1 %*% tcrossprod(vcov(m1.graph),matrix.m1))

pred.m1<-data.frame(pred.m1,plo=pred.m1$exp.seedling.count.y1-2*sqrt(pvar.m1),phi=pred.m1$exp.seedling.count.y1+2*sqrt(pvar.m1))

pred.m1$exp.countBT<-exp(pred.m1$exp.seedling.count.y1)
pred.m1$ploBT<-exp(pred.m1$plo)
pred.m1$phiBT<-exp(pred.m1$phi)

ggplot(pred.m1)+
  layer(data=pred.m1,
        geom='point',mapping=aes(x=seedT,y=exp.countBT,colour=scarT),alpha=0.9,size=5,position=position_dodge(0.9))+
  layer(data=pred.m1,
        geom='errorbar',mapping=aes(
          x=seedT,ymin=ploBT,ymax=phiBT,colour=scarT),alpha=0.9,width=0.1,size=2,position=position_dodge(0.9))+theme_bw()+scale_colour_manual(values=c("blue","red3"))+ylab("Survival (Oxyria at Migration, 2013)\n ")+xlab(" \nSource Site")+theme(legend.title=element_text(size=19,face="plain"),legend.text=element_text(size=18),legend.key.size=unit(0.8,"cm"),axis.text.x=element_text(size=18),axis.text.y=element_text(hjust=0,size=18),axis.title.x=element_text(size=22,face="plain"),axis.title.y=element_text(angle=90,size=22,face="plain"))