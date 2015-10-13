
table(plotData$zone,paste(plotData$treatment,plotData$herb.treat,sep="_"),plotData$site)

plotData2<-plotData

plotData2<-read.csv(file.choose(),stringsAsFactors=FALSE)

plotData2$herb.treat[is.na(plotData2$herb.treat)]<-"nonex"

library(ggplot2)

#germination

for (i in unique(plotData2$site[!is.na(plotData2$germ.y1)])){
  
qplot(factor(zone),germ.y1,colour=factor(exp.seedling.sp):factor(treatment):factor(herb.treat),data=plotData2[plotData2$site==i,],geom="boxplot")+theme_bw()+theme(legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=1,size=22),axis.title.x=element_blank(),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.9),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())+geom_rug(position="jitter")+annotate("text",label=length(unique(paste(plotData2$plot[plotData2$site==i],plotData2$treatment[plotData2$site==i],plotData2$zone[plotData2$site==i],plotData2$herb.treat[plotData2$site==i],sep="_"))),size=10,x=0.8,y=-0.5)+stat_summary(aes(x=as.factor(zone),y=germ.y1,group=factor(exp.seedling.sp):factor(treatment):factor(herb.treat)),fun.y=mean,colour="red",geom="point",size=6,position=position_dodge(width=0.75),shape=5)

ggsave(filename=paste("figures/data_checking/Boxplot_cleaned",i,".pdf",sep=""),width=15)

}

#survival

for (i in unique(plotData2$site[!is.na(plotData2$surv.seedlings.yr2)])){
  
  qplot(factor(zone),surv.seedlings.yr2,colour=factor(exp.seedling.sp):factor(treatment):factor(herb.treat),data=plotData2[plotData2$site==i,],geom="boxplot")+theme_bw()+theme(legend.title=element_text(size=24,face="plain"),legend.text=element_text(size=22),legend.key = element_blank(),axis.text.x=element_text(size=20,angle=45,vjust=1,hjust=1),axis.text.y=element_text(hjust=1,size=22),axis.title.x=element_blank(),axis.title.y=element_text(angle=90,size=24,face="plain",vjust=0.9),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank())+geom_rug(position="jitter")+annotate("text",label=length(unique(paste(plotData2$plot[plotData2$site==i],plotData2$treatment[plotData2$site==i],plotData2$zone[plotData2$site==i],plotData2$herb.treat[plotData2$site==i],sep="_"))),size=10,x=0.8,y=-0.5)+stat_summary(aes(x=as.factor(zone),y=surv.seedlings.yr2,group=factor(exp.seedling.sp):factor(treatment):factor(herb.treat)),fun.y=mean,colour="red",geom="point",size=6,position=position_dodge(width=0.75),shape=5)
  
  ggsave(filename=paste("figures/data_checking/Boxplot_survival",i,".pdf",sep=""),width=15)
  
}


#FARM

ggplot(plotData2[plotData2$site==i,],aes(x=as.factor(zone),y=exp.seedling.count.y1,shape=factor(exp.seedling.sp),colour=factor(treatment):factor(herb.treat)))+geom_point(position=position_dodge(width=1),size=5)+theme_bw()+stat_summary(aes(x=as.factor(zone),y=exp.seedling.count.y1,group=factor(exp.seedling.sp):factor(treatment):factor(herb.treat)),fun.y=mean,colour="red",geom="point",size=6,position=position_dodge(width=1),shape=5)

siteData[siteData$site=="Wolf Creek, YK",c("site","zone","transect","start.utm.zone","start.utm.easting","start.utm.northing","start.elev","slope","aspect","sp.seeded")]

plotData[plotData$site=="Wolf Creek, YK",c("site","zone","plot","transect","utm.zone","easting","northing","exp.seedling.sp")]
