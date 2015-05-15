library(dplyr)
library(ggplot2)
library(reshape2)
library(grid)
library(plotrix)
library(gridExtra)
library(Cairo)
###################################  初婚和年龄 ############################

dat<-read.csv("年龄与初婚.csv",header=T)
dat.fam<-dat[-c(1, seq(3,51,by=6),57),c(1,seq(6,84,by=3))]
dat.wom<-dat[-c(1, seq(3,51,by=6),57),c(1,seq(7,85,by=3))]
names(dat.wom)<-names(dat.fam)<-c("age",14:40);
dat.fplot<-melt(dat.fam,id="age");dat.wplot<-melt(dat.wom,id="age");
dat.fplot$gentle<-rep("Male",1242);dat.wplot$gentle<-rep("Female",1242);
dat.total<-rbind(dat.fplot,dat.wplot)

CairoPDF("age and marriage.pdf",heigh=8.27,width=12.69,pointsize = 10)
ggplot(dat.total,aes(x=age,y=variable))+geom_tile(aes(fill=value))+
  scale_fill_continuous(limits=c(174,225893),low = "white", high="#008B00",na.value="white")+
  theme_bw(base_size = 14,base_family="serif")+labs(x ="Date", y = "Cities") +
  xlab("Age")+ylab("Age at first marriage")+facet_wrap(~gentle,ncol=2)+
  scale_y_discrete(breaks=c(15,20,25,30,35,40), labels=c("15","20","25","30","35",">=40"))+
  scale_x_discrete(breaks=c(20,25,35,45,55,65), labels=c("20","25","35","45","55",">=65"))+
  theme( axis.ticks = element_blank(),axis.text.x = element_text(angle =0, hjust = 0.5), 
         legend.title = element_text(face="plain",hjust=2),
         legend.text.align=0.5,legend.margin=unit(0.1,"cm"),
         axis.ticks.margin=unit(0.1,"cm"),axis.ticks.length = unit(0,"cm"))+
  guides(fill=guide_colourbar(title="", barheight=10,barwidth=3))
dev.off()

###############################  教育和婚姻 #########################
############################## 总体情况  #############################

dat<-read.csv("教育与婚姻.csv",header=T)
dat.total<-dat[-c(1,seq(2,56,by=6),63:496),c(1:13)]
dat.total<-mutate(dat.total,nom=未婚总数/总数15,nomm=未婚男/男15,nomw=未婚女/女15,
                  po=配偶总数/总数15,pom=配偶男/男15,pow=配偶女/女15,lh=离婚总数/总数15,
                  lhm=100*离婚男/男15,lhw=1000*离婚女/女15,fwb=未婚男/未婚女)

dat.2<-melt(dat.total[-51,c(1,6:7)]);names(dat.2)<-c("age","lb","pop")

p1<-ggplot(dat.2,aes(age,pop,group=lb,fill=lb))+
  geom_bar(stat="identity",position="dodge",width=0.5)+  
  theme_bw(base_size =14,base_family="serif")+xlab("Age")+ylab("Population not marriaged")+
  scale_x_discrete(breaks=seq(15,65,by=2))+scale_fill_manual(values=c("#8A2BE2","limegreen"),labels=c("Man","Woman"))+
  guides(fill=guide_legend(title="", barheight=10,barwidth=3))

p2<-ggplot(dat.total[-51,c(1,23)],aes(x=教育程度,y=fwb))+geom_point(color="#9A32CD")+
  theme_bw(base_size=14,base_family="serif")+xlab("")+ylab("Ratio")+
  scale_x_discrete(breaks=seq(15,65,by=2))+theme(
    plot.margin = unit(c(3,38,-5,13), "mm"))
pp1 <- ggplot_gtable(ggplot_build(p1));pp2 <- ggplot_gtable(ggplot_build(p2))
grid.arrange(pp2,pp1,ncol=1,heights=c(3/8,5/8))

################################### 本科生 #####################################
dat.total<-dat[-c(1:373,seq(374,428,by=6),434:496),c(1:13)]
dat.total<-mutate(dat.total,nom=未婚总数/总数15,nomm=未婚男/男15,nomw=未婚女/女15,
                  po=配偶总数/总数15,pom=配偶男/男15,pow=配偶女/女15,lh=离婚总数/总数15,
                  lhm=100*离婚男/男15,lhw=1000*离婚女/女15,fwb=未婚男/未婚女)

dat.2<-melt(dat.total[-51,c(1,6:7)]);names(dat.2)<-c("age","lb","pop")

p1<-ggplot(dat.2,aes(age,pop,group=lb,fill=lb))+
  geom_bar(stat="identity",position="dodge",width=0.5)+  
  theme_bw(base_size =14,base_family="serif")+xlab("Age")+ylab("Population not marriaged")+
  scale_x_discrete(breaks=seq(15,65,by=2))+scale_fill_manual(values=c("#8A2BE2","limegreen"),labels=c("Man","Woman"))+
  guides(fill=guide_legend(title="", barheight=10,barwidth=3))

p2<-ggplot(dat.total[-51,c(1,23)],aes(x=教育程度,y=fwb))+geom_point(color="#9A32CD")+
  theme_bw(base_size=14,base_family="serif")+xlab("")+ylab("Ratio")+
  scale_x_discrete(breaks=seq(15,65,by=2))+theme(
    plot.margin = unit(c(3,38,-5,10), "mm"))
pp1 <- ggplot_gtable(ggplot_build(p1));pp2 <- ggplot_gtable(ggplot_build(p2))
grid.arrange(pp2,pp1,ncol=1,heights=c(3/8,5/8))



################################### 研究生 #####################################
dat.total<-dat[-c(1:435,seq(436,490,by=6),496),c(1:13)]
dat.total<-mutate(dat.total,nom=未婚总数/总数15,nomm=未婚男/男15,nomw=未婚女/女15,
                  po=配偶总数/总数15,pom=配偶男/男15,pow=配偶女/女15,lh=离婚总数/总数15,
                  lhm=100*离婚男/男15,lhw=1000*离婚女/女15,fwb=未婚男/未婚女)

dat.2<-melt(dat.total[-51,c(1,6:7)]);names(dat.2)<-c("age","lb","pop")

p1<-ggplot(dat.2,aes(age,pop,group=lb,fill=lb))+
  geom_bar(stat="identity",position="dodge",width=0.5)+  
  theme_bw(base_size =14,base_family="serif")+xlab("Age")+ylab("Population not marriaged")+
  scale_x_discrete(breaks=seq(15,65,by=2))+scale_fill_manual(values=c("#8A2BE2","limegreen"),labels=c("Man","Woman"))+
  guides(fill=guide_legend(title="", barheight=10,barwidth=3))

p2<-ggplot(dat.total[-51,c(1,23)],aes(x=教育程度,y=fwb))+geom_point(color="#9A32CD")+
  theme_bw(base_size=14,base_family="serif")+xlab("")+ylab("Ratio")+
  scale_x_discrete(breaks=seq(15,65,by=2))+theme(
    plot.margin = unit(c(3,38,-5,10), "mm"))
pp1 <- ggplot_gtable(ggplot_build(p1));pp2 <- ggplot_gtable(ggplot_build(p2))
grid.arrange(pp2,pp1,ncol=1,heights=c(3/8,5/8))



#########################  region and marriage ###############################
dat<-read.csv("地区和婚姻.csv",header=T)
dat.total<-dat[-1,c(1:13)]
dat.total<-mutate(dat.total,nom=notmtotal/total15,nomm=notmman/total15man,nomw=notmwom/total15fe,
                  po=poutotal/total15,pom=pouman/total15man,pow=pouwom/total15fe,lh=lihtotal/total15,
                  lhm=1000*lihunnan/total15man,lhw=1000*lihunwom/total15fe,fwb=notmman/notmwom)

pyramid.plot(dat.total$nomm,dat.total$nomw,labels=dat.total$region)


dat.2<-melt(dat.total[,c(1,6:7)]);names(dat.2)<-c("region","lb","pop")

CairoPDF("char_3.7_2.pdf",heigh=8.27,width=12.69,pointsize = 10)
p1<-ggplot(dat.2,aes(region,pop,group=lb,fill=lb))+
  geom_bar(stat="identity",position="dodge",width=0.5)+  
  theme_bw(base_size =12,base_family="serif")+xlab("")+ylab("Population not marriaged")+
  scale_fill_manual(values=c("#8A2BE2","limegreen"),labels=c("Man","Woman"))+
  guides(fill=guide_legend(title="", barheight=10,barwidth=3))

p2<-ggplot(dat.total[,c(1,23)],aes(x=region,y=fwb))+geom_point(color="#9A32CD")+
  theme_bw(base_size=13,base_family="serif")+xlab("")+ylab("Ratio")+theme(
    plot.margin = unit(c(5,28,-5,6), "mm"))
pp1 <- ggplot_gtable(ggplot_build(p1));pp2 <- ggplot_gtable(ggplot_build(p2))
grid.arrange(pp2,pp1,ncol=1,heights=c(3/8,5/8))
dev.off()

################################## 初婚和初婚年龄 ###############################

dat<-read.csv("初婚和初婚年份.csv",header=T)
dat.man<-dat[-c(1,seq(3,45,by=3),46),c(1,seq(6,97,by=3))]
dat.wom<-dat[-c(1,seq(3,45,by=3),46),c(1,seq(7,97,by=3))]
names(dat.man)<-names(dat.wom)<-c("age",1980:2010)
dat.mplot<-melt(dat.man,id="age");dat.wplot<-melt(dat.wom,id="age")
dat.mplot$gentle<-rep("Male",899);dat.wplot$gentle<-rep("Female",899);
dat.total<-rbind(dat.mplot,dat.wplot)

man.me<-data.frame(year=1980:2010,age=as.numeric(dat[46,c(seq(6,97,by=3))]),gentle=rep("Male",31))
wom.me<-data.frame(year=1980:2010,age=as.numeric(dat[46,c(seq(7,97,by=3))]),gentle=rep("Female",31))
mean.total<-rbind(wom.me,man.me)

CairoPDF("age and year.pdf",heigh=8.27,width=12.69,pointsize = 10)
p1<-ggplot(dat.total,aes(y=age,x=variable))+geom_tile(aes(fill=value))+
  scale_fill_continuous(limits=c(0,208292),low = "white", high="#008B00",na.value="white")+
  theme_bw(base_size = 14,base_family="serif")+labs(x ="Date", y = "Cities") +
  xlab("Age")+ylab("Age at first marriage")+facet_wrap(~gentle, ncol = 2)+
  scale_x_discrete(breaks=seq(1980,2010,by=3))+
  scale_y_discrete(breaks=c(20,25,35,45,55,65), labels=c("20","25","35","45","55",">=65"))+
  theme( axis.ticks = element_blank(),axis.text.x = element_text(angle =0, hjust = 0.5), 
         legend.title = element_text(face="plain",hjust=2),
         legend.text.align=0.5,legend.margin=unit(0.1,"cm"),
         axis.ticks.margin=unit(0.1,"cm"),axis.ticks.length = unit(0,"cm"))+
  guides(fill=guide_colourbar(title="", barheight=10,barwidth=3))

p2<-ggplot(mean.total,aes(y=age,x=year))+geom_point(color="#9A32CD")+
  theme_bw(base_size = 13,base_family="serif")+xlab("")+ylab("Mean age at first marriage")+
  scale_x_continuous(breaks=seq(1980,2010,by=3))+facet_wrap(~gentle,ncol=2)+
  theme( axis.ticks = element_blank(),axis.text.x = element_text(angle =0, hjust = 0.5), 
         plot.margin = unit(c(5,40,-5,4), "mm"),
         legend.title = element_text(face="plain",hjust=2),
         legend.text.align=0.5,legend.margin=unit(0.1,"cm"),
         axis.ticks.margin=unit(0.1,"cm"),axis.ticks.length = unit(0,"cm"))
  
pp1 <- ggplot_gtable(ggplot_build(p1));pp2 <- ggplot_gtable(ggplot_build(p2))
grid.arrange(pp2,pp1,ncol=1,heights=c(3/8,5/8))
dev.off()
