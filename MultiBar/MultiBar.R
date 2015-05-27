setwd("E:/Github/Data-visualization-in-public-health/MultiBar")
##################### multibar ######################
library(plyr);library(reshape);library(ggplot2);library(showtext)
aqi<-read.csv("aqi.csv",header=T,stringsAsFactors = F);
site<-read.csv("site.csv",header=T,stringsAsFactors = F);
aqi$city<-factor(aqi$city,levels=c(1:113),labels=site$城市)
aqi$district<-factor(aqi$district,levels=c(1:7),labels=c("东北","华北",
                      "华东","华中","华南","西南","西北"))

mt<-aqi[aqi$season==4,-3];mt[,3:9]<-apply(mt[,3:9],2,as.numeric)


source("多个条形图.R")

df1=melt(mt,c("city","district"),variable_name = "score");

names(df1)=c("item","family","score","value")
df1$score<-factor(df1$score,levels=c("NO2","PM2.5","SO2","PM10","优","O3","CO"))
#df1[,1:2]<-apply(df1[,1:2],2,as.character);

p1<-polarHistogram(df1,circleProportion=0.95,familyLabels=T)
font.add("simhei","simhei.ttf")     ########### font.files()
tiff(filename ="4.tiff",width=12,height=12,units="cm",
     res=400,pointsize=8,type="cairo")
showtext.begin();print(p1);showtext.end();dev.off()


