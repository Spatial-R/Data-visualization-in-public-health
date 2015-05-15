setwd("E:/MultiBar")
##################### multibar ######################
library(plyr);library(reshape);library(ggplot2);library(showtext)
aqi<-read.csv("aqi.csv",header=T,stringsAsFactors = F);
site<-read.csv("site.csv",header=T,stringsAsFactors = F);
aqi$city<-factor(aqi$city,levels=c(1:113),labels=site$城市)
aqi$district<-factor(aqi$district,levels=c(1:7),labels=c("东北","华北",
                      "华东","华中","华南","西南","西北"))
mt<-aqi[aqi$season==3,-3];mt[,3:9]<-apply(mt[,3:9],2,as.numeric)


source("E:/R软件知识/R软件作图/多个条形图可视化---66个都可以/多个条形图.R")

df1=melt(mt,c("city","district"),variable_name = "score");

names(df1)=c("item","family","score","value")
df1$score<-factor(df1$score,levels=c("NO2","PM2.5","SO2","PM10","Good","O3","CO"))
#df1[,1:2]<-apply(df1[,1:2],2,as.character);

p1<-polarHistogram(df1,circleProportion=0.95,familyLabels=F)
font.add("simhei","simhei.ttf")     ########### font.files()
cairo_pdf(filename="3season.pdf",height=14.27,width=18.69,pointsize=14)
showtext.begin();print(p1);showtext.end();dev.off()


