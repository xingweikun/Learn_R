setwd("./课程数据集")
MyData<-read.table(file="空气质量.txt",header=TRUE,sep=" ",stringsAsFactors=FALSE)
Data<-subset(MyData,(MyData$date<=20160315|MyData$date>=20161115))   #仅分析供暖季的空气质量数据
Data<-na.omit(Data)   #完整观测
library("psych")
describe(Data$PM2.5,IQ=TRUE)
par(mfrow=c(1,2))
hist(Data$PM2.5,xlab="PM2.5",ylab="密度",main="PM2.5直方图",cex.lab=0.8,freq=FALSE,ylim=c(0,0.01))
lines(density(Data$PM2.5,na.rm=TRUE),col=2)
boxplot(Data$PM2.5,horizontal=TRUE,main="供暖季的PM2.5箱线图",xlab="PM2.5",cex.lab=0.8)
dotchart(Data$PM2.5,main = "PM2.5克利夫兰图",cex.main=0.8,xlab="PM2.5",ylab="观测编号",cex.lab=0.8)
abline(v=mean(Data$PM2.5),col=2)
(InDiff<-quantile(Data$PM2.5,0.75)-quantile(Data$PM2.5,0.25))
(threshold<-(quantile(Data$PM2.5,0.75)+1.5*InDiff))
exData<-Data[Data$PM2.5>threshold,]
length(unique(exData$date))
sort(table(exData[,"SiteName"]) ,decreasing=TRUE)
unique(exData[, "SiteName"])

