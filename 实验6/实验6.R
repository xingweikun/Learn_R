setwd('课程数据集')
MyData<-read.table(file="空气质量.txt",header=TRUE,sep=" ",stringsAsFactors = FALSE)
MyData<-subset(MyData,(MyData$date<=20160315|MyData$date>=20161115))
MyData<-na.omit(MyData)
hist(MyData$PM2.5,xlab="PM2.5浓度",ylab="密度",main="2016年北京供暖季PM2.5浓度直方图",cex.lab=0.8,freq=FALSE,ylim=c(0,0.01))
lines(density(MyData$PM2.5,na.rm=TRUE),col=2)

MyData<-read.table(file="美食餐馆食客评分数据.txt",header = TRUE,sep=" ",stringsAsFactors = FALSE)
(freqT<-table(MyData$food_type))
sum1<-addmargins(freqT)
prop.table(freqT)*100
T<-barplot(freqT,xlab="主打菜",ylab="餐馆数",main="主打菜分布柱形图")
box()
text(T,5,freqT,col=2)

pie(freqT)
Pct<-round(freqT/length(MyData$food_type)*100,3)
Lab<-sapply(dimnames(freqT), FUN=function(x) paste(x,Pct,"%",sep=" "))
pie(freqT,cex=0.8,labels=Lab,main="主打菜分布饼图",cex.main=0.8)

boxplot(iris[2],main='箱线图')
par(mfrow=c(1,2))
boxplot(iris[1:4],main='单独的箱线图')
boxplot(Sepal.Length ~ Species,data=iris,main='组间比较的箱线图')
par(mfrow=c(1,2))

plot(cars[,1],cars[,2])
plot(iris[,1:4])
