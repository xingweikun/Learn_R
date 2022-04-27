set.seed(12345)
Pop<-rnorm(100000,mean=4,sd=2)     
MeanX<-vector()
for(i in 1:2000){
  x<-sample(Pop,size=1000,replace=TRUE)
  MeanX<-c(MeanX,mean(x))
}
plot(density(MeanX),xlab="样本均值",ylab="密度",main="样本均值的抽样分布",cex.main=0.8,cex.lab=0.8)
points(mean(MeanX),sd(MeanX),pch=1,col=1)   
points(4,sqrt(2^2/1000),pch=2,col=2)   


set.seed(12345)
x<-rnorm(1000,0,1)
Ord<-order(x,decreasing=FALSE)
x<-x[Ord]
y<-dnorm(x,0,1)
plot(x,y,type="l",ylab="密度",main="标准正态分布与不同自由度下的t分布密度函数",lwd=1.5)
df<-c(1,5,10,30)
for(i in 1:4){
  x<-rt(1000,df)       
  Ord<-order(x,decreasing=FALSE)
  x<-x[Ord]
  y<-dt(x,df[i])
  lines(x,y,lty=i+1)
}
legend("topright",title="自由度",c("标准正态分布",df),lty=1:5)


MyData<-read.table(file="空气质量.txt",header=TRUE,sep=" ",stringsAsFactors=FALSE)
Data<-subset(MyData,(MyData$date<=20160315|MyData$date>=20161115))  
Data<-na.omit(Data)
t.test(Data$PM2.5,mu=95,alternative="less")  