
library("foreign")
d<-read.spss("ALSPAC.sav",to.data.frame=T, use.value.labels = F)

#age and puberty assessment
y<-cbind(d$pub195,d$pub295,d$pub397a,d$pub497a,d$pub597a,d$pub697a,d$pub797a,d$pub897a,d$pub997a)
y2<-cbind(d$pub295,d$pub397a,d$pub497a,d$pub597a,d$pub697a,d$pub797a,d$pub897a,d$pub997a)
y3<-cbind(d$pub397a,d$pub497a,d$pub597a,d$pub697a,d$pub797a,d$pub897a,d$pub997a)

resul<-data.frame(matrix(nrow=nrow(d),ncol=30))
s<-data.frame(matrix(nrow=nrow(d),ncol=ncol(y)))


x<- cbind(d$pub150,d$pub250,d$pub350,d$pub450,d$pub550,d$pub650,d$pub750,d$pub850,d$pub950)

x<-(x-1)/4

x[which(x>1)]<-NA



f <-function(x,b,c,d) d/(1+2.71^(-b*(x-c)))
min.RSS <- function(data, par) 
sum((f(data[,1],par[1],par[2],par[3])- data[,2])^2)


for (i in 1:nrow(x)){
xx<-t(rbind(x[i,],y[i,]))

xx[which(xx[,1]>5),1]<-NA

if (nrow(na.omit(xx))>0){
xx<-na.omit(xx)


if (nrow(xx)>1){
xx[,1]<-(xx[,1])






yy<-cbind(xx[,2],xx[,1])

#if (sd(yy[,2]>0)){
tryCatch(result <- optim(par = c(0.18,145,0.95), lower=c(0, 50, 0.95),upper=c(3, 220, 1),control = list(maxit = 5000), min.RSS, data = yy[,1:2],method = "L-BFGS-B"))
resul[i,1]<-result$par[2]
resul[i,2]<-result$par[1]
resul[i,3]<-result$par[3]



#resul[i,3]<-cor(yy[,1],f(yy[,1],result$par[1],result$par[2],result$par[3]))


resul[i,4]<-f(36,result$par[1],result$par[2],result$par[3])
resul[i,5]<-f(42,result$par[1],result$par[2],result$par[3])
resul[i,6]<-f(48,result$par[1],result$par[2],result$par[3])
resul[i,7]<-f(54,result$par[1],result$par[2],result$par[3])
resul[i,8]<-f(60,result$par[1],result$par[2],result$par[3])
resul[i,9]<-f(66,result$par[1],result$par[2],result$par[3])
resul[i,10]<-f(72,result$par[1],result$par[2],result$par[3])
resul[i,11]<-f(78,result$par[1],result$par[2],result$par[3])
resul[i,12]<-f(84,result$par[1],result$par[2],result$par[3])
resul[i,13]<-f(90,result$par[1],result$par[2],result$par[3])
resul[i,14]<-f(96,result$par[1],result$par[2],result$par[3])
resul[i,15]<-f(102,result$par[1],result$par[2],result$par[3])
resul[i,16]<-f(108,result$par[1],result$par[2],result$par[3])
resul[i,17]<-f(114,result$par[1],result$par[2],result$par[3])
resul[i,18]<-f(120,result$par[1],result$par[2],result$par[3])
resul[i,19]<-f(126,result$par[1],result$par[2],result$par[3])
resul[i,20]<-f(132,result$par[1],result$par[2],result$par[3])
resul[i,21]<-f(138,result$par[1],result$par[2],result$par[3])
resul[i,22]<-f(144,result$par[1],result$par[2],result$par[3])
resul[i,23]<-f(150,result$par[1],result$par[2],result$par[3])
resul[i,24]<-f(156,result$par[1],result$par[2],result$par[3])
resul[i,25]<-f(162,result$par[1],result$par[2],result$par[3])
resul[i,26]<-f(168,result$par[1],result$par[2],result$par[3])
resul[i,27]<-f(174,result$par[1],result$par[2],result$par[3])
resul[i,28]<-f(180,result$par[1],result$par[2],result$par[3])
resul[i,29]<-f(186,result$par[1],result$par[2],result$par[3])
resul[i,30]<-f(192,result$par[1],result$par[2],result$par[3])
resul[i,31]<-f(198,result$par[1],result$par[2],result$par[3])
resul[i,32]<-f(204,result$par[1],result$par[2],result$par[3])
resul[i,33]<-f(210,result$par[1],result$par[2],result$par[3])
resul[i,34]<-f(216,result$par[1],result$par[2],result$par[3])


resul[i,35]<-cor(yy[,2],f(yy[,1],result$par[1],result$par[2],result$par[3]))


}
}
}
#}



resul[which(resul[,1]>1000),1]<-NA
names(resul)[1]<-"50pTotalGenital"
names(resul)[2]<-"SpeedTotalGenital"
names(resul)[3]<-"AsymTotalGenital"

names(resul)[4:34]<-paste("Genital",seq(3,18,0.5),sep="")
names(resul)[35]<-"ModelFitGenital"


for (i in 4:34){
z<-scale(resul[,i])
resul[which(z>5),i]<-NA
resul[which(z< -5),i]<-NA
}



write.csv(resul,file="genitalnew.csv")


total<-resul[,1:19]

#graphical test
z<-cbind(resul[,1:3],x,y)


#examples with negative correlations
neg<-which(resul[,35]<0)
neg<-neg[1:9]

par(mfrow=c(3,3))

for (i in 1:9){

n<-neg[i]
x<-seq(0:240)
df<-data.frame(cbind(x,f(x,z[n,2],z[n,1],z[n,3])))
ddf<-data.frame(cbind(t(z[n,11:17]),t((z[n,4:10]-1))/4))
tit<-paste("ID",gsub(" ","",d[n,1]),collapse=" ")
plot(ddf[,1],ddf[,2],xlim=c(0,240),ylim=c(0,1),xlab="Age [months]",ylab="Pub. Progress [%]")
title(main=tit)
lines(df[,1],df[,2],col="red")
}



#graphical tests for complete cases

z<-z[which(z[,1]>0),]
z<-z[complete.cases(z),]
id<-d[complete.cases(z),1]


par(mfrow=c(3,3))
for (n in 1:9){
n<-round(runif(1,1,nrow(z)),digits=0)
x<-seq(0:240)
df<-data.frame(cbind(x,f(x,z[n,2],z[n,1],z[n,3])))
ddf<-data.frame(cbind(t(z[n,11:17]),t(z[n,4:10])))
tit<-paste("ID",gsub(" ","",id[n]),collapse=" ")
plot(ddf[,1],ddf[,2],xlim=c(0,240),ylim=c(0,1),xlab="Age [months]",ylab="Voice Change Boys [%]")
title(main=tit)
lines(df[,1],df[,2],col="red")
}


#logistic curve males and females and model fit
par(mfrow=c(1,3))
plot.new()
text(0.5,0.5,"Males",cex=1.1,font=2,pos=1)
r<-data.frame(matrix(0,ncol=3,nrow=20))
for (i in 4:20){
r[i,1]<-quantile(resul[which(d$kz021==1),i],0.16,na.rm=T)
r[i,2]<-mean(na.omit(resul[which(d$kz021==1),i]))
r[i,3]<-quantile(resul[which(d$kz021==1),i],0.84,na.rm=T)
}


t<-paste("M=",round(mean(na.omit(resul[which(d$kz021==1),1])/12),digits=2),"SD=",round(sd(na.omit(resul[which(d$kz021==1),1]/12)),digits=2),collapse="")
t<-paste("Age [years] \n",t,sep="")

plot(r[,2],type="l",col="green",xlab=t,ylab="Progress [%]",main="Overall Progress",ylim=c(0,1))
lines(r[,1],col="red",lty=3)
lines(r[,3],col="red",lty=3)


t<-paste("M=",round(mean(na.omit(resul[which(d$kz021==1),35])),digits=2),"SD=",round(sd(na.omit(resul[which(d$kz021==1),35])),digits=2),collapse="")
t<-paste("Correlation of observed with modelled \n",t,sep="")

hist(resul[which(d$kz021==1),35],main="Model Fit",xlab=t)




#Graph parameters logistic fit
par(mfrow=c(1,4))
plot.new()
text(0.5,0.5,"Males",cex=1.1,font=2)

t<-paste("M=",round(mean(na.omit(resul[which(d$kz021==1),1])),digits=2),"SD=",round(sd(na.omit(resul[which(d$kz021==1),1])),digits=2),collapse="")
t<-paste("Age [months] (Parameter A)\n",t,sep="")
hist(resul[which(d$kz021==1),1],main="50% Threshold",xlab=t)
t<-paste("M=",round(mean(na.omit(resul[which(d$kz021==1),2])),digits=2),"SD=",round(sd(na.omit(resul[which(d$kz021==1),2])),digits=2),collapse="")
t<-paste("Parameter b\n",t,sep="")
hist(resul[which(d$kz021==1),2],main="Gradient of logistic curve",xlab=t,xlim=c(0,3))
t<-paste("M=",round(mean(na.omit(resul[which(d$kz021==1),3])),digits=2),"SD=",round(sd(na.omit(resul[which(d$kz021==1),3])),digits=2),collapse="")
t<-paste("Parameter c\n",t,sep="")
hist(resul[which(d$kz021==1),3],main="Asymptote",xlab=t)



