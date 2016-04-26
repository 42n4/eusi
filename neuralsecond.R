pkglist<-c("clusterGeneration","corrplot","nnet","neuralnet",
           "RSNNS","reshape","rockchalk","fifer")
pkgcheck <- pkglist %in% row.names(installed.packages())
#for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}

library(clusterGeneration)
library(corrplot)
#import the function from Github
library(devtools)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
#nnet function from nnet package
library(nnet)

seed.val<-86644
set.seed(seed.val)

#num.vars<-1
div<-3
num.obs<-100/div
max.neurons<-100

#rand.vars<-rnorm(num.obs)
x1<-seq(1,num.obs,div)/10*div
xx1<-seq(1,num.obs)/10*div

y1<-sin(x1)
yy1<-sin(xx1)
dim(x1)
plot(x1,y1)

#final datasets
rand.vars<-data.frame(x1)
names(rand.vars)<-c('X1')
resp<-data.frame(y1)
names(resp)<-c('Y1')
dat.in<-data.frame(resp,rand.vars)

set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=20,linout=T)

par(mfrow = c(3,1))
#plot each model
plot.nnet(mod1)

ypred<-predict(mod1,cbind(xx1))
plot(xx1,ypred)
kwadroznicy<-(yy1-ypred)^2
sumkwadrozn<-sqrt(sum((yy1-ypred)^2))
pierwkwadsumkwadrozn<-sqrt(sum((yy1-ypred)^2))
pierwkwadsumkwadrozn

errorlist<-list()
for (i in 4:max.neurons){
set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=i,linout=T,trace=FALSE)
ypred<-predict(mod1,cbind(xx1))
error<-sqrt(sum((yy1-ypred)^2))
errorlist<-c(errorlist,error)
}
errorvector<-rapply(errorlist,c)
plot(errorvector)
minerror<-min(errorvector)
minerror
#optimise<-which(errorvector %in% c(min(errorvector)))
optimsize<-match(min(errorvector),errorvector)
optimsize

set.seed(seed.val)
mod1<-nnet(rand.vars,resp,data=dat.in,size=optimsize+3,linout=T,trace=FALSE)
ypred<-predict(mod1,cbind(xx1)) #uwaga xx1 , a nie x1
error<-sqrt(sum((yy1-ypred)^2))
error

par(mfrow = c(3,1))
#plot each model
plot.nnet(mod1)
plot(x1,y1,col="red")
lines(xx1,ypred)
plot(errorvector)


