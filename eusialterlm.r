# skrypt do zajęć EU SI: lars, leaps
# TODO: test, testować
# Licence LGPL  
# Author: Piotr Wąsiewicz
########################################################################################################

#te ścieżki mypath i mypathout muszą istnieć w systemie
#to ścieżka do plików wykonywalnych z R powłoki za pomocą source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/guest/workspace/iso/"

#(mypath <- getwd())
#if (!is.null(mypath)) setwd(mypath)

#to ścieżka do plików graficznych uzyskiwanych za pomocą funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")
dir.create(mypathout, showWarnings = TRUE, recursive = TRUE, mode = "0755")
#Sys.chmod(paths, mode = "0755")

#na końcu tego pliku funkcje, w razie czego należy je przenieść tutaj, aby testować krok po kroku, jak 
#pokazałem na wykładzie ;)
source(paste(mypath,"eusifunkcje.r",sep=""))

##########################################################################################################
#nData<-"Glass"
#nData<-"nihills"
#nData<-"photocar"
#nData<-"meatspec"
source(paste(mypath,"eusidatasets.r",sep=""))
source(paste(mypath,"eusidataprep.r",sep=""))

##################################################################################################
#podział danych na trenujące i testowe
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
DataSet.train <- DataSet[etykiety,]
DataSet.test <- DataSet[-etykiety,]
mparvec=setdiff(names(DataSet),parvecnolm)

#set.seed(23123)
lars.lm.train <- lars.lm(paste(mypathout,nData,"_lmlrs_norm",sep=""),DataSet,moutput,mparvec,etykiety);

mean.lasso.AIC<-mean((DataSet[-etykiety,moutput] - lars.lm.train$bLARSAICfits[-etykiety])^2)
mean.lasso.BIC<-mean((DataSet[-etykiety,moutput] - lars.lm.train$bLARSBICfits[-etykiety])^2)
mean.lasso.bHat<-mean((DataSet[-etykiety,moutput] - lars.lm.train$bHatfits[-etykiety])^2)
mean.lasso.bHat
mean.lasso.AIC



ridge.lm.train <- ridge.lm(paste(mypathout,nData,"_lmrdg_norm",sep=""),DataSet,moutput,mparvec,etykiety);

ridge.pred.cv <- pred.ridge(ridge.lm.train$ridge.train.cv,moutput,mparvec,DataSet)
ridge.pred.best <- pred.ridge(ridge.lm.train$ridge.best,moutput,mparvec,DataSet)
rmse.ridge.cv<-rmse(DataSet[-etykiety,moutput],ridge.pred.cv[-etykiety])
mean.ridge.cv<-mean((DataSet[-etykiety,moutput] - ridge.pred.cv[-etykiety])^2)
rmse.ridge.best<-rmse(DataSet[-etykiety,moutput],ridge.pred.best[-etykiety])
mean.ridge.best<-mean((DataSet[-etykiety,moutput] - ridge.pred.best[-etykiety])^2)

ridge.pred.cv1 <- pred.ridge1(ridge.lm.train$ridge.train.cv,moutput,mparvec,DataSet)
ridge.pred.best1 <- pred.ridge1(ridge.lm.train$ridge.best,moutput,mparvec,DataSet)
mean.ridge.cv1<-mean((DataSet[-etykiety,moutput] - ridge.pred.cv1[-etykiety])^2)
mean.ridge.best1<-mean((DataSet[-etykiety,moutput] - ridge.pred.best1[-etykiety])^2)

p <- ncol(DataSet.train)-1

leaps.lm.train <- leaps.lm(paste(mypathout,nData,"_lmbic_norm",sep=""),DataSet.train, moutput, mparvec,really.big=T);
names(leaps.lm.train);
# modele wybrane przez metodę BIC, validation, CV (cross validation)
summary(leaps.lm.train$bic.lm)
summary(leaps.lm.train$validation.lm)
summary(leaps.lm.train$cv.lm)

#Stepwise Regression
#MASS pakiet
oData<-DataSet #dane z procedury evalwithattr, niestety tu potrzebne
fit <- evalwithattr("lm",moutput,mparvec,DataSet)
lmstep <- stepAIC(fit, direction="both")
#lmstep$anova #nowe dodane dane 
mean.lm.aic<-mean((DataSet[-etykiety,moutput] - predict(lmstep,DataSet)[-etykiety])^2)
rmse.lm.aic<-rmse(DataSet[-etykiety,moutput],predict(lmstep,DataSet)[-etykiety])

#y~1
mean.lm.1<-mean((DataSet[-etykiety,moutput] - mean(DataSet[etykiety,moutput]))^2)
rmse.lm.1<-rmse(DataSet[-etykiety,moutput],rep(mean(DataSet[etykiety,moutput]),nrow(DataSet[-etykiety,])))
#bic.lm 
mean.lm.bic<-mean((DataSet[-etykiety,moutput] - predict(leaps.lm.train$bic.lm,DataSet)[-etykiety])^2)
rmse.lm.bic<-rmse(DataSet[-etykiety,moutput],predict(leaps.lm.train$bic.lm,DataSet)[-etykiety])
#validation.lm 
mean.lm.validation<-mean((DataSet[-etykiety,moutput] - predict(leaps.lm.train$validation.lm,DataSet)[-etykiety])^2)
rmse.lm.validation<-rmse(DataSet[-etykiety,moutput],predict(leaps.lm.train$validation.lm,DataSet)[-etykiety])
#cv.lm 
mean.lm.cv<-mean((DataSet[-etykiety,moutput] - predict(leaps.lm.train$cv.lm,DataSet)[-etykiety])^2)
rmse.lm.cv<-rmse(DataSet[-etykiety,moutput],predict(leaps.lm.train$cv.lm,DataSet)[-etykiety])
#ols.lm
lmfit <- evalwithattr("lm",moutput,mparvec,DataSet.train)
mean.lm.ols<-mean((DataSet[-etykiety,moutput]-predict(lmfit,DataSet)[-etykiety])^2)
rmse.lm.ols<-rmse(DataSet[-etykiety,moutput],predict(lmfit,DataSet)[-etykiety])

rmeanstr<-c("mean.lm.1",
"mean.lm.ols",
"mean.ridge.cv",
"mean.ridge.best",
"mean.lasso.AIC",
"mean.lasso.bHat",
"mean.lm.aic",
"mean.lm.bic",
"mean.lm.validation",
"mean.lm.cv")

j=0;
for(i in rmeanstr[seq(1,10)]) 
	{j=j+1; if(j%%4==0) cat(i,":",get(i),"\n", sep="") else cat(i,":",get(i),"\t,", sep="")} 

meanvec<-as.vector(sapply(rmeanstr[seq(1,10)],function(x) get(x)))
olsdiv=round(meanvec[1]/max(meanvec[-c(1)]))
meanvec[1]<-meanvec[1]/olsdiv
#meanscl<-as.data.frame(t(as.vector(scale(meanvec))))
meanscl<-as.data.frame(t(meanvec))
names(meanscl)<-rmeanstr[seq(1,10)]

#################################################################################################
#generuje rysunek z liniami błędami kolejnych metod znajdowania modelu lm, 
#niżej lepiej za wyjątkiem Raw, bo jest podzielone

fname=paste(mypathout,nData,"_lmall_norm",sep="")
jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
x<-seq(1:100)
y<-seq(min(meanvec),max(meanvec),length.out=100)
plot(x,y,ylab="Test Mean RSS",xlab="Tuning Parameter", type="n",lwd=3)
abline(mean.lm.1/olsdiv,0,lwd=6,lty=2, col = "green")
abline(mean.lm.ols,0,lwd=6,lty=3, col = "blue")
abline(mean.lm.bic,0,lwd=4,lty=4, col = "grey")
abline(mean.lm.aic,0,lwd=5,lty=5, col = "red")
abline(mean.ridge.best,0,lwd=6,lty=6, col = "orange")
abline(mean.lasso.AIC,0,lwd=2,lty=1, col = "black")
abline(mean.lasso.bHat,0,lwd=3,lty=7, col = "pink")
legend(70,mean(y)+sd(y),c(paste("Raw/",olsdiv,sep=""),"OLS","LeapsBIC","StepAIC","Ridge","LassoAIC","LassobHat"),col = c("green", "blue", "grey", "red", "orange","black","pink"), lty=c(2,3,4,5,6,1,7),lwd=c(6,6,4,5,6,2,3))
dev.off()


#KONIEC PLIKU PONIŻEJ NOTATKI

#Przykład z książki "Linear models with R" James Faraway
#pozostałe ze strony http://statisticsr.blogspot.com/
#oraz ze strony http://www.statmethods.net/stats/regression.html
#mm<-apply(meatspec[1:172,-101],2,mean)
#trainx <- as.matrix(sweep(meatspec[1:172,-101],2,mm))
#testx <- as.matrix(sweep(meatspec[173:215,-101],2,mm))
#yc<-meatspec$fat[1:172]-mean(meatspec$fat[1:172])
#ridge.train<-lm.ridge(yc~trainx,lambda=seq(0,5e-8,1e-9))
#ridge.train.cv <- ridge.train
#ypredg<-scale(trainx,center=FALSE,scale=ridge.train.cv$scales) %*% ridge.train.cv$coef[,which.min(ridge.train.cv$GCV)]+mean(meatspec$fat[1:172])
#rmse(ypredg,meatspec$fat[1:172])
#ytpredg<-scale(testx,center=FALSE,scale=ridge.train.cv$scales) %*% ridge.train.cv$coef[,which.min(ridge.train.cv$GCV)]+mean(meatspec$fat[1:172])
#rmse(ytpredg,meatspec$fat[173:215])
#rmse(ytpredg[-13],meatspec$fat[173:215][-13])







