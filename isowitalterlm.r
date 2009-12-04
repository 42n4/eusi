# skrypt do zajêæ ISO WIT: lars, leaps
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

#te ¶cie¿ki mypath i mypathout musz± istnieæ w systemie
#to ¶cie¿ka do plików wykonywalnych z R pow³oki za pomoc± source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"

#(mypath <- getwd())
#if (!is.null(mypath)) setwd(mypath)

#to ¶cie¿ka do plików graficznych uzyskiwanych za pomoc± funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")
dir.create(mypathout, showWarnings = TRUE, recursive = TRUE, mode = "0755")
#Sys.chmod(paths, mode = "0755")

source(paste(mypath,"isowitfunkcje.r",sep=""))

##########################################################################################################
nData<-"Glass"
#nData<-"nihills"
#nData<-"photocar"
source(paste(mypath,"isowitdatasets.r",sep=""))

##################################################################################################
#podzia³ danych na trenuj±ce i testowe
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
DataSet.train <- DataSet[etykiety,]
DataSet.test <- DataSet[-etykiety,]
mparvec=setdiff(names(DataSet),parvecnolm)

#Stosujemy kryteria: 
#BIC Bayes Information Criterion  -2maxlog-likelihood+plogn wybór mniejszych modeli
#AIC Akaike Information Cryterion -2 maxlog-likelihood + 2p
#Ridge metoda wybiera jak najmniejsze parametry Beta regresji
#dla liniowej regresji -2max loglikelihood = nlog(RSS/n)+constant 


##################################################################################################
#Funkcja optim.lm wybiera "najlepszy" model regresji liniowej na podstawie DataSet.train
#library leaps i lars
leaps.lm <-function(fname,data,moutput,mparvec,val.size=NULL,cv=10,bic=T,test=T,really.big=F){
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	n.plots <- sum(c(bic,test,cv!=0))
	par(mfrow=c(n.plots,2),pch=1.0,cex.lab=1.5,lwd=3)
	n <- nrow(data)
    evalstr<-"x=T"
	mlm<-evalwithattr("lm",moutput,mparvec,data,evalstr)
	X <- mlm$x
	data <- as.data.frame(cbind(data[,moutput],X[,-1]))
	names(data)[1] <- moutput
	if (is.null(val.size))
		val.size <- round(n/4)
	s <- sample(n,val.size,replace=F)
	data.train <- data[-s,]
	data.test <- data[s,]
	p <- ncol(data)-1
	data.names <- names(data)[names(data)!=moutput]
	evalstr<-"nvmax=p,nbest=1,really.big=really.big"
	tmp <- printwithattr("regsubsets",moutput,mparvec,data,evalstr)
	regfit.full <- eval(parse(text=tmp))
	varvec<-data.names[summary(regfit.full)$which[order(summary(regfit.full)$bic)[1],][-1]]
	bic.lm <- evalwithattr("lm",moutput,varvec,data)
	tmp <- printwithattr("regsubsets",moutput,mparvec,data.train,evalstr)
	regfit <- eval(parse(text=tmp))
	plot(regfit.full,scale="r2",main="RegFit on Full Data")
	plot(regfit,scale="r2",main="RegFit on Train Data")
	
	if (bic){
		plot(summary(regfit.full)$bic,type='l',xlab="Number of Predictors",ylab="BIC",main="BIC Method")
		points(summary(regfit.full)$bic,pch=20)
		points(order(summary(regfit.full)$bic)[1],min(summary(regfit.full)$bic),col=2,pch=20,cex=1.5)
	}
	cv.rss <- rootmse <- rep(0,p)
	for (i in 1:p){
		varvec<-data.names[summary(regfit)$which[i,][-1]]
		data.lmfit <-evalwithattr("lm",moutput,varvec,data.train)
		rootmse[i]<-sqrt(mean((predict(data.lmfit,data.test)-data.test[,moutput])^2))
	}
	if (test){
		plot(rootmse,type='l',xlab="Number of Predictors",ylab="Root Mean RSS on Test Data",main="Validation Method")
		points(rootmse,pch=20)
		points(order(rootmse)[1],min(rootmse),col=2,pch=20,cex=1.5)
	}
	varvec<-data.names[summary(regfit)$which[order(rootmse)[1],][-1]]
	validation.lm <- evalwithattr("lm",moutput,varvec,data)
	if (cv!=0){
		if (cv==n) s <- 1:n else s <- sample(cv,n,replace=T);
		for (i in 1:p)
			for (j in 1:cv){
				data.train <- data[s!=j,]
				data.test <- data[s==j,]
				varvec<-data.names[summary(regfit.full)$which[i,][-1]]
				data.lmfit<-evalwithattr("lm",moutput,varvec,data.train)
				cv.rss[i]<-cv.rss[i]+sum((predict(data.lmfit,data.test)-data.test[,moutput])^2)
			}
		cv.rss <- sqrt(cv.rss/n)
		plot(cv.rss,type='l',xlab="Number of Predictors",
				ylab="Cross-Validated Root Mean RSS",main="Cross-Validation Method")
		points(cv.rss,pch=20)
		points(order(cv.rss)[1],min(cv.rss),col=2,pch=20,cex=1.5)
		varvec<-data.names[summary(regfit.full)$which[order(cv.rss)[1],][-1]]
		cv.lm <-evalwithattr("lm",moutput,varvec,data)
	}
	else
		cv.lm <- NULL
	dev.off()
	list(bic.lm = bic.lm,validation.lm=validation.lm,cv.lm=cv.lm)
}


##################################################################################################
#Funkcja RMSE
rmse <- function(x,y) sqrt(mean((x-y)^2))


##################################################################################################
#Funkcja pred.ridge predykcja z lm.ridge wewn±trz i DataSet z etykietami: 
#podzia³ na zbiór treninuj±cy i testowy
pred.ridge.etykiety<-function(moutput,mparvec,DataSet,etykiety){
	mm<-apply(DataSet[etykiety,mparvec],2,mean)
	trainx <- as.matrix(sweep(DataSet[etykiety,mparvec],2,mm))
	testx <- as.matrix(sweep(DataSet[-etykiety,mparvec],2,mm))
	yc<-DataSet[etykiety,moutput]-mean(DataSet[etykiety,moutput])
	lambda.set <- 10^(seq(-2, 8, length = 100))
	ridge.train<-lm.ridge(yc~trainx,lambda=lambda.set)
	mlambda<-as.numeric(names(which.min(ridge.train$GCV)))
	ridge.train <- lm.ridge(yc~trainx,lambda=mlambda)
	ypredg<-scale(trainx,center=FALSE,scale=ridge.train$scales) %*% ridge.train$coef+mean(DataSet[etykiety,moutput])
	ytpredg<-scale(testx,center=FALSE,scale=ridge.train$scales) %*% ridge.train$coef+mean(DataSet[etykiety,moutput])
	l<-list()
	l[["y"]]<-ypredg
	l[["yt"]]<-ytpredg
	l
}

##################################################################################################
#Funkcja pred.ridge predykcja po lm.ridge - jak z ksi±¿ki
pred.ridge1<-function(ridge.train,moutput,mparvec,DataSet){
	mm<-apply(DataSet[,mparvec],2,mean)
	trainx <- as.matrix(sweep(DataSet[,mparvec],2,mm))
	yc<-DataSet[,moutput]-mean(DataSet[,moutput])
	#lambda.set <- 10^(seq(-2, 8, length = 100))
	#ridge.train<-lm.ridge(yc~trainx,lambda=lambda.set)
	#mlambda<-as.numeric(names(which.min(ridge.train$GCV)))
	#ridge.train <- lm.ridge(yc~trainx,lambda=mlambda)
	scale(trainx,center=FALSE,scale=ridge.train$scales) %*% ridge.train$coef+mean(DataSet[,moutput])
}


##################################################################################################
#Funkcja pred.ridge predykcja po lm.ridge - moja wersja
pred.ridge<-function(ridge.train,moutput,mparvec,DataSet){
	yc<-DataSet[,moutput]-mean(DataSet[,moutput])
	scale(DataSet[,mparvec],scale=ridge.train$scales) %*% ridge.train$coef+mean(DataSet[,moutput])
}

###########################################################
#Funkcja ridge.lm wybiera "najlepszy" model regresji liniowej na podstawie DataSet.train
#library MASS
#lambda is the penalty coefficient on the beta squares
#lambda 0, beta same as OLS
ridge.lm <-function(fname,DataSet,moutput,mparvec,etykiety){
	DataSet.train <- DataSet[etykiety,]
	#jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	#par(mfrow=c(2,2),pch=1.0,cex.lab=1.5,lwd=3)
	lambda.set <- 10^(seq(-2, 8, length = 100));
	evalstr<-"lambda = lambda.set"
	tmp<-printwithattr("lm.ridge",moutput,mparvec,DataSet.train,evalstr)
	ridge.train1 <- eval(parse(text=tmp))
	#select(ridge.train1);
	mlambda<-as.numeric(names(which.min(ridge.train1$GCV)))
	#mlambda
	evalstr<-"lambda = mlambda"
	tmp <- printwithattr("lm.ridge",moutput,mparvec,DataSet.train,evalstr)
	ridge.train.cv <- eval(parse(text=tmp))
	#ridge.train.cv$coef;
	#varvec<-paste(moutput,"~",paste(mparvec,collapse="+"),sep="")
	#iteracje CV cross-validation
	rss.ridge <- rep(0, 100);
	for(i in 1:100){
		evalstr<-"lambda = lambda.set[i]"
		tmp <- printwithattr("lm.ridge",moutput,mparvec,DataSet.train,evalstr)
		ridge.train <- eval(parse(text=tmp))
		ridge.pred <- pred.ridge(ridge.train,moutput,mparvec,DataSet);
		rss.ridge[i] <- mean((DataSet[-etykiety,moutput] - ridge.pred[-etykiety])^2);
	}
	#min(rss.ridge);
	#plot(rss.ridge, type = "l")
	#points(min(rss.ridge))
	best.lambda <- lambda.set[order(rss.ridge)[1]]
	#best.lambda;
	evalstr<-"lambda = best.lambda"
	tmp <- printwithattr("lm.ridge",moutput,mparvec,DataSet.train,evalstr)
	ridge.best <- eval(parse(text=tmp))
	#dev.off()
	list(ridge.best=ridge.best,ridge.train.cv=ridge.train.cv,rss.ridge=rss.ridge)
}

#### LASSO ############################################################################################
# metoda LASSO redukuje wymiar i robi ridge optymalizacjê
# s is the constraint sum |beta| < s, dla s infinity, beta takie samo jak OLS
#######################################################################################################

#######################################################################################################
"cv.lasso" <- function(formula,data,K=10,subset=NULL){
	if (!is.null(subset))
		data <- data[subset,]
	y <- data[,names(data)==as.character(formula)[2]]
	x <- model.matrix(as.formula(formula),data)[,-1]
	#mycv<-cv;rm(cv)
	larsfit <- cv.lars(x,y,K=K,plot.it=FALSE)
	larsfit
}

#######################################################################################################
"lasso" <-	function(formula,data,subset=NULL){
	if (!is.null(subset))
		data <- data[subset,]
	
	y <- data[,names(data)==as.character(formula)[2]]
	x <- model.matrix(as.formula(formula),data)[,-1]
	larsfit <- lars(x,y,type="lasso")
	larsfit
}


###########################################################
#Funkcja lars.lm wybiera kompaktowy i w miarê dobry model regresji liniowej na podstawie lars i cv.lars
#library lars
lars.lm <-function(fname,DataSet,moutput,mparvec,etykiety){
	DataSet.train <- DataSet[etykiety,]
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(mfrow=c(1,2),pch=1.0,cex.lab=1.5,lwd=3)
	lasso.fit <- evalwithattr("lasso",moutput,mparvec,DataSet.train)
	larsfit <- predict.lars(lasso.fit, DataSet[,mparvec], type="fit")
	plot(lasso.fit,breaks=F,lty="solid") 
	DF <- lasso.fit$df #df on the regularization path
	p <- length(mparvec)
	n <- nrow(DataSet.train)
	#note: DF[1] == 1, intercept only
	#Best AIC/BIC Model
	LL<- (-n/2)*log(lasso.fit$RSS/n)
	AICLasso <- -2*LL + 2*DF
	BICLasso<- -2*LL + log(n)*DF
	names(AICLasso)<-names(BICLasso)<-paste("df=",DF,sep="")
	indAICModel<-which.min(AICLasso)
	indBICModel<-which.min(BICLasso)
	bLARSAIC<-coef(lasso.fit)[indAICModel,]
	bLARSBIC<-coef(lasso.fit)[indBICModel,]
	#names(indAICModel)
	#sum(bLARSAIC!=0.0) 
	#names(indBICModel)
	#sum(bLARSBIC!=0.0)
	bLARSAICfits=larsfit$fit[,indAICModel]
	bLARSBICfits=larsfit$fit[,indBICModel]
	
	#set.seed(2317723)
	## best fmin
	evalstr<-"K=10"
	lasso.cv <- evalwithattr("cv.lasso",moutput,mparvec,DataSet.train,evalstr)
	#lasso.cv contains components: 'cv', 'cv.error', 'fraction'
	CVout <- matrix(c(lasso.cv$cv,lasso.cv$cv.error), ncol=2)
	indBest<-oneSdRule(CVout)
	fBest<-(lasso.cv$fraction)[indBest]
	#fmin <- lasso.cv$fraction[order(lasso.cv$cv)[1]]
	fmin<-(lasso.cv$fraction)[which.min(lasso.cv$cv)]
	#
	plotCVLars(lasso.cv)
	TotalAbsBeta<-apply(coef(lasso.fit), MARGIN=1, function(x) sum(abs(x)))[-1]
	BLength<-TotalAbsBeta/TotalAbsBeta[length(TotalAbsBeta)]
	tablef<-matrix( c(DF[-1], BLength) , ncol=2)
	#label the df on the plot
	axis(side=3, at=tablef[,2], labels=tablef[,1])
	mtext("DF", side=3, line=2)
	abline(v=fmin, col="red", lwd=3)
	abline(v=fBest, col="blue", lwd=3)
	title(sub="One-sd-rule CV (blue) and minimum CV (red) shown")
	pHatF <- round(approx(x=tablef[,2],y=tablef[,1], xout=fBest)$y,1)
	#Note: phatF is the estimated df including the intercept
	# So pHatF-1 is the number of inputs
	text(0.2, 1.3, labels=bquote(hat(p)==.(pHatF-1)))
	#Best model using 10-fold CV with one-sd rule
	pHat <- round(pHatF)
	indpHat <- match(pHat, lasso.fit$df)
	bHat<-coef(lasso.fit)[indpHat,]
	bHatfits=larsfit$fit[,indpHat]
	dev.off()
	list(bLARSAIC=bLARSAIC,bLARSAICfits=bLARSAICfits,bLARSBIC=bLARSBIC,bLARSBICfits=bLARSBICfits,bHat=bHat,bHatfits=bHatfits)
}

#set.seed(23123)
lars.lm.train <- lars.lm(paste(mypathout,nData,"_lmlrs_norm",sep=""),DataSet,moutput,mparvec,etykiety);

mean.lasso.AIC<-mean((DataSet[-etykiety,moutput] - lars.lm.train$bLARSAICfits[-etykiety])^2)
mean.lasso.BIC<-mean((DataSet[-etykiety,moutput] - lars.lm.train$bLARSBICfits[-etykiety])^2)
mean.lasso.bHat<-mean((DataSet[-etykiety,moutput] - lars.lm.train$bHatfits[-etykiety])^2)
mean.lasso.bHat
mean.lasso.AIC
bHat[bHat!=0]


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
really.big=F
leaps.lm.train <- leaps.lm(paste(mypathout,nData,"_lmbic_norm",sep=""),DataSet.train, moutput, mparvec);
names(leaps.lm.train);
# modele wybrane przez metodê BIC, validation, CV (cross validation)
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
#generuje rysunek z liniami b³êdami kolejnych metod znajdowania modelu lm, 
#ni¿ej lepiej za wyj±tkiem Raw, bo jest podzielone

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


#KONIEC PLIKU PONI¯EJ NOTATKI

# K-fold cross-validation
#DAAG pakiet
#cv.lm(df=DataSet, fit, m=10) # 10 fold cross-validation

#Przyk³ad z ksi±¿ki "Linear models with R" James Faraway
#pozosta³e ze strony http://statisticsr.blogspot.com/
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



#predict.lars(lasso.fit,type="fit", s)

#### iterations #########
#s.set <- seq(0, 1, length = 100);
#rss.lasso <- rep(0, 100);
#for(i in 1:100){
#	lasso.pred <- predict.lars(lasso.fit, RI~Na+Mg+Al+Si+K+Ca+Ba+Fe, DataSet, s = s.set[i]);
#	rss.lasso[i] <- mean((DataSet[-etykiety,moutput] - lasso.pred[-etykiety])^2);
#}
#min(rss.lasso)
#plot(rss.lasso,type="l")
#s <- s.set[order(rss.lasso)[1]];
#s
#predict.lars(lasso.fit, RI~Na+Mg+Al+Si+K+Ca+Ba+Fe, DataSet, s, "coefficients")






