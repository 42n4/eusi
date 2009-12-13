# zbiór funkcji do zajêæ ISO WIT 
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

Sys.setlocale("LC_NUMERIC","C") 

#lista pakietów z CRAN-u
#naprawiæ pakier gRain
#pkglist<-c("gRain")
pkglist<-c("ggplot2","reshape","gbm","caret","arules","mboost","bestglm","ElemStatLearn","faraway","relaimpo","leaps","lars","bootstrap","DAAG","ff","biglm","bigmemory","splines","betareg","ellipse","nlme","MASS","leaps","car","lmtest","gregmisc","foreign","plyr","mlbench","boot","Hmisc","RWeka","ipred","klaR","ROCR","rpart","dprep","maptree","party","grid","lattice","latticeExtra","playwith","ada","randomForest","kknn","e1071","cluster","class","caret","fda","zoo","lattice","deal","RJDBC","cairoDevice")
pkgcheck <- pkglist %in% row.names(installed.packages())
for(i in pkglist[!pkgcheck]){
	install.packages(i,depend=TRUE)
}

#lista pakietów z bioconductora
biolist<-c("Rgraphviz")
biocheck <- biolist %in% row.names(installed.packages())
for(i in biolist[!biocheck]){
    source("http://bioconductor.org/biocLite.R")
	#update.packages(repos=biocinstallRepos(), ask=FALSE)
	biocLite(i)
}

for(i in pkglist)
{ library(i, character.only = TRUE);}

for(i in biolist)
{ library(i, character.only = TRUE);}

#############################################################################
#Funkcje podstawowe zalecane do procesu odkrywania wiedzy
#############################################################################

#Funkcja brutoptim.klas znajduje najlepszy klasyfikator przeszukuj±c wszystkie kombinacje atrybutów
brutoptim.klas <-function(mypathout,nData,DataSet,nFunction,paroutputree,parvectree,etykiety,evalstr,jmax){
	verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
	for(j in 1:jmax){
		verr<-c();vmod<-list();
		for(i in 1:4){
			etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.9))
			if(i==1) {mDataSet<-DataSet[etykiety,]; mDataSetn<-DataSet[-etykiety,];datype<-"norm"}
			else if(i==2) {mDataSet<-DataSetz[etykiety,]; mDataSetn<-DataSetz[-etykiety,];datype<-"zesc"}
			else if(i==3) {mDataSet<-DataSetd[etykiety,]; mDataSetn<-DataSetd[-etykiety,];datype<-"nrdi"}
			else if(i==4) {mDataSet<-DataSetzd[etykiety,]; mDataSetn<-DataSetzd[-etykiety,];datype<-"zedi"}
			classifier<-try(evalwithattr(nFunction,paroutputree,parvectree,mDataSet,evalstr),TRUE)
			if(!inherits(classifier, "try-error")){
				lres<-prederror(classifier,paroutputree,parvectree,mDataSetn,evalstr)
				vmod[[i]]<-classifier;verr<-c(verr,lres$perror);
			}
			else jdiv<-jdiv-1;
		}
		lverr[[j]]<-verr; lvmod[[j]]<-vmod
	}
	verr<-meanverr(lverr,jdiv,4); n<-min(which(verr==min(verr)))
	if(n==1){mbDataSet<-DataSet;datype<-"norm";}
	if(n==2){mbDataSet<-DataSetz;datype<-"zesc";}
	if(n==3){mbDataSet<-DataSetd;datype<-"nrdi";}
	if(n==4){mbDataSet<-DataSetzd;datype<-"zedi";}
#generujemy nFunction automatycznie dla parvectree i ró¿nej liczby atrybutów dla najlepiej pasuj±cych danych
	pred_norm<-combesteval(nFunction, paste(mypathout,nData,"_pred_",nFunction,"_",datype,sep=""),mbDataSet, paroutputree,parvectree,80,5,evalstr)
	y<-seq(1,length(pred_norm));y<-sapply(y,function(x){if(x %% 5) x<-0 else x<-x});y<-y[!y==0];
	z<-c();for(i in y) z<-c(z,pred_norm[[i-1]]$perror); n<-min(which(z==min(z)))
	bestclass<-pred_norm[[(n-1)*5+3]]
	betykiety<-pred_norm[[(n-1)*5+1]]
	bestli<-pred_norm[[(n-1)*5+2]]
	bestn<-pred_norm[[(n-1)*5+5]]
	if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
	if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
	list(bestclass=bestclass,betykiety=betykiety,bestli=bestli,bestn=bestn,datype=datype,z=z)
}

#Funkcja prederror zwraca warto¶ci predykcji klasyfikatora, tablicê rezultatów i rzeczywistych warto¶ci, 
#oraz b³±d klasyfikatora, na wej¶ciu dane testowe, ale nie treningowe wykorzystane do konstrukcji klasyfikatora
prederror<-function(classimod,paroutputree,parvectree,DataSet,EvalString="DEFAULT"){
	if (EvalString == "CLASS"){
		predvalues=predict(classimod,DataSet[,parvectree])$class
	} else if(EvalString=="DEFAULT"){
		predvalues=predict(classimod,newdata=DataSet[,parvectree],"class");
	}  else {
		predvalues=predict(classimod,DataSet[,parvectree]);
	}
	tabresults=table(predicted=predvalues, real=DataSet[,paroutputree])
	prederr<-1-sum(diag(tabresults))/sum(tabresults)
	l<-list()
	l[["pvalues"]]<-predvalues
	l[["table"]]<-tabresults
	l[["perror"]]<-prederr
	l
}


#Funkcja combesteval liczy kombinacje zmiennych obja¶niaj±cych niezale¿nych dla n=1 do n równego ilo¶æ wszystkich zmiennych 
#i oblicza dla nich funkcje get(nFunction) i wybiera najlepsz± dla ka¿dego n, wraca listê najlepszych obiektów z kolejnych iteracji  
#w lb<n> mamy wybierane po kolei kolejne zestawy na najlepsz± regresjê 
#w m<n> najlepszy model dla n, w sm<n> summary(m<n>), dla n=1 rysuje plot regresji
combesteval<-function (nFunction, fname, combiDataSet, moutput, parvec, nleven=-1, alpha=0.01, EvalString="DEFAULT"){
	l<-list();ilist<-list();etykiety<-seq(1,nrow(combiDataSet));m01<-FALSE
	#cat(" NR:",nrow(combiDataSet))
	n<-length(parvec)
	varvec<-parvec
	varplus<-paste(parvec,collapse="+")
	for(numvar in 1:length(parvec)){
		#cat(paste("1:",moutput,"~",varplus)," n:",numvar," length:",n," EvalString:",EvalString,"\n")
		cat(" i: ",numvar)
		if(nFunction=="lm")
			ilist<-lmwithattr(combiDataSet,moutput,parvec,numvar,nleven,alpha,EvalString)
		else if(nFunction=="polr")
			ilist<-polrwithattr(combiDataSet,moutput,parvec,numvar,EvalString)
		else {
			#cat(paste("1:",moutput,"~",varplus)," n:",numvar," length:",n," EvalString:",EvalString,"\n")
			ilist<-classwithattr(nFunction,combiDataSet,moutput,parvec,numvar,70,5,EvalString)
			etykiety<-ilist[[4]]
			m01<-ilist[[5]]
		}
		i<-ilist[[1]]; 
		if(i){
			if(numvar<n){
				combi<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
				varvec<-combi[i,]
			}
			else if(numvar==n) varvec<-parvec
			if(EvalString=="SPLINE") varplus<-paste("bs(",varvec,")",sep="") else varplus<-varvec
			varplus<-paste(varplus,collapse="+")
			if(nFunction%in%c("lm","polr"))
			m01<-try(evalwithattr(nFunction,moutput,varvec,combiDataSet[etykiety,],EvalString),TRUE)
			#cat("Najlepszy!!!!!!!\n\n")
		    #print(m01)
			if(!inherits(m01, "try-error")){
				if(numvar<10)mnv<-paste("m0",numvar,sep="")else mnv<-paste("m",numvar,sep="")
				if(numvar<10)smnv<-paste("sm0",numvar,sep="")else smnv<-paste("sm",numvar,sep="")
				if(numvar<10)lbnv<-paste("lb0",numvar,sep="")else lbnv<-paste("lb",numvar,sep="")
				if(numvar<10)etnv<-paste("etiq0",numvar,sep="")else etnv<-paste("etiq",numvar,sep="")
				if(numvar<10)numv<-paste("numv0",numvar,sep="")else numv<-paste("numv",numvar,sep="")
				assign(lbnv,ilist[[2]]);
				assign(mnv,m01);
				assign(etnv,etykiety);
				assign(numv,numvar);
				if(nFunction%in%c("lm","polr"))
					assign(smnv,summary(get(mnv)))
				else{ 
					assign(smnv,prederror(m01,moutput,parvec,combiDataSet[-etykiety,],EvalString));
					l[[etnv]]<-get(etnv)
				}
				#get(smnv)
				#Vif(get(mnv))
				#l[[etnv]]<-get(etnv)
				l[[lbnv]]<-get(lbnv)
				l[[mnv]]<-get(mnv)
				l[[smnv]]<-get(smnv)
				l[[numv]]<-get(numv)
				if(nFunction=="lm"){
					if(numvar==1){
						jpeg(file=paste(fname,numvar,"_1.jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
						par(lwd=4)
						vartemp<-eval(parse(text=paste("combiDataSet$",varvec,sep="")))
						rangetemp<-seq(min(vartemp),max(vartemp),length.out=213)
						plot(eval(parse(text=paste(moutput,"~",varvec))),data=combiDataSet,main=paste(moutput,"~",varplus), pch=1.0, cex.lab=1.5)
						dframetemp<-as.data.frame(rangetemp)
						names(dframetemp)<-varvec
						lines(rangetemp,predict(get(mnv),newdata=dframetemp), col="red", lwd=3)
						#cat(paste("3:",moutput,"~",varvec)," n:",numvar," mnv:",mnv," varvec:",paste("combiDataSet$",varvec,sep=""),"\n")
						dev.off()
					}
					jpeg(file=paste(fname,numvar,"_2.jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
					par(mfrow=c(2,2))
					plot(get(mnv),main=paste(moutput,"~",varplus), pch=1.0, cex.lab=1.5)
					dev.off()
				}
			}
		}
	}
	return (l)
}

#Funkcja classwithattr znajduje najlepszy klasyfikator dla numvar zmiennych - atrybutów
#moutput - wyj¶cie modelu zmienna zale¿na np. "Type", 
#parvec - wektor zmiennych wej¶ciowych niezale¿nych dla modelu,
#numvar - ile zmiennych z parvec ma wzi±æ udzia³ w combiutacji zmiennych niezale¿nych
classwithattr<-function (nFunction, cDataSet, moutput, parvec, numvar, percent=70, trials=5,EvalString="DEFAULT"){
	if(numvar<length(parvec)){
		combi<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
		rowcombi<-nrow(combi)
	}
	else rowcombi<-1
	lb<-c(); ibest<-0; lres<-list(); gbep<-1; gbetykiety<-c(); gbclas<-FALSE;
	for(i in 1:rowcombi){
		#cat("\nwewnatrz funkcji: ",i," ",numvar)
		if(numvar<length(parvec)) 	varplus<-combi[i,]
		if(numvar==length(parvec))	varplus<-parvec
		vecp<-c(); bep<-1; betykiety<-c(); bclas<-FALSE;
		for(j in 1:trials){
			classifier<-FALSE
			etykiety <- sample(1:nrow(cDataSet), round(nrow(cDataSet)*(percent/100)))
			#print(etykiety)
			#print(cDataSet[1:3,])
			classifier<-try(evalwithattr(nFunction,moutput,varplus,cDataSet[etykiety,],EvalString),TRUE)
			#print(classifier)
			if(!inherits(classifier, "try-error") || classifier==FALSE){
				#cat(" po funkcji:",prederr)
				lres<-try(prederror(classifier,moutput,parvec,cDataSet[-etykiety,],EvalString),TRUE)
				if(!inherits(lres, "try-error")){
					vecp<-c(vecp,lres$perror)
					if(lres$perror<bep){
						bep<-lres$perror;
						betykiety<-etykiety;
						bclas<-classifier;
					}
				}
			}
		}
		if(bep<1){
			meanp<-sum(vecp)/length(vecp)
			if(meanp<gbep){ 
				gbep<-meanp
				gbetykiety<-betykiety
				lb<-c(lb, i)
				ibest<-i
				gbclas<-bclas
			}
		}
	}
	#cat("\nNajlepszy dla n:",numvar,"zmiennych: i:",ibest,"\n")
	#print(parvec)
	#print(gbclas)
	return (list(ibest,lb,numvar,betykiety,gbclas))
}


#Funkcja polrwithattr znajduje najlepsz± logistyczn± regresjê dla parametrów
#moutput - wyj¶cie modelu zmienna zale¿na np. "RI", 
#parvec - wektor zmiennych wej¶ciowych niezale¿nych dla modelu,
#numvar - ile zmiennych z parvec ma wzi±æ udzia³ w combiutacji zmiennych niezale¿nych
polrwithattr<-function (DataSet, moutput, parvec, numvar, EvalString="DEFAULT"){
	if(numvar<length(parvec)){
		combi<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
		rowcombi<-nrow(combi)
	}
	else rowcombi<-1
	lb<-c(); ibest<-0; br2<-1000000000;
	for(i in 1:rowcombi){
		if(numvar<length(parvec)) 	varplus<-combi[i,]
		if(numvar==length(parvec))	varplus<-parvec
		m01<-try(evalwithattr("polr",moutput,varplus,DataSet,EvalString),TRUE);
		if(!inherits(m01, "try-error")){
			sm01<-summary(m01)
			if(sm01$deviance < br2){ 
				br2<-sm01$deviance
				lb<-c(lb, i)
				ibest<-i
			}
		}
	}
	return (list(ibest,lb))
}


#Funkcja lmwithattr znajduje najlepsz± liniow± regresjê dla parametrów
#moutput - wyj¶cie modelu zmienna zale¿na np. "RI", 
#parvec - wektor zmiennych wej¶ciowych niezale¿nych dla modelu,
#numvar - ile zmiennych z parvec ma wzi±æ udzia³ w combiutacji zmiennych niezale¿nych
lmwithattr<-function (DataSet, moutput, parvec, numvar, nleven=-1, alpha=0.01, EvalString="DEFAULT"){
	if(numvar<length(parvec)){
		combi<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
		rowcombi<-nrow(combi)
	}
	else rowcombi<-1
	lb<-c(); ibest<-0; br2<-0; nbs<-1;
	for(i in 1:rowcombi){
		if(numvar<length(parvec)) 	varplus<-combi[i,]
		if(numvar==length(parvec))	varplus<-parvec;
		m01<-try(evalwithattr("lm",moutput,varplus,DataSet,EvalString),TRUE);
		if(!inherits(m01, "try-error")){
			#p.value <- 1-pf(f.stat["value"],f.stat["numdf"],f.stat["dendf"])			
			sm01<-summary(m01)
			#cat(" every:",as.character(sm01$call)," -: ",length(varplus)," :: ");
			#print(sm01$coefficients[,4]);
			#cat(is.finite(sm01$fstatistic["value"])," <> ")
			#cat(isTRUE(all.equal(as.vector(is.finite(sm01$coefficients[,4])),rep(TRUE,nbs*length(varplus)+1))))
			if(EvalString=="SPLINE") nbs<-3 else nbs<-1
			if(is.finite(sm01$fstatistic["value"])
					&&(isTRUE(all.equal(as.vector(is.finite(sm01$coefficients[,4])),rep(TRUE,nbs*length(varplus)+1))))){
				#tmp=paste("anova(",deparse(substitute(lm)),"(",moutput,"~1,DataSet),m01)",sep="")
				#an<-anova(lm(RI~1),m01);
				#an<-eval(parse(text=tmp))
				if(nleven>0){
					mintervals<-cut(m01$fitted.values,nleven)
					lt<-levene.test(m01$residuals,factor(mintervals))
				}
				if(nleven<0)
					bp<-evalwithattr("bptest",moutput,varplus,DataSet)
				#levene pominiêty dla nleven=0, dla nleven < 0 bptest
				if((nleven==0 && qf(0.99,1,m01$df)<sm01$fstatistic["value"] && sm01$r.squared > br2) 
							|| (nleven < 0 && qf(0.99,1,m01$df)<sm01$fstatistic["value"] && sm01$r.squared > br2 && bp$p.value > alpha)
							|| (nleven > 0 && qf(0.99,1,m01$df)<sm01$fstatistic["value"] && sm01$r.squared > br2 && lt$"Pr(>F)"[1] > alpha && qf(1-alpha,lt$Df[1],lt$Df[2]) > lt$"F value"[1])){
					br2<-sm01$r.squared
					lb<-c(lb, i)
					ibest<-i
					#cat("\n best:",as.character(sm01$call)," -: ",sm01$fstatistic["value"])
				}
			}
		}
	}
	return (list(ibest,lb))
}

#Funkcja evalwithattr wywo³uje podan± funkcjê z wyj¶ciowym atrybutem output i 
#wej¶ciowymi atrybutami parvec oraz zbiorem danych
evalwithattr<-function(nFunction,output,parvec,oData,EvalString="DEFAULT")
{
	tmp=paste(nFunction,"(",output,"~",sep="")
	if(EvalString=="SPLINE")  
		parvec<-paste("bs(",parvec,")",sep="")
	parvec<-paste(parvec,collapse="+")
	if(EvalString=="DEFAULT" || EvalString=="SPLINE" || EvalString=="CLASS") 
#		tmp<-paste(tmp,parvec,",data=",deparse(substitute(oData)),")",sep="")
		tmp<-paste(tmp,parvec,",data=oData)",sep="")
	else
		tmp<-paste(tmp,parvec,",data=oData,",EvalString,")",sep="")
#		tmp<-paste(tmp,parvec,",data=",deparse(substitute(oData)),",",EvalString,")",sep="")
	return(eval(parse(text=tmp)))
#	return(tmp)
}

#Funkcja printwithattr wy¶wietla podan± funkcjê z wyj¶ciowym atrybutem output i 
#wej¶ciowymi atrybutami parvec oraz zbiorem danych
printwithattr<-function(nFunction,output,parvec,oData,EvalString="DEFAULT")
{
	mcall<-match.call()
	tmp=paste(nFunction,"(",output,"~",sep="")
	if(EvalString=="SPLINE")  
		parvec<-paste("bs(",parvec,")",sep="")
	parvec<-paste(parvec,collapse="+")
	if(EvalString=="DEFAULT" || EvalString=="SPLINE" || EvalString=="CLASS") 
#		tmp<-paste(tmp,parvec,",data=",deparse(substitute(oData)),")",sep="")
		tmp<-paste(tmp,parvec,",data=",as.character(mcall$oData),")",sep="")
	else
		tmp<-paste(tmp,parvec,",data=",as.character(mcall$oData),",",EvalString,")",sep="")
#		tmp<-paste(tmp,parvec,",data=",deparse(substitute(oData)),",",EvalString,")",sep="")
#	return(eval(parse(text=tmp)))
	return(tmp)
}

#Metoda funkcja Vif okre¶la stopieñ korelacji zmiennych niezale¿nych, 
#podaje warto¶ci takie same jak vif z pakietu car, a jest prostsza i dzia³a dla jednej zmiennej (podaje 1) 
Vif <- function(object, ...)
	UseMethod("Vif")

Vif.default <- function(object, ...)
	stop("No default method for Vif.  Sorry.")

Vif.lm <- function(object, ...) {       
	V <- summary(object)$cov.unscaled
	Vi <- crossprod(model.matrix(object))
	nam <- names(coef(object))
	if(k <- match("(Intercept)", nam, nomatch = F)) {
		v1 <- diag(V)[-k]
		v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
		nam <- nam[-k]
	} else {
		v1 <- diag(V)
		v2 <- diag(Vi)
		warning("No intercept term detected.  Results may
						surprise.")
	}
	structure(v1*v2, names = nam)
}

#Funkcja defactor.numeric najpierw defaktoryzuje, a potem oznacza jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi, tak na wszelki wypadek, gdyby csv ¼le siê wczyta³ (w przypadku zbiorów data() to tylko æwiczenie)
defactor.numeric<-function (DataSet, parvec)         
{
	DataSet<-unfactorto(DataSet, which(names(DataSet) %in% parvec))
	for(i in 1:ncol(DataSet)){
        	if(i%in%which(names(DataSet) %in% parvec))
	        DataSet[,i]<-as.numeric(DataSet[,i])
	}
 	return (DataSet)
}

#Funkcja scale.numeric skaluje wybrane numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi
#z paramerami CENTER=TRUE mean=0, a dla SCALE=TRUE sd=1
scale_for<-function (DataSet, parvec, CENTER, SCALE)         
{
	mtemp<-DataSet[parvec]
	mtemp<-as.data.frame(scale(mtemp,center=CENTER,scale=SCALE))
	DataSet[parvec]<-mtemp
	return (DataSet)
}


#Funkcja zscore.for.integer zetskoruje wybrane kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi po kolumnie zdyskretyzowanej (etykiecie) integercolumnforzscore dla poszczególnych jej warto¶ci
zscore.for.integer<-function (DataSet, parvec, integercolumnforzscore)         
{
	indata<-DataSet
	for(i in sort(unique(DataSet[[integercolumnforzscore]]))){
		indata[(indata[[integercolumnforzscore]]==i),]=zscore(indata[(indata[[integercolumnforzscore]]==i),],which(names(indata) %in% parvec))
	}
	return(indata)
}

#Funkcja discret.for.chosen dyskretyzujê atrybuty (kolumny) z parvec na levelnum poziomów
disc.for.chosen<-function (DataSet, parvec, levelnum)         
{
	DataSetd<-DataSet
	DataSetd[,parvec]<-disc.ef(DataSet[,parvec], levelnum)
	DataSetd<-factorto(DataSetd, which(names(DataSetd) %in% parvec))
	return(DataSetd)
}

#Funkcja KruskelMDS generuje z daisy ró¿nice miêdzy wierszami podanego zbioru i wprowadza do isoMDS rzutuj±cego na wymiary k=dimnum (jak siê pojawi± dwa takie same wiersze to wyrzuca b³±d, dlatego na samym pocz±tku usuwa³em wiersze z Glass
KruskelMDS<-function (DataSet, parvec, dimnum)         
{
	DataSet<-subset(DataSet,!duplicated(DataSet))
	return(isoMDS(daisy(DataSet[,which(names(DataSet)%in%parvec)]),k=dimnum))
}

plotMDS.for.chosen<-function (fname, nDataSets, DataSet, parvec, wzorzec1)         
{
	DataSet1<-DataSet[,which(names(DataSet)%in%parvec)]
	for(i in which(names(DataSet1)%in%parvec)){
		wzorzec=DataSet1[,i]
		#wzorzec1=2
		if(i < 10){
			zapisz_pplot(nDataSets,paste(fname,"_0",i,parvec[i],sep=""),wzorzec,wzorzec1,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),3.5)
			#zapisz_pplot(nDataSets,paste(fname,"_0",i,parvec[i],sep=""),wzorzec,2,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),wzorzec1)
		}else{
			zapisz_pplot(nDataSets,paste(fname,"_",i,parvec[i],sep=""),wzorzec,wzorzec1,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),3.5)
		}
	}
}

#Funkcja disc2 wykonywana w funkcji disc.ef
disc2<-function (x, k) 
{
	z = x
    	n = length(x)
	f = seq(n)
	if(sum(is.na(x))!=0)
	{
		f<-f[-which(is.na(x))]
		x<-x[-which(is.na(x))]
	}
	m = length(x)
	ciclo = ceiling(m/k)
	y = x
	for (i in 1:(k - 1)) {
	    y[order(x)[((i - 1) * ciclo + 1):(i * ciclo)]] = i
	}
	y[order(x)[((k - 1) * ciclo + 1):m]] = k
	z[f]<-y; 
	return(z)
}

#Funkcja disc.ef z pakietu dprep zmieniona (pomija nulle), 
#gdy¿ w tym pakiecie prawie wszystkie funkcje wymagaj± zmian 
disc.ef<-function (indata, k)        # Nastêpuje dyskretyzacja danych
{
	varcon<-seq(1,ncol(indata))
    indata = as.matrix(indata)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
            indata[, i] <- disc2(as.vector(indata[, i]), k)
        }
    }
    indata
}

#Funkcja zscore zeskoruje dane ca³kowite lub zmiennoprzecinkowe 
#czyli odejmuje ¶redni± i dzieli przez standardowe odchylenie
zscore<-function (indata, varcon)        # Nastêpuje dyskretyzacja danych
{
#    indata = as.matrix(indata)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
			z = indata[, i]
			x = indata[, i]
    		n = length(x)
			fs = seq(n)
			if(sum(is.na(x))!=0)
			{
				fs<-fs[-which(is.na(x))]
				x<-as.numeric(x[-which(is.na(x))])
			} 
			if (sum(x!=0) != 0) {
				if(sd(x)!=0)
            		x<-(x-mean(x))/sd(x)
				else x[x!=0]=0
			}
			z[fs]=x
			indata[, i]=z
        }
    }
    indata
}

#Funkcja factorto faktoryzuje dane wej¶ciowe z domy¶lnymi poziomami
factorto<-function (indata, varcon)  
{
	varcon <- as.vector(varcon)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
            indata[, i] <- factor(indata[, i])
        }
    }
    indata
}

#Funkcja unfactorto defaktoryzuje dane wej¶ciowe z ustalonymi poziomami
unfactorto<-function (indata, varcon)  
{
	varcon <- as.vector(varcon)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
			if(!is.na(as.numeric(as.character(indata[1,i])))){
            	indata[, i] <- as.numeric(as.character(indata[, i]))
			}
			if(is.na(as.numeric(as.character(indata[1,i])))){
            	indata[, i] <- as.character(indata[, i])
			}
        }
    }
    indata
}

#Funkcja "zmianana"  zamienia warto¶ci 9; 99; 99.9 na NA
zmianana<- function (kolumna_wyjatek, wartoscNA)         
{
 	x<-kolumna_wyjatek
    is.na(x)<-which(x==wartoscNA) 
 	return (x)
}

#Funkcja z4na3 zamienia przedzia³y 1,2,3,4 na 1,2,2,3
z4na3<-function (indata, varcon)
{
    indata = as.matrix(indata)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
		 	if(sum(!is.na(indata[indata[,i]==3, i])))
            	indata[indata[,i]==3, i] = 2;
		 	if(sum(!is.na(indata[indata[,i]==4, i])))
            	indata[indata[,i]==4, i] = 3;
        }
    }
    indata
}


#Funkcja zapisz_pplot zapisuje wykres rzutu wielowymiarowego w pliku jpeg
zapisz_pplot = function (npointszds,fname,wzorzec1,wzorzec2,co,pc,scex)
{
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=scex)
	#plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=c(2,3.5,5,6.5,8,9.5,11))
	dev.off()
}

#Funkcja zapisz_weka zapisuje wykres drzewa uzyskanego procedurami RWeka w pliku png
zapisz_weka = function (drzewo, fname)
{
	#dotname=paste(fname,".dot",sep="")
	dotname=tempfile()
	pngname=paste(fname,".png",sep="")
	write_to_dot(drzewo,dotname)
	system(paste("dot -Tpng ",dotname," > ",pngname,sep=""))
}

#Funkcja zapisz_rpart zapisuje wykres drzewa uzyskanego procedurami rpart w pliku jpeg
zapisz_rpart = function (drzewo, fname)
{
    jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(lwd=4)
	draw.tree(drzewo, cex=3.3, pch=1.0, print.levels=TRUE)
   # plot(drzewo,uniform=T,branch=0.3,compress=T,margin=0.02)
   # text(drzewo,all=T,use.n=T, fancy=T)
    dev.off()
}

#próba napisania uniwersalej funkcji write2jpg
write2jpg <- function(object, ...)
	UseMethod("write2jpg")

write2jpg.default <- function(object, ...)
	stop("No default method for write2jpg.  Sorry.")

write2jpg.lm <- function(object, ...) {       
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(lwd=4)
	
	dev.off()
}

#Funkcja hier2jpg zapisuje dendogram w pliku jpeg
hier2jpg<-function(inmethod,indata,fname){
	indatanum<-indata[, sapply(indata, class) == "numeric"]
	cc <- cor(indatanum, use="pairwise", method=inmethod)
	# Generate hierarchical cluster of variables.
	hc <- hclust(dist(cc), "ave")
	# Generate the dendrogram.
	dn <- as.dendrogram(hc)
	# Now draw the dendrogram.
	#op <- par(mar = c(3, 4, 3, 2.86))
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(mar=c(9,9,9,9))
	plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "black"), lab.cex = 3.75, cex.main = 1.8, cex.axis = 1.2,  lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2))
	title(main=paste("Variable Correlation Clusters ",as.character(substitute(indata)),"using",inmethod),cex.main=2)
	#par(op)
	dev.off()
}

#Funkcja latt2jpg zapisuje lattice w pliku jpeg
latt2jpg<-function(indata, gvec, fname){
	dat<-indata[, sapply(indata, class) == "numeric"]
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	## Assuming that the data are attached and any
	## customised style settings are in place; save with
	## myStyle <- trellis.par.get(); then restore with
	## trellis.par.set(myStyle)
	fvec=as.character(substitute(gvec))
	print(marginal.plot(dat, data = dat, groups = gvec, par.settings = list(cex=2.6), auto.key = list(lines = TRUE, title = paste("Variable Plots by ",fvec[3]), cex.title = 3, columns = 2)))
	opar <- trellis.par.set(list(plot.symbol = list( cex = 2.6), dot.symbol = list( cex = 2.6), par.main = list( cex = 2.6), par.sub.text = list( cex = 2.6), par.xlab.text = list( cex = 2.6), plot.line = list(), plot.polygon = list(), superpose.symbol = list(cex = 2.6), superpose.line = list(), superpose.polygon = list()))
	#latticeStyleToBasePar()
	on.exit(trellis.par.set(opar))
	dev.off()
}


#Funkcja meanverr oblicza ¶redni± z list list wyników 
meanverr<-function(lverr,jmax,imax){
	verr<-c()
	for(i in 1:imax){
		tmp<-"("; mdiv<-jmax;  
		for(j in 1:jmax){
			if(!is.na(lverr[[j]][i]))
				tmp<-paste(tmp,deparse(substitute(lverr)),"[[",j,"]][",i,"]",sep="");
			if(j!=jmax && !is.na(lverr[[j+1]][i])) tmp<-paste(tmp,"+",sep="");
			if(is.na(lverr[[j]][i])) {mdiv<-mdiv-1;}
		}
		tmp<-paste(tmp,")/",mdiv,sep="")
		#cat(tmp,"\n")
		verr<-c(verr,eval(parse(text=tmp)))
	}; 
	verr
}


##################################################################################################
#REGRESJA LINIOWA - redukcja parametrów i atrybutów, funkcje do isowitalterlm.r
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

