# zbiór funkcji do zajęć EU SI 
# TODO: test, testować
# Licence LGPL  
# Author: Piotr Wąsiewicz
########################################################################################################

Sys.setlocale("LC_NUMERIC","C") 

#lista pakietów z CRAN-u
#naprawić pakier gRain
#pkglist<-c("gRain")
pkglist<-c("devtools","TTR","TSPostgreSQL","modeest","flexclust","rgl","magic","snowfall","bnlearn","snow","ggplot2","reshape","gbm","arules","mboost","bestglm","ElemStatLearn","faraway","relaimpo","leaps","lars","bootstrap","DAAG","ff","biglm","bigmemory","splines","betareg","ellipse","nlme","MASS","leaps","lmtest","foreign","plyr","mlbench","boot","Hmisc","RWeka","ipred","klaR","ROCR","rpart","maptree","party","grid","lattice","latticeExtra","playwith","ada","randomForest","kknn","e1071","cluster","class","fda","zoo","lattice","deal","RJDBC","cairoDevice","iterators","doMC","foreach","nws","PTAk","Rcpp","Boruta")

pkgcheck <- pkglist %in% row.names(installed.packages())
for(i in pkglist[!pkgcheck]){
	install.packages(i,depend=TRUE)
}

#lista pakietów z bioconductora
biolist<-c("Rgraphviz","PROcess")
biocheck <- biolist %in% row.names(installed.packages())
for(i in biolist[!biocheck]){
    source("http://bioconductor.org/biocLite.R")
	#update.packages(repos=biocinstallRepos(), ask=FALSE)
	biocLite(i)
}

pkgrforge<-c("mlr","tm.plugin.dc","hive")
pkgcheck <- pkgrforge %in% row.names(installed.packages())
for(i in pkgrforge[!pkgcheck]){
	install.packages(i, repos="http://R-Forge.R-project.org")
}

pkgall<-c(pkglist,biolist,pkgrforge)

for(i in c("bnlearn","lmtest","bestglm","leaps","lars","mda","xtable","lattice","TSPostgreSQL","RWeka","doMC","foreach","nws","ROCR","rpart","e1071","MASS","cluster","party","randomForest","PROcess"))
{ library(i, character.only = TRUE);}

#pkgbyhand<-c("rparallel")
#for(i in pkgbyhand)
#{ library(i, character.only = TRUE);}

#######################################################################################################
#############################################################################
#Funkcje podstawowe zalecane do procesu odkrywania wiedzy
#############################################################################

#######################################################################################################
#Funkcja most frequent factor
mff<-function(x){
	names(table ( x ))[which ( table ( x ) == max ( table ( x ) ) )]
}

#######################################################################################################
#Funkcja brutoptim.klas znajduje najlepszy klasyfikator przeszukując wszystkie kombinacje atrybutów
brutoptim.klas <-function(mypathout,nData,nFunction,paroutputree,parvectree,percent,trials,evalstr){
	DataSet<-get(nData)
	DataSet<-DataSet[!is.na(DataSet[[paroutputree]]),]
	pred_norm<-combestklas(nFunction, paste(mypathout,nData,"_pred_",nFunction,"_",paroutputree,sep=""),DataSet, paroutputree,parvectree,percent,trials,evalstr)
	if(length(pred_norm))
	{
		y<-seq(1, length(pred_norm));y<-sapply(y,function(x){if(x %% 5) x<-0 else x<-x});y<-y[!y==0];
		z<-c();for(i in y) z<-c(z,pred_norm[[i-1]]$perror); n<-min(which(z==min(z)))
		bestclass<-pred_norm[[(n-1)*5+3]]
		betykiety<-pred_norm[[(n-1)*5+1]]
		bestli<-pred_norm[[(n-1)*5+2]]
		bestn<-pred_norm[[(n-1)*5+5]]
		list(bestclass=bestclass,betykiety=betykiety)
	} else list()
}

#######################################################################################################
#Funkcja prederror zwraca wartości predykcji klasyfikatora, tablicę rezultatów i rzeczywistych wartości, 
#oraz błąd klasyfikatora, na wejściu dane testowe, ale nie treningowe wykorzystane do konstrukcji klasyfikatora
prederror<-function(classimod,nFunction,paroutputree,parvectree,DataSet,EvalString="DEFAULT"){
	if (EvalString == "CLASS"){
		predvalues=predict(classimod,DataSet[,parvectree])$class
	} else if(EvalString=="DEFAULT" || nFunction =="rpart"){
		predvalues=predict(classimod,newdata=DataSet[,parvectree],"class");
	}  else {
		predvalues=predict(classimod,DataSet[,parvectree]);
	}
	if(length(predvalues)==length(DataSet[,paroutputree]))
		tabresults=table(predicted=predvalues, real=DataSet[,paroutputree])
	else tabresults<-c()
	prederr<-1-sum(diag(tabresults))/sum(tabresults)
	l<-list()
	l[["pvalues"]]<-predvalues
	l[["table"]]<-tabresults
	l[["perror"]]<-prederr
	l
}

#######################################################################################################
#Funkcja combestklas liczy kombinacje zmiennych objaśniających niezależnych dla n=1 do n równego ilość wszystkich zmiennych 
#i oblicza dla nich funkcje get(nFunction) i wybiera najlepszą dla każdego n, wraca listę najlepszych obiektów z kolejnych iteracji  
#w lb<n> mamy wybierane po kolei kolejne zestawy na najlepszą regresję 
#w m<n> najlepszy model dla n, w sm<n> summary(m<n>), dla n=1 rysuje plot regresji
combestklas<-function (nFunction, fname, DataSet, moutput, parvec, nleven=-1, alpha=0.01, EvalString="DEFAULT"){
	l<-list();ilist<-list();etykiety<-seq(1,nrow(DataSet));m01<-FALSE
	n<-length(parvec)
	varvec<-parvec
	varplus<-paste(parvec,collapse="+")
	for(numvar in 1:length(parvec)){
		#cat(paste("1:",moutput,"~",varplus)," n:",numvar," length:",n," EvalString:",EvalString,"\n")
		cat("\n i: ",numvar)
		ilist<-classwithattr(nFunction,DataSet,moutput,parvec,numvar,nleven,alpha,EvalString)
		etykiety<-ilist[[4]]
		m01<-ilist[[5]]
		i<-ilist[[1]]; 
		if(i){
			if(numvar<n){
				combi<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
				varvec<-combi[i,]
			}
			else if(numvar==n) varvec<-parvec
			if(EvalString=="SPLINE") varplus<-paste("bs(",varvec,")",sep="") else varplus<-varvec
			varplus<-paste(varplus,collapse="+")
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
				assign(smnv,prederror(m01,nFunction,moutput,parvec,DataSet[! row.names(DataSet) %in% etykiety,],EvalString));
				l[[etnv]]<-get(etnv)
				l[[lbnv]]<-get(lbnv)
				l[[mnv]]<-get(mnv)
				l[[smnv]]<-get(smnv)
				l[[numv]]<-get(numv)
			}
		}
	}
	return (l)
}


#######################################################################################################
#Funkcja combesteval liczy kombinacje zmiennych objaśniających niezależnych dla n=1 do n równego ilość wszystkich zmiennych 
#i oblicza dla nich funkcje get(nFunction) i wybiera najlepszą dla każdego n, wraca listę najlepszych obiektów z kolejnych iteracji  
#w lb<n> mamy wybierane po kolei kolejne zestawy na najlepszą regresję 
#w m<n> najlepszy model dla n, w sm<n> summary(m<n>), dla n=1 rysuje plot regresji
combesteval<-function (nFunction, fname, DataSet, moutput, parvec, nleven=-1, alpha=0.01, EvalString="DEFAULT"){
	l<-list();ilist<-list();etykiety<-seq(1,nrow(DataSet));m01<-FALSE
	#cat(" NR:",nrow(DataSet))
	n<-length(parvec)
	varvec<-parvec
	varplus<-paste(parvec,collapse="+")
	for(numvar in 1:length(parvec)){
		#cat(paste("1:",moutput,"~",varplus)," n:",numvar," length:",n," EvalString:",EvalString,"\n")
		cat("\n i: ",numvar)
		if(nFunction=="lm")
			ilist<-lmwithattr(DataSet,moutput,parvec,numvar,nleven,alpha,EvalString)
		else if(nFunction=="polr")
			ilist<-polrwithattr(DataSet,moutput,parvec,numvar,EvalString)
		else {
			#cat(paste("1:",moutput,"~",varplus)," n:",numvar," length:",n," EvalString:",EvalString,"\n")
			ilist<-classwithattr(nFunction,DataSet,moutput,parvec,numvar,nleven,alpha,EvalString)
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
			m01<-try(evalwithattr(nFunction,moutput,varvec,DataSet[etykiety,],EvalString),TRUE)
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
				#print(ilist)
				assign(etnv,etykiety);
				assign(numv,numvar);
				if(nFunction%in%c("lm","polr"))
					assign(smnv,summary(get(mnv)))
				else{ 
					assign(smnv,prederror(m01,nFunction,moutput,parvec,DataSet[! row.names(DataSet) %in% etykiety,],EvalString));
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
						vartemp<-eval(parse(text=paste("DataSet$",varvec,sep="")))
						rangetemp<-seq(min(vartemp),max(vartemp),length.out=213)
						plot(eval(parse(text=paste(moutput,"~",varvec))),data=DataSet,main=paste(moutput,"~",varplus), pch=1.0, cex.lab=1.5)
						dframetemp<-as.data.frame(rangetemp)
						names(dframetemp)<-varvec
						lines(rangetemp,predict(get(mnv),newdata=dframetemp), col="red", lwd=3)
						#cat(paste("3:",moutput,"~",varvec)," n:",numvar," mnv:",mnv," varvec:",paste("DataSet$",varvec,sep=""),"\n")
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

#######################################################################################################
#Funkcja classwithattr znajduje najlepszy klasyfikator dla numvar zmiennych - atrybutów
#moutput - wyjście modelu zmienna zależna np. "Type", 
#parvec - wektor zmiennych wejściowych niezależnych dla modelu,
#numvar - ile zmiennych z parvec ma wziąć udział w combiutacji zmiennych niezależnych
classwithattr<-function (nFunction, DataSet, moutput, parvec, numvar, percent=70, trials=5,EvalString="DEFAULT"){
	if(numvar<length(parvec)){ 
		combi<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
		rowcombi<-nrow(combi)
	}else rowcombi<-1
	etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*(percent/100)))
	lb<-c(); ibest<-0; lres<-list(); gbep<-1; gbetykiety<-c(); gbclas<-FALSE;div<-0;
	for(i in 1:rowcombi){
		cat(".")
		if(numvar<length(parvec)) 	varplus<-combi[i,]
		if(numvar==length(parvec))	varplus<-parvec
		vecp<-c(); bep<-1.1; betykiety<-c(); bclas<-FALSE;
		for(j in 1:trials){
			classifier<-FALSE
			etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*(percent/100)))
			if(length(unique(DataSet[,moutput]))==2){
				while((table(DataSet[etykiety,moutput])[1]*table(DataSet[etykiety,moutput])[2])==0){
					cat("-")
					etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*(percent/100)))
				}
			}
			classifier<-try(evalwithattr(nFunction,moutput,varplus,DataSet[etykiety,],EvalString),TRUE)
			#print(classifier)
			if(!inherits(classifier, "try-error")){
				lres<-try(prederror(classifier,nFunction,moutput,parvec,DataSet[-etykiety,],EvalString),TRUE)
				if((!inherits(lres, "try-error"))&&(((length(unique(DataSet[,moutput]))==2)&&(length(unique(lres$pvalues))>1)&&(det(matrix(lres$table, ncol=length(unique(DataSet[,moutput]))))>0))||(length(unique(DataSet[,moutput]))>2))){
					vecp<-c(vecp,lres$perror)
					#print(lres)
					if(!is.na(lres$perror<bep))
						if(lres$perror<bep ){
							bep<-lres$perror;
							betykiety<-etykiety;
							bclas<-classifier;
					}
				}
			}
		}
		if(bep<=1){
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
	return (list(ibest,lb,numvar,gbetykiety,gbclas))
}


#######################################################################################################
#Funkcja polrwithattr znajduje najlepszą logistyczną regresję dla parametrów
#moutput - wyjście modelu zmienna zależna np. "RI", 
#parvec - wektor zmiennych wejściowych niezależnych dla modelu,
#numvar - ile zmiennych z parvec ma wziąć udział w combiutacji zmiennych niezależnych
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


#######################################################################################################
#Funkcja lmwithattr znajduje najlepszą liniową regresję dla parametrów
#moutput - wyjście modelu zmienna zależna np. "RI", 
#parvec - wektor zmiennych wejściowych niezależnych dla modelu,
#numvar - ile zmiennych z parvec ma wziąć udział w combiutacji zmiennych niezależnych
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
				#levene pominięty dla nleven=0, dla nleven < 0 bptest
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

#######################################################################################################
#Funkcja evalwithattr wywołuje podaną funkcję z wyjściowym atrybutem output i 
#wejściowymi atrybutami parvec oraz zbiorem danych
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

#######################################################################################################
#Funkcja printwithattr wyświetla podaną funkcję z wyjściowym atrybutem output i 
#wejściowymi atrybutami parvec oraz zbiorem danych
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

#######################################################################################################
#Metoda funkcja Vif określa stopień korelacji zmiennych niezależnych, 
#podaje wartości takie same jak vif z pakietu car, a jest prostsza i działa dla jednej zmiennej (podaje 1) 
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

#######################################################################################################
#Funkcja defactor.numeric najpierw defaktoryzuje, a potem oznacza jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i całkowitymi, tak na wszelki wypadek, gdyby csv źle się wczytał (w przypadku zbiorów data() to tylko ćwiczenie)
defactor.numeric<-function (DataSet, parvec)         
{
	DataSet<-unfactorto(DataSet, which(names(DataSet) %in% parvec))
	for(i in 1:ncol(DataSet)){
        	if(i%in%which(names(DataSet) %in% parvec))
	        DataSet[,i]<-as.numeric(DataSet[,i])
	}
 	return (DataSet)
}

#######################################################################################################
#Funkcja normfactor zmienia faktory na numery i ponownie faktory
normfactor<-function (DataSet, parvecfactor)         
{
	for(i in parvecfactor){
		x<-as.character(DataSet[,i])
		z = x
		n = length(x)
		f = seq(n)
		if(sum(is.na(x))!=0)
		{
			f<-f[-which(is.na(x))]
			x<-x[-which(is.na(x))]
		}
		m = length(x)
		nfn=1:length(unique(x))
		ofn=sort(unique(x))
		y<-x; k=0
		for(j in ofn){ 
			k<-k+1
			y[x==j]<-k
		}
		z[f]<-y
		DataSet[,i]<-factor(as.numeric(z))
	}
	DataSet
}

#######################################################################################################
#Funkcja scale.numeric skaluje wybrane numeryczne kolumny z liczbami zmiennoprzecinkowymi i całkowitymi
#z paramerami CENTER=TRUE mean=0, a dla SCALE=TRUE sd=1
scale_for<-function (DataSet, parvec, CENTER, SCALE)         
{
	mtemp<-DataSet[parvec]
	mtemp<-as.data.frame(scale(mtemp,center=CENTER,scale=SCALE))
	DataSet[parvec]<-mtemp
	return (DataSet)
}


#######################################################################################################
#Funkcja zscore.for.integer zetskoruje wybrane kolumny z liczbami zmiennoprzecinkowymi i całkowitymi po kolumnie zdyskretyzowanej (etykiecie) integercolumnforzscore dla poszczególnych jej wartości
zscore.for.integer<-function (DataSet, parvec, integercolumnforzscore)         
{
	indata<-DataSet
	if(length(integercolumnforzscore))
		for(i in sort(unique(DataSet[[integercolumnforzscore]]))){
			indata[(indata[[integercolumnforzscore]]==i),]=zscore(indata[(indata[[integercolumnforzscore]]==i),],which(names(indata) %in% parvec))
		}
	return(indata)
}

#######################################################################################################
#Funkcja discret.for.chosen dyskretyzuję atrybuty (kolumny) z parvec na levelnum poziomów
disc.for.scale<-function (DataSetd, parvec, scale)         
{
	if(length(scale)>0&length(DataSetd[DataSetd[,c(parvec)]<=scale[1],c(parvec)]))
		DataSetd[DataSetd[,c(parvec)]<=scale[1],c(parvec)]<-1	
	if(length(scale)>1){
		for(i in 2:length(scale)){
			if(length(DataSetd[DataSetd[,c(parvec)]>scale[i-1]&DataSetd[,c(parvec)]<=scale[i],c(parvec)]))
				DataSetd[DataSetd[,c(parvec)]>scale[i-1]&DataSetd[,c(parvec)]<=scale[i],c(parvec)]<-i
		}
		if(length(DataSetd[DataSet[,c(parvec)]>scale[length(scale)],c(parvec)]))
			DataSetd[DataSet[,c(parvec)]>scale[length(scale)],c(parvec)]<-length(scale)+1
	}
	DataSetd<-factorto(DataSetd, which(names(DataSetd) %in% parvec))
	return(DataSetd)
}



#######################################################################################################
#Funkcja discret.for.chosen dyskretyzuję atrybuty (kolumny) z parvec na levelnum poziomów
disc.for.chosen<-function (DataSet, parvec, levelnum, zero=-1)         
{
	DataSetd<-DataSet
	DataSetd[,parvec]<-disc.ef(DataSet[,parvec], levelnum, zero)
	DataSetd<-factorto(DataSetd, which(names(DataSetd) %in% parvec))
	return(DataSetd)
}

#######################################################################################################
#Funkcja KruskelMDS generuje z daisy różnice między wierszami podanego zbioru i wprowadza do isoMDS rzutującego na wymiary k=dimnum (jak się pojawią dwa takie same wiersze to wyrzuca błąd, dlatego na samym początku usuwałem wiersze z Glass
KruskelMDS<-function (DataSet, parvec, dimnum)         
{
	DataSet<-subset(DataSet,!duplicated(DataSet))
	return(isoMDS(daisy(DataSet[,which(names(DataSet)%in%parvec)]),k=dimnum))
}

#######################################################################################################
#Funkcja plotMDS.for.chosen zapisuje rzuty wielowymiarowe na płaszczyznę
plotMDS.for.chosen<-function (fname, nDataSets, DataSetd, parvec, wzorzec1)         
{
	DataSet1<-DataSetd[,which(names(DataSetd)%in%parvec)]
	for(i in which(names(DataSet1)%in%parvec)){
		wzorzec=as.integer(DataSet1[,i])
		print(names(DataSet1)[i])
		print(wzorzec)
		#wzorzec1=2
		zapisz_pplot(nDataSets,paste(fname,names(DataSet1)[i],sep=""),wzorzec,wzorzec1,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),3.5)
			#zapisz_pplot(nDataSets,paste(fname,"_0",i,parvec[i],sep=""),wzorzec,2,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),wzorzec1)
	}
}

#######################################################################################################
#Funkcja disc2 wykonywana w funkcji disc.ef
disc2<-function (x, k, zero=-1) 
{
	z = x
    n = length(x)
	f = seq(n)
	if(sum(is.na(x))!=0)
	{
		f<-f[-which(is.na(x))]
		x<-x[-which(is.na(x))]
	}
	if(zero>=0)
	{
		f<-f[-which(x==zero)]
		x<-x[-which(x==zero)]
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

#######################################################################################################
#Funkcja disc.ef z pakietu dprep zmieniona (pomija nulle), 
#gdyż w tym pakiecie prawie wszystkie funkcje wymagają zmian 
disc.ef<-function (indata, k, zero=-1)        # Następuje dyskretyzacja danych
{
	indata = as.matrix(indata)
	if(ncol(indata)>1)
		varcon<-seq(1,ncol(indata)) else varcon=1
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
            indata[, i] <- disc2(as.vector(indata[, i]), k, zero)
        }
    }
    indata
}

#######################################################################################################
#Funkcja zscore zeskoruje dane całkowite lub zmiennoprzecinkowe 
#czyli odejmuje średnią i dzieli przez standardowe odchylenie
zscore<-function (indata, varcon)        # Następuje dyskretyzacja danych
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

#######################################################################################################
#Funkcja factorto faktoryzuje dane wejściowe z domyślnymi poziomami
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

#######################################################################################################
#Funkcja unfactorto defaktoryzuje dane wejściowe z ustalonymi poziomami
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

#######################################################################################################
#Funkcja "zmianana"  zamienia wartości 9; 99; 99.9 na NA
zmianana<- function (kolumna_wyjatek, wartoscNA)         
{
 	x<-kolumna_wyjatek
    is.na(x)<-which(x==wartoscNA) 
 	return (x)
}


#######################################################################################################
#Funkcja "zmianana"  zamienia wartości B na A
zamiana<- function (kolumna_wyjatek, wartoscB, wartoscA)         
{
	x<-kolumna_wyjatek
	x[x==wartoscB]<-wartoscA 
	return (x)
}

#######################################################################################################
#Funkcja z4na3 zamienia przedziały 1,2,3,4 na 1,2,2,3
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


#######################################################################################################
#Funkcja zapisz_pplot zapisuje wykres rzutu wielowymiarowego w pliku jpeg
zapisz_pplot = function (npointszds,fname,wzorzec1,wzorzec2,co,pc,scex)
{
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=scex)
	#plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=c(2,3.5,5,6.5,8,9.5,11))
	dev.off()
}

#######################################################################################################
#Funkcja zapisz_weka zapisuje wykres drzewa uzyskanego procedurami RWeka w pliku png
zapisz_weka = function (drzewo, fname)
{
	#dotname=paste(fname,".dot",sep="")
	dotname=tempfile()
	pngname=paste(fname,".png",sep="")
	write_to_dot(drzewo,dotname)
	system(paste("dot -Tpng ",dotname," > ",pngname,sep=""))
}

#######################################################################################################
#Funkcja zapisz_rpart zapisuje wykres drzewa uzyskanego procedurami rpart w pliku jpeg
zapisz_rpart = function (drzewo, fname)
{
    jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(lwd=4)
	draw.tree(drzewo, cex=2, pch=1.0, print.levels=TRUE)
   # plot(drzewo,uniform=T,branch=0.3,compress=T,margin=0.02)
   # text(drzewo,all=T,use.n=T, fancy=T)
    dev.off()
}

#######################################################################################################
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

#######################################################################################################
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
	jpeg(file=paste(fname,".jpg",sep=""),width = 1400, height = 1000, quality = 55, bg = "white")
	par(mar=c(9,9,9,9))
	plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "black"), lab.cex = 1.75, cex.main = 0.6, cex.axis = 1.0,  lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2))
	title(main=paste("Variable Correlation Clusters ",as.character(substitute(indata)),"using",inmethod),cex.main=2)
	#par(op)
	dev.off()
}

#######################################################################################################
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

#######################################################################################################
#Funkcja meanverr oblicza średnią z list list wyników 
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


#######################################################################################################
##################################################################################################
#REGRESJA LINIOWA - redukcja parametrów i atrybutów, funkcje do eusialterlm.r
#Stosujemy kryteria: 
#BIC Bayes Information Criterion  -2maxlog-likelihood+plogn wybór mniejszych modeli
#AIC Akaike Information Cryterion -2 maxlog-likelihood + 2p
#Ridge metoda wybiera jak najmniejsze parametry Beta regresji
#dla liniowej regresji -2max loglikelihood = nlog(RSS/n)+constant 
#######################################################################################################


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
#Funkcja pred.ridge predykcja z lm.ridge wewnątrz i DataSet z etykietami: 
#podział na zbiór treninujący i testowy
pred.ridge.etykiety<-function(moutput,mparvec,DataSet,etykiety){
	mm<-apply(DataSet[etykiety,mparvec],2,mean)
	trainx <- as.matrix(sweep(DataSet[etykiety,mparvec],2,mm))
	testx <- as.matrix(sweep(DataSet[! row.names(DataSet) %in% etykiety,mparvec],2,mm))
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
#Funkcja pred.ridge predykcja po lm.ridge - jak z książki
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
# metoda LASSO redukuje wymiar i robi ridge optymalizację
# s is the constraint sum |beta| < s, dla s infinity, beta takie samo jak OLS
#######################################################################################################

#######################################################################################################
cv.lasso <- function(formula,data,K=10,subset=NULL){
	if (!is.null(subset))
		data <- data[subset,]
	y <- data[,names(data)==as.character(formula)[2]]
	x <- model.matrix(as.formula(formula),data)[,-1]
	#mycv<-cv;rm(cv)
	larsfit <- cv.lars(x,y,K=K,plot.it=FALSE)
	larsfit
}

#######################################################################################################
lasso <-	function(formula,data,subset=NULL){
	if (!is.null(subset))
		data <- data[subset,]
	
	y <- data[,names(data)==as.character(formula)[2]]
	x <- model.matrix(as.formula(formula),data)[,-1]
	larsfit <- lars(x,y,type="lasso")
	larsfit
}


#######################################################################################################
#Funkcja lars.lm wybiera kompaktowy i w miarę dobry model regresji liniowej na podstawie lars i cv.lars
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

#######################################################################################################
#Funkcja evalbn wywołuje z bnlearn funkcję nFunction=c("iabm","gc","hc","fast.iabm","inter.iabm") 
#tworzenia sieci bayesa i zwraca bn (sieć) i bnfit (prawdopodobieństwa lub regresje)  
evalbn<-function(fname,nFunction,oData){
	mcall<-match.call()
	jpeg(file=paste(fname,"_",nFunction,".jpg",sep=""),width = 1600, height = 1000, quality = 85, bg = "white")
	par(mfrow=c(1,2),pch=1.0,cex.lab=1.5,lwd=3)
	tmp<-paste(nFunction,"(",as.character(mcall$oData),")")
	bnet1 = eval(parse(text=tmp))
	tmp<-paste("pdag2dag(bnet1, ordering=c(\"",paste(names(oData),collapse='","'),"\"))",sep="")
	bnet1b = eval(parse(text=tmp))
	plot(bnet1b)
	graphviz.plot(bnet1b,shape = "ellipse")
	dev.off()
	fit = bn.fit(bnet1b, oData)
	list(bn=bnet1b,bnfit=fit)
}


#######################################################################################################
#Funkcja removenullbn usuwa puste kolumny z tablic prawdopodobieństw bnfit sieci bayesa bn
#uzyskanych metodą bnlearn za pomocą mojej funkcji evalbn
removenullbn<-function(bn,bnfit){
	for(indx in 1:length(bnfit))
	{
		bf<-bnfit[[indx]]$prob
		sdim<-dim(bnfit[[indx]]$prob)
		ndim<-dims(bnfit[[indx]]$prob)
		if(ndim-1>0)
		{
			ldg<-list()
			ishift<-1
			for(i in (1+ishift):ndim)
			{
				vi<-1:sdim[i]
				ldg[[i-ishift]]<-vi
			}
			bindex<-as.matrix(expand.grid(ldg))
			for(i in 1:nrow(bindex))
			{
				tmp<-paste("bf[,",paste(bindex[i,],collapse=","),"]",sep="")
				#cat(i," ",tmp,"\n")
				vcheck<-eval(parse(text=tmp))
				if(sum(is.na(vcheck))==sdim[1])
				{
					vcheck[which(is.na(vcheck))]<-1/sdim[1]
				}else{
					vcheck[which(is.na(vcheck))]<-0.01
				}
				tmp<-paste("bf[,",paste(bindex[i,],collapse=","),"]<-vcheck",sep="")
				#cat(tmp,"\n")
				eval(parse(text=tmp))
			}
			#bf[array(c(4,2,3,4),c(1,ndim))]
			bf->bnfit[[indx]]$prob
		}
	}
	bnfit
}

#######################################################################################################
#Funkcja bn2gr przekształca w naiwny i prosty sposób sieć bayesa uzyskaną metodą bnlearn do sieci gRain
bn2gr<-function(bn,bnfit){
	bnlearn::modelstring(bn)
	library(deal)
	library(MASTINO)
	bnlearn::node.ordering(bn)
	net = deal::network(DataSetm[, node.ordering(bn)])
	net = deal::as.network(bnlearn::modelstring(bn), net)
	#deal::modelstring(net)
	#bnlearn::modelstring(bn)
	detach(package:MASTINO)
	detach(package:deal)
	library(gRbase)
	library(gRain)
	#names(best.hc[[1]]$nodes)
	#best.hc[[1]]$nodes[[1]]$parents
	#best.hc[[1]]$nodes[[1]]$prob
	#bnfit[[]]$node,prob,parents
	#names(bn$nodes[[1]]): "mb" "nbr" "parents" "children"
	nw<-net
	lname<-c()
	lcpname<-c()
	for(i in 1:length(names(bn$nodes))){
		#i<-"L_foot_sw"
		nd <- nw$nodes[[i]]
		nname <- nd$name
		psize <- length(nd$parents)
		sl <- nd$levels
		dpar <- intersect(nd$parents,nw$discrete)
		tcol <- c()
		if(psize){
			for (j in 1:psize) {
				tcol <-c(tcol, nw$nodes[[dpar[j]]]$name)
			}
		}
		Dim <- c()                
		for (k in dpar) {
			Dim <- c(Dim, nw$nodes[[k]]$levels)
		}
		TDC <- prod(Dim)*nd$levels
		levelprob<-c()
		levelnames <- nd$levelnames;
		if(length(tcol)&&!prod(tcol==names(dimnames(bnfit[[nname]]$prob))[-1])){
			tcol1<-names(dimnames(bnfit[[nname]]$prob))[-1]
			tcolord<-c(); for(i in 1:length(tcol1)){tcolord<-c(tcolord,which(tcol==tcol1[i]))}
			tcolord<-c(1,tcolord+1)
			bnfit[[nname]]$prob=aperm(bnfit[[nname]]$prob,tcolord)
		}
		if(!psize){	
			levelprob <- bnfit[[nname]]$prob
			name<-nd$name
			tmp<-paste("cptable(~",nd$name,",values=c(",paste(levelprob,collapse=","),")",",levels=c(\"",paste(levelnames,collapse='","'),"\"))",sep="")
		}else{
			levelprob <- bnfit[[nname]]$prob[1:TDC]
			name<-paste(nd$name,".",paste(tcol,collapse="_"),sep="")
			tmp<-paste("cptable(~",nd$name,"+",paste(tcol,collapse="+"),",values=c(",paste(levelprob,collapse=","),")",",levels=c(\"",paste(levelnames,collapse='","'),"\"))",sep="")
		}
#		assign(name,eval(parse(text=tmp)))
		lname<-c(lname,name)
		lcpname<-c(lcpname,tmp)
	}
	list(lname=lname,lcpname=lcpname)
}

#######################################################################################################
#Funkcja clust_plot zapisuje cluster z etykietami do pliku
clust_plot<-function(tname,tvect,DataSetm,etykiety)
{
	fname<-paste(mypathout,nData,"_cluster_",tname,sep="")
	jpeg(file=paste(fname,".jpg",sep=""),width = 500, height = 500, quality = 35, bg = "white")
	plot(cmdscale(dist(DataSetm)),pch=c(18,19,17)[etykiety],col=c("yellow","black","green","red")[tvect],cex=3,main=tname)
	dev.off()
}

#######################################################################################################
#Funkcja clust_multiplot wypisuje kombinacje klaster, a zdyskretyzowane wartości 
clust_multiplot<-function(tmeth,DataSetd,DataSetm,etykiety)
{
	for(i in 1:length(names(DataSetd)))
	{
		tname<-names(DataSetd)[i]
		tname<-paste(tmeth,"_",tname,sep="")
		#print(tname)
		clust_plot(tname,DataSetd[,i],DataSetm,etykiety)
	}
}

#######################################################################################################
#Funkcja binplot.3d
binplot.3d<-function(x,y,z,alpha=1,topcol="#ff0000",sidecol="#aaaaaa")
{
	save <- par3d(skipRedraw=TRUE)
	on.exit(par3d(save))
	
	x1<-c(rep(c(x[1],x[2],x[2],x[1]),3),rep(x[1],4),rep(x[2],4))
	z1<-c(rep(0,4),rep(c(0,0,z,z),4))
	y1<-c(y[1],y[1],y[2],y[2],rep(y[1],4),rep(y[2],4),rep(c(y[1],y[2],y[2],y[1]),2))
	x2<-c(rep(c(x[1],x[1],x[2],x[2]),2),rep(c(x[1],x[2],rep(x[1],3),rep(x[2],3)),2))
	z2<-c(rep(c(0,z),4),rep(0,8),rep(z,8) )
	y2<-c(rep(y[1],4),rep(y[2],4),rep(c(rep(y[1],3),rep(y[2],3),y[1],y[2]),2) )
	rgl.quads(x1,z1,y1,col=rep(sidecol,each=4),alpha=alpha)
	rgl.quads(c(x[1],x[2],x[2],x[1]),rep(z,4),c(y[1],y[1],y[2],y[2]),col=rep(topcol,each=4),alpha=1) 
	rgl.lines(x2,z2,y2,col="#000000")
}

#######################################################################################################
#Funkcja hist3d
hist3d<-function(x,y=NULL,nclass="auto",alpha=1,col="#ff0000",scale=10)
{
	save <- par3d(skipRedraw=TRUE)
	on.exit(par3d(save))
	xy <- xy.coords(x,y)
	x <- xy$x
	y <- xy$y
	n<-length(x)
	if (nclass == "auto") { nclass<-ceiling(sqrt(nclass.Sturges(x))) }
	breaks.x <- seq(min(x),max(x),length=(nclass+1))
	breaks.y <- seq(min(y),max(y),length=(nclass+1))
	z<-matrix(0,(nclass),(nclass))
	for (i in 1:nclass) 
	{
		for (j in 1:nclass) 
		{
			z[i, j] <- (1/n)*sum(x < breaks.x[i+1] & y < breaks.y[j+1] & 
							x >= breaks.x[i] & y >= breaks.y[j])
			binplot.3d(c(breaks.x[i],breaks.x[i+1]),c(breaks.y[j],breaks.y[j+1]),
					scale*z[i,j],alpha=alpha,topcol=col)
		}
	}
}


#######################################################################################################
#Funkcja zapisz_cloud zapisuje wykres ISOMDS z dwóch wzorców w pliku jpeg
zapisz_cloud <- function (nDataSetds, fname, wzorzec1, wzorzec2, co, pc, scex)
{
	jpeg(file=paste(mypathout,fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	#plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=scex)
	#co = c('black','green','red','yellow')
	#pc = c("*","+",".","o")
	#scex = 2.5
	#wzorzec1 = DataSetd[,18]
	#wzorzec2 = ifelse(DataSetd$normal_develop==0,1,ifelse(DataSetd$AED_off==1,2,3))
	cat(paste(mypathout,fname,".jpg",sep="")," \n")
	p<-cloud(wzorzec2 ~ nDataSetds$points[,1] + nDataSetds$points[,2],
			type="p", col=co[wzorzec2], pch=pc[wzorzec1], cex = scex, zoom = 1,
			xlab=NULL, ylab = NULL, zlab = NULL,
			par.settings = list(axis.line = list(col = "transparent")),
			scales = list (draw = FALSE),perspective=TRUE, screen=list(z=0,x=0,y=0))
	xpanel=2;ypanel=2
	rotx<-c(0,0,0,0)
	roty<-c(0,70,60,120)
	rotz<-c(0,0,60,60)
	plot(update(p[rep(1,2*xpanel)],layout=c(ypanel,xpanel),
					panel = function(...,screen){
						crow<-current.row();ccol<-current.column();
						panel.cloud(...,distance=0.145, screen = list(z=rotz[ypanel*(ypanel-crow)+ccol],x=rotx[ypanel*(ypanel-crow)+ccol],y=roty[ypanel*(ypanel-crow)+ccol]))
					}))
	
#   cloud(nDataSetds$points[,1] ~ nDataSetds$points[,2] + nDataSetds$points[,3],type="p",col=co[wzorzec_kol2],
#	pch = pc[wzorzec1_pun2],cex = scex, zoom = 1, xlab=NULL, ylab = NULL, zlab = NULL,
#	par.settings = list(axis.line = list(col = "transparent")),	scales = list (draw = FALSE))
	dev.off()
}



#######################################################################################################
#Funkcja zapisz_3dcast
zapisz_3dcast <- function (nDataSets, fname, DataSetd, nwzorzec1, nwzorzec2)
{
	rgl.open()
	open3d()
	par3d(windowRect=c(1,1,1000,1000))
	x<-nDataSets$points[,1]
	y<-nDataSets$points[,2]
	wzorzec1<-DataSetd[,which(names(DataSetd)%in%nwzorzec1)]
	wzorzec2<-DataSetd[,which(names(DataSetd)%in%nwzorzec2)]
	jpeg(file=paste(fname,"_0.jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	plot(table(wzorzec2,wzorzec1))
	dev.off()
	wzorzec1s = as.numeric(as.vector(wzorzec1)) * 0.5
	z<-wzorzec2
	co = c('black','green','red','yellow','blue')
	rgl.viewpoint(40,25,60,1)
	plot3d(x,y,z,col=co[wzorzec2],type="s",size=wzorzec1s)
	fname1<-paste(fname,"_2.jpg",sep="")
	rgl.snapshot(fname1)
	rgl.viewpoint(0,0,60,1)
	plot3d(x,y,1,col=co[wzorzec2],type="s",size=wzorzec1s)
	fname1<-paste(fname,"_1.jpg",sep="")
	rgl.snapshot(fname1)
	co = c('black','green','red','yellow','blue')
	pc = c(".","-","+","*","o")
	fname1<-paste(fname,"_3",sep="")
	zapisz_cloud(nDataSets,fname1,wzorzec1,wzorzec2,co,pc,2.5)
}

#######################################################################################################
#Funkcja lessone
lessone<-function(x){ifelse(x>1,x<-1/x,ifelse(x<(-1),x<-1/x,x<-x))}

#######################################################################################################
#Funkcja lesstwo
lesstwo<-function(DataSetm,x,y){
	parvec<-names(DataSetm)
	#z<-0;
	#for(i in 1:length(x)){
	#	if(abs(x)>abs(y)){z<-x;x<-y;y<-z}
	#	if((x[i]<0 & y[i]>0)|(x[i]>0 & y[i]<0)) {x[i]<-abs(x[i]); y[i]<-abs(y[i])+2*abs(x[i]); }
	#	if(x[i]<0 & y[i]<0) {x[i]<-abs(x[i]); y[i]<-abs(y[i])}
	#}
	#sapply(x/y,lessone)
	#sapply(sd(DataSetm)/abs(y-x),lessone)
	sd(DataSetm)/abs(y-x)
}



#######################################################################################################
#Funkcja clusterfun 
clusterfun<-function(DataSetm,etykiety,nFunFrame){
	parvec<-names(DataSetm)
	ltmp<-list()
	for(j in parvec){
		tmp<-c()
		datatmp<-c()
		for(i in sort(unique(etykiety))){
			datatmp<-DataSetm[etykiety==i,c(j)]
			tmp<-c(tmp,eval(parse(text=paste(nFunFrame,"(datatmp)",sep=""))))
		}
		ltmp[[j]]<-tmp
	}
	ltmp<-t(as.data.frame.list(ltmp))
	#tmp<-sapply(ltmp[,1]/ltmp[,2],lessone)
	tmp<-lesstwo(DataSetm,ltmp[,1],ltmp[,2])
	#for(i in 1:length(tmp)) ifelse(tmp[i]>1,tmp[i]<-1/tmp[i],tmp[i]<-tmp[i])
	if(length(unique(etykiety))>=3){
		for(i in 3:length(unique(etykiety))){
			#tmp1<-sapply(ltmp[,2]/ltmp[,3],lessone)
			tmp1<-lesstwo(DataSetm,ltmp[,i-1],ltmp[,i])
			#tmp<-sapply((tmp+tmp1)/2,lessone)
			tmp<-(tmp+tmp1)/2
		}
		tmp1<-lesstwo(DataSetm,ltmp[,i],ltmp[,1])
		#tmp<-sapply((tmp+tmp1)/2,lessone)
		tmp<-(tmp+tmp1)/2
	}
	ltmp<-merge(ltmp,tmp,by=0)
	ltmp<-ltmp[order(ltmp[,length(unique(etykiety))+2]),]
}

#######################################################################################################
#Funkcja sexcount 
sexcount<-function(DataSetd,etykiety){
	DataSet<-cbind(DataSetd,etykiety)
	ltmp<-list()
	for(j in sort(unique(etykiety))){
		tmp<-c()
		datatmp<-DataSet[DataSet[,ncol(DataSet)]==j,c("sex")]
		for(i in sort(unique(DataSet$sex))){
			tmp<-c(tmp,length(datatmp[datatmp==i]))
		}
		ltmp[[j]]<-tmp
	}
	ltmp<-as.data.frame.list(ltmp)
}
	
#######################################################################################################
#Funkcja meanvar 
meanvar<-function(x) c(sum=sum(x),mean=mean(x),var=var(x),n=length(x))

#######################################################################################################
#Funkcja clusterfunext
clusterfunext<-function(DataSetm,etykiety,nFunFrame){
	ltmp<-clusterfun(DataSetm,etykiety,nFunFrame);etykiety<-factor(etykiety)
	DataSetk<-cbind(DataSetm,etykiety)
	pvans<-c()
	for(i in 1:length(ltmp[,1])){
		fit<-lm(DataSetk[[ltmp[i,1]]]~DataSetk$etykiety, DataSetk)
		pvans<-c(pvans,as.matrix(anova(fit)[5])[1,1])
	}
	ltmp<-cbind(ltmp,pvans)
	sdv<-c()
	for(i in 1:length(ltmp[,1])){
		sdv<-c(sdv,sd(DataSetk[[ltmp[i,1]]]))
	}
	ltmp<-cbind(ltmp,sdv)
	ltmp
}

#######################################################################################################
#Funkcja multicluster
multicluster<-function(DataSetm,DataSetd,ploton,clname,ntimes,nFunFrame){
	klaster <- diana(DataSetm, metric = "manhattan", stand = TRUE)
	etykiety <- cutree(as.hclust(klaster), k = ntimes)
	tmeth <- paste(clname,"_diana",ntimes,sep="")
	if(ploton) clust_multiplot(tmeth,DataSetd,DataSetm,etykiety)
	print(tmeth);
	ltmp<-sexcount(DataSetd,etykiety); print(ltmp)
	ltmp<-clusterfunext(DataSetm,etykiety,nFunFrame);
	print(format(ltmp[ltmp[length(ltmp)-1]<=0.05,], digits = 3, nsmall = 3))
	print(format(ltmp[ltmp[length(ltmp)-1]>0.05,], digits = 3, nsmall = 3))
	
	#metric = c("euclidean", "manhattan", "SqEuclidean")
	klaster <- fanny(dist(DataSetm)^2, ntimes)
	etykiety<-klaster$clustering
	tmeth <- paste(clname,"_fanny",ntimes,sep="")
	if(ploton) clust_multiplot(tmeth,DataSetd,DataSetm,etykiety)
	print(tmeth);
	ltmp<-sexcount(DataSetd,etykiety); print(ltmp)
	ltmp<-clusterfunext(DataSetm,etykiety,nFunFrame);
	print(format(ltmp[ltmp[length(ltmp)-1]<=0.05,], digits = 3, nsmall = 3))
	print(format(ltmp[ltmp[length(ltmp)-1]>0.05,], digits = 3, nsmall = 3))
	
	klaster <- hclust(dist(DataSetm)^2, "cen")
	etykiety<- cutree(klaster, k = ntimes)
	tmeth <- paste(clname,"_hclus",ntimes,sep="")
	if(ploton) clust_multiplot(tmeth,DataSetd,DataSetm,etykiety)
	print(tmeth);
	ltmp<-sexcount(DataSetd,etykiety); print(ltmp)
	ltmp<-clusterfunext(DataSetm,etykiety,nFunFrame);
	print(format(ltmp[ltmp[length(ltmp)-1]<=0.05,], digits = 3, nsmall = 3))
	print(format(ltmp[ltmp[length(ltmp)-1]>0.05,], digits = 3, nsmall = 3))
	
	klaster<-agnes(dist(DataSetm,method="manhattan"),method="complete");
	etykiety<-cutree(klaster,k=ntimes)
	tmeth <- paste(clname,"_agnes",ntimes,sep="")
	if(ploton) clust_multiplot(tmeth,DataSetd,DataSetm,etykiety)
	print(tmeth);
	ltmp<-sexcount(DataSetd,etykiety); print(ltmp)
	ltmp<-clusterfunext(DataSetm,etykiety,nFunFrame);
	print(format(ltmp[ltmp[length(ltmp)-1]<=0.05,], digits = 3, nsmall = 3))
	print(format(ltmp[ltmp[length(ltmp)-1]>0.05,], digits = 3, nsmall = 3))
	
	klaster<-kmeans(DataSetm,ntimes,nstart=100)
	etykiety<-klaster$cluster
	tmeth <- paste(clname,"_kmean",ntimes,sep="")
	if(ploton) clust_multiplot(tmeth,DataSetd,DataSetm,klaster$cluster)
	print(tmeth);
	ltmp<-sexcount(DataSetd,etykiety); print(ltmp)
	ltmp<-clusterfunext(DataSetm,etykiety,nFunFrame);
	print(format(ltmp[ltmp[length(ltmp)-1]<=0.05,], digits = 3, nsmall = 3))
	print(format(ltmp[ltmp[length(ltmp)-1]>0.05,], digits = 3, nsmall = 3))
}


#######################################################################################################
#Funkcja joinYN
joinYN<-function(DataSet,parvecforout){
	DataVec<-c(); 
	for(j in 1:nrow(DataSet)) 
	{
		y_one<-1
		for( i in parvecforout){ 
			if(!is.na(DataSet[j,i])){
				if(DataSet[j,i]=="Y"){ 
					DataVec[j]<-"Y"; 
					y_one<-0
					#cat("Y(",j," ",i,")")
				}else
				if(y_one==1)
					DataVec[j]<-"N"
			}else{
				if(y_one==1)
					DataVec[j]<-"N"
			}
		}
	}
	factor(DataVec)
}
