# skrypt do zajêæ ISO WIT: Kruskal, lm, rpart
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

#te ¶cie¿ki mypath i mypathout musz± istnieæ w systemie
#to ¶cie¿ka do plików wykonywalnych z R pow³oki za pomoc± source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"
#to ¶cie¿ka do plików graficznych uzyskiwanych za pomoc± funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")
dir.create(mypathout, showWarnings = TRUE, recursive = TRUE, mode = "0755")
#Sys.chmod(paths, mode = "0755")

source(paste(mypath,"isowitfunkcje.r",sep=""))

##########################################################################################################
#nData<-"Glass"
#nData<-"nihills"
#nData<-"photocar"
source(paste(mypath,"isowitdatasets.r",sep=""))

##########################################################################################################
#Kruskal dla normalnych danych
#generujê z daisy ró¿nice miêdzy wierszami podanego zbioru i wprowadzam do isoMDS rzutuj±cego na dwa wymiary k=2 
parveckruskal=setdiff(names(DataSet),parnokruskal)
nDataSets<-KruskelMDS(DataSet, parveckruskal, 2)

# uzyskany zbiór punktów kolorujê ró¿nymi atrybutami podstawianymi do wzorzec i uzyskujê wizualizacjê cech
fname<-paste(mypathout,nData,"_MDSnorm_nrmdis",sep="")
wzorzec1=DataSet$Type
#pdm=1:length(unique(DataSet$Type))
#org=sort(unique(DataSet$Type))
#wzorzec1=2
plotMDS.for.chosen(fname, nDataSets, DataSetd, parvecol, wzorzec1) 

#Kruskal dla zeskorowanych danych
parveckruskal=setdiff(names(DataSet),parnokruskal)
nDataSets<-KruskelMDS(DataSetz, parveckruskal, 2)

fname<-paste(mypathout,nData,"_MDSzesc_zscdis",sep="")
wzorzec1=DataSet$Type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvecol, wzorzec1) 


##########################################################################################################
#generujemy drzewa rpart dla parvectree
nFunction<-"rpart"
evalstr<-"DEFAULT"
jmax<-30
verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
parvectree=setdiff(names(DataSet),parvecnotree)
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
besti<-bestli[length(bestli)]
combi<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)



##########################################################################################################
#generujemy drzewa J48 dla parvectree
nFunction<-"J48"
evalstr<-"DEFAULT"
jmax<-10
verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
parvectree=setdiff(names(DataSet),parvecnotree)
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
besti<-bestli[length(bestli)]
combi<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)

##########################################################################################################
#generujemy maszyny svm dla parvectree
nFunction<-"svm"
#evalstr<-'kernel="linear"'
#evalstr<-'kernel="sigmoid"'
#evalstr<-'kernel="polynomial"'
evalstr<-'kernel="radial"'
jmax<-10
verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
parvectree=setdiff(names(DataSet),parvecnotree)
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
besti<-bestli[length(bestli)]
combi<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)


##########################################################################################################
#generujemy klasyfikatory ipredknn dla parvectree
nFunction<-"ipredknn"
evalstr<-"DEFAULT"
jmax<-10
verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
parvectree=setdiff(names(DataSet),parvecnotree)
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
besti<-bestli[length(bestli)]
combi<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)


##########################################################################################################
#generujemy klasyfikatory lda dla parvectree
nFunction<-"lda"
evalstr<-"CLASS"
jmax<-10
verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
parvectree=setdiff(names(DataSet),parvecnotree)
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
besti<-bestli[length(bestli)]
combi<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)

##########################################################################################################
#NaiveBayes,qda
##########################################################################################################
#generujemy klasyfikatory lda dla parvectree
nFunction<-"NaiveBayes"
evalstr<-"CLASS"
#jmax<-10
#verr<-c();vmod<-list();lverr<-list(); lvmod<-list(); jdiv<-jmax;
#parvectree=setdiff(names(DataSet),parvecnotree)
#for(j in 1:jmax){
#	verr<-c();vmod<-list();
#	for(i in 1:4){
#		etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.9))
#		if(i==1) {mDataSet<-DataSet[etykiety,]; mDataSetn<-DataSet[-etykiety,];datype<-"norm"}
#		else if(i==2) {mDataSet<-DataSetz[etykiety,]; mDataSetn<-DataSetz[-etykiety,];datype<-"zesc"}
#		else if(i==3) {mDataSet<-DataSetd[etykiety,]; mDataSetn<-DataSetd[-etykiety,];datype<-"nrdi"}
#		else if(i==4) {mDataSet<-DataSetzd[etykiety,]; mDataSetn<-DataSetzd[-etykiety,];datype<-"zedi"}
#		classifier<-try(evalwithattr(nFunction,paroutputree,parvectree,mDataSet,evalstr),TRUE)
#		if(!inherits(classifier, "try-error")){
#			lres<-prederror(classifier,paroutputree,parvectree,mDataSetn,evalstr)
#			vmod[[i]]<-classifier;verr<-c(verr,lres$perror);
#		}
#		else jdiv<-jdiv-1;
#	}
#	lverr[[j]]<-verr; lvmod[[j]]<-vmod
#}
#verr<-meanverr(lverr,jdiv,4); n<-min(which(verr==min(verr)))
n=1;#Bayes nie chce dla innych danych dzia³aæ ni¿ normalne
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
besti<-bestli[length(bestli)]
combi<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)
