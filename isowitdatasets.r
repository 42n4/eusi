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
nData<-"photocar"

##########################################################################################################
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,"meatDataEN.csv",sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))
#library(mlbench)
data(list=nData)
DataSet<-get(nData)

##########################################################################################################
if(nData=="Glass"){
	#operacje kosmetyczne poprawiaj±ce dane, przystosowuj±ce
	#usuwanie takich samych wierszy ze zbioru Glass, a dok³adnie jednego z nich
	#(jak siê pojawi± dwa takie same wiersze to wyrzuca b³±d isoMDS, 
	# dlatego na samym pocz±tku usuwane s± niektóre wiersze z Glass)
	DataSet<-DataSet[-c(40),]
	#te atrybuty, co s± faktorami, zmiennymi jako¶ciowymi z kategoriami, etykietami
	parvecfactor=c("Type")
	#w parnokruskal to co zostanie nie wys³ane do isoMDS(daisy) np. parnokruskal=c("Type")
	parnokruskal=c()
	#atrybut pojedyñczy do zescorowania po jego dyskretnych warto¶ciach
	vecfactorzesc=c("Type")
	#atrybuty w plotMDS do kolorowania powsta³ych punktów 
	parvecol<-names(DataSet)
	#zmienne, które nie wchodz± do drzewa m.in. jego li¶cie, target, cel optymalizacji drzewa
	parvecnotree=c("Type")
	#li¶æ drzewa, etykieta
	paroutputree=c("Type")
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("Type","RI")
	#wybieramy zmienn± zale¿n±, target dla regresji, zwykle zmiennoprzecinkowy
	moutput<-"RI"
}

##########################################################################################################

if(nData=="nihills"){
	#te atrybuty, co s± faktorami, zmiennymi jako¶ciowymi z kategoriami, etykietami
	parvecfactor=c()
	#w parnokruskal to co zostanie nie wys³ane do isoMDS(daisy) np. parnokruskal=c("Type")
	parnokruskal=c()
	#atrybut pojedyñczy do zescorowania po jego dyskretnych warto¶ciach
	#vecfactorzesc=c()
	#atrybuty w plotMDS do kolorowania powsta³ych punktów 
	parvecol<-names(DataSet)
	#zmienne, które nie wchodz± do drzewa m.in. jego li¶cie, target, cel optymalizacji drzewa
	parvecnotree=c('time')
	#li¶æ drzewa, etykieta
	paroutputree=c('time')
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("time")
	#wybieramy zmienn± zale¿n±, target dla regresji, zwykle zmiennoprzecinkowy
	moutput<-"time"
} 

##########################################################################################################
if(nData=="photocar"){
	#usuwanie do Kruskala
	DataSet<-DataSet[-c(13,25,32,33,34,36,37,67,64,77,52),]
 	#te atrybuty, co s± faktorami, zmiennymi jako¶ciowymi z kategoriami, etykietami
	parvecfactor=c("group","event","tumor")
	#atrybut pojedyñczy do zescorowania po jego dyskretnych warto¶ciach
	vecfactorzesc=c("event")
	#w parnokruskal to co zostanie nie wys³ane do isoMDS(daisy) np. parnokruskal=c("Type")
	parnokruskal=c()
	#atrybuty w plotMDS do kolorowania powsta³ych punktów 
	parvecol<-names(DataSet)
	#zmienne, które nie wchodz± do drzewa m.in. jego li¶cie, target, cel optymalizacji drzewa
	parvecnotree=c("group")
	#li¶æ drzewa, etykieta
	paroutputree=c("group")
	#zmienne zale¿ne i inne zbêdne w regresji
	#parvecnolm=c("time","group","tumor","event")
	parvecnolm=c("time")
	#wybieramy zmienn± zale¿n±, target dla regresji, zwykle zmiennoprzecinkowy
	moutput<-"time"
}

##########################################################################################################
#te atrybuty, które s± zmiennymi ilo¶ciowymi
parvec=setdiff(names(DataSet),parvecfactor)

#potem faktoryzujê na wszelki wypadek kolumny, które s± etykietami, maj± ju¿ zdyskretyzowane warto¶ci; ich wybór jest w wektorze parvecfactor
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))

#najpierw defaktoryzujê i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi, tak na wszelki wypadek, gdyby csv ¼le siê wczyta³ (w przypadku zbiorów data() to tylko æwiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0 i sd=1
DataSetms<-scale_for(DataSet,parvec,TRUE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do sd=1
DataSets<-scale_for(DataSet,parvec,FALSE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0
DataSetm<-scale_for(DataSet,parvec,TRUE,FALSE)

#zetskorujê wybrane kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,vecfactorzesc)

#dyskretyzujê kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzujê tak¿e nie zeskorowane warto¶ci zwyk³ego zbioru wczytanego na pocz±tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


##########################################################################################################
#korelacje miêdzy atrybutami
#for(cormethod in c("pearson","kendall","spearman")){
for(cormethod in c("pearson")){
	hier2jpg(cormethod,DataSet[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_norm",sep=""))
	hier2jpg(cormethod,DataSetz[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_zesc",sep=""))
}
try(latt2jpg(DataSet[,parvec],DataSetd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_norm",sep="")),TRUE)
try(latt2jpg(DataSetz[,parvec],DataSetzd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_zesc",sep="")),TRUE)
#pairs


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
verr<-c();vmod<-list();lverr<-list(); lvmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
for(j in 1:jmax){
	verr<-c();vmod<-list();
	for(i in 1:4){
		etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.8))
		if(i==1) {mDataSet<-DataSet[etykiety,]; mDataSetn<-DataSet[-etykiety,];datype<-"norm"}
		else if(i==2) {mDataSet<-DataSetz[etykiety,]; mDataSetn<-DataSetz[-etykiety,];datype<-"zesc"}
		else if(i==3) {mDataSet<-DataSetd[etykiety,]; mDataSetn<-DataSetd[-etykiety,];datype<-"nrdi"}
		else if(i==4) {mDataSet<-DataSetzd[etykiety,]; mDataSetn<-DataSetzd[-etykiety,];datype<-"zedi"}
		classifier<-evalwithattr(nFunction,paroutputree,parvectree,mDataSet,evalstr)
		lres<-prederror(classifier,paroutputree,parvectree,mDataSetn,evalstr)
		vmod[[i]]<-classifier;verr<-c(verr,lres$perror)
		if(nFunction=="rpart") zapisz_rpart(vmod[[i]],paste(mypathout,nData,"_",nFunction,"_",datype,sep=""))
		if(nFunction=="J48") zapisz_weka(vmod[[i]],paste(mypathout,nData,"_wkJ48_nrm",sep=""))
	}
	lverr[[j]]<-verr; lvmod[[j]]<-vmod
}
verr<-meanverr(lverr,jmax,4); n<-min(which(verr==min(verr)))
if(n==1){mbDataSet<-DataSet;datype<-"norm";}
if(n==2){mbDataSet<-DataSetz;datype<-"zesc";}
if(n==3){mbDataSet<-DataSetd;datype<-"nrdi";}
if(n==4){mbDataSet<-DataSetzd;datype<-"zedi";}
#generujemy nFunction automatycznie dla parvectree i ró¿nej liczby atrybutów dla najlepiej pasuj±cych danych
pred_norm<-permbesteval(nFunction, paste(mypathout,nData,"_pred_",nFunction,"_",datype,sep=""),mbDataSet, paroutputree,parvectree,80,5,evalstr)
y<-seq(1,length(pred_norm));y<-sapply(y,function(x){if(x %% 5) x<-0 else x<-x});y<-y[!y==0];
z<-c();for(i in y) z<-c(z,pred_norm[[i-1]]$perror); n<-min(which(z==min(z)))
bestclass<-pred_norm[[(n-1)*5+3]]
betykiety<-pred_norm[[(n-1)*5+1]]
bestli<-pred_norm[[(n-1)*5+2]]
bestn<-pred_norm[[(n-1)*5+5]]
besti<-bestli[length(bestli)]
perm<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
lres$table
lres$perror
min(z)

##########################################################################################################
#generujemy drzewa J48 dla parvectree
nFunction<-"J48"
evalstr<-"DEFAULT"
jmax<-10
verr<-c();vmod<-list();lverr<-list(); lvmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
for(j in 1:jmax){
	verr<-c();vmod<-list();
	for(i in 1:4){
		etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.8))
		if(i==1) {mDataSet<-DataSet[etykiety,]; mDataSetn<-DataSet[-etykiety,];datype<-"norm"}
		else if(i==2) {mDataSet<-DataSetz[etykiety,]; mDataSetn<-DataSetz[-etykiety,];datype<-"zesc"}
		else if(i==3) {mDataSet<-DataSetd[etykiety,]; mDataSetn<-DataSetd[-etykiety,];datype<-"nrdi"}
		else if(i==4) {mDataSet<-DataSetzd[etykiety,]; mDataSetn<-DataSetzd[-etykiety,];datype<-"zedi"}
		classifier<-evalwithattr(nFunction,paroutputree,parvectree,mDataSet,evalstr)
		lres<-prederror(classifier,paroutputree,parvectree,mDataSetn,evalstr)
		vmod[[i]]<-classifier;verr<-c(verr,lres$perror)
	}
	lverr[[j]]<-verr; lvmod[[j]]<-vmod
}
verr<-meanverr(lverr,jmax,4); n<-min(which(verr==min(verr)))
if(n==1){mbDataSet<-DataSet;datype<-"norm";}
if(n==2){mbDataSet<-DataSetz;datype<-"zesc";}
if(n==3){mbDataSet<-DataSetd;datype<-"nrdi";}
if(n==4){mbDataSet<-DataSetzd;datype<-"zedi";}
#generujemy nFunction automatycznie dla parvectree i ró¿nej liczby atrybutów dla najlepiej pasuj±cych danych
pred_norm<-permbesteval(nFunction, paste(mypathout,nData,"_pred_",nFunction,"_",datype,sep=""),mbDataSet, paroutputree,parvectree,80,5,evalstr)
y<-seq(1,length(pred_norm));y<-sapply(y,function(x){if(x %% 5) x<-0 else x<-x});y<-y[!y==0];
z<-c();for(i in y) z<-c(z,pred_norm[[i-1]]$perror); n<-min(which(z==min(z)))
bestclass<-pred_norm[[(n-1)*5+3]]
betykiety<-pred_norm[[(n-1)*5+1]]
bestli<-pred_norm[[(n-1)*5+2]]
bestn<-pred_norm[[(n-1)*5+5]]
besti<-bestli[length(bestli)]
perm<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
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
verr<-c();vmod<-list();lverr<-list(); lvmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
for(j in 1:jmax){
	verr<-c();vmod<-list();
	for(i in 1:4){
		etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.8))
		if(i==1) {mDataSet<-DataSet[etykiety,]; mDataSetn<-DataSet[-etykiety,];datype<-"norm"}
		else if(i==2) {mDataSet<-DataSetz[etykiety,]; mDataSetn<-DataSetz[-etykiety,];datype<-"zesc"}
		else if(i==3) {mDataSet<-DataSetd[etykiety,]; mDataSetn<-DataSetd[-etykiety,];datype<-"nrdi"}
		else if(i==4) {mDataSet<-DataSetzd[etykiety,]; mDataSetn<-DataSetzd[-etykiety,];datype<-"zedi"}
		classifier<-evalwithattr(nFunction,paroutputree,parvectree,mDataSet,evalstr)
		lres<-try(prederror(classifier,paroutputree,parvectree,mDataSetn,evalstr),TRUE)
		if(!inherits(lres, "try-error"))
			vmod[[i]]<-classifier;verr<-c(verr,lres$perror)
	}
	lverr[[j]]<-verr; lvmod[[j]]<-vmod
}
verr<-meanverr(lverr,jmax,4); n<-min(which(verr==min(verr)))
if(n==1){mbDataSet<-DataSet;datype<-"norm";}
if(n==2){mbDataSet<-DataSetz;datype<-"zesc";}
if(n==3){mbDataSet<-DataSetd;datype<-"nrdi";}
if(n==4){mbDataSet<-DataSetzd;datype<-"zedi";}
#generujemy nFunction automatycznie dla parvectree i ró¿nej liczby atrybutów dla najlepiej pasuj±cych danych
pred_norm<-permbesteval(nFunction, paste(mypathout,nData,"_pred_",nFunction,"_",datype,sep=""),mbDataSet, paroutputree,parvectree,80,5,evalstr)
y<-seq(1,length(pred_norm));y<-sapply(y,function(x){if(x %% 5) x<-0 else x<-x});y<-y[!y==0];
z<-c();for(i in y) z<-c(z,pred_norm[[i-1]]$perror); n<-min(which(z==min(z)))
bestclass<-pred_norm[[(n-1)*5+3]]
betykiety<-pred_norm[[(n-1)*5+1]]
bestli<-pred_norm[[(n-1)*5+2]]
bestn<-pred_norm[[(n-1)*5+5]]
besti<-bestli[length(bestli)]
perm<-get("combinations","package:gtools")(length(parvectree),bestn,parvectree)
lres<-prederror(bestclass, paroutputree, parvectree, mbDataSet[-betykiety,],evalstr)
cat("Best ",nFunction," for variable number=",bestn)
bestclass
if(nFunction=="rpart") zapisz_rpart(bestclass,paste(mypathout,nData,"_Be",nFunction,"_",datype,sep=""))
if(nFunction=="J48") zapisz_weka(bestclass,paste(mypathout,nData,"_Best",nFunction,"_",datype,sep=""))
lres$table
lres$perror
min(z)


##########################################################################################################
#ipredknn
verr<-c();vmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
mDataSet<-DataSet[etykiety,]
classifier <- evalwithattr("ipredknn",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,])
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetz[etykiety,]
classifier <- evalwithattr("ipredknn",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,])
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetd[etykiety,]
classifier <- evalwithattr("ipredknn",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,])
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetzd[etykiety,]
classifier <- evalwithattr("ipredknn",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,])
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
verr

##########################################################################################################
#lda
verr<-c();vmod<-list()
evalstr<-"CLASS"
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
mDataSet<-DataSet[etykiety,]
classifier <- evalwithattr("lda",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,],evalstr)
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetz[etykiety,]
classifier <- evalwithattr("lda",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,],evalstr)
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetd[etykiety,]
classifier <- evalwithattr("lda",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,],evalstr)
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetzd[etykiety,]
classifier <- evalwithattr("lda",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,],evalstr)
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
verr

##########################################################################################################
#NaiveBayes,qda
##########################################################################################################
evalstr<-"CLASS"
verr<-c();vmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
mDataSet<-DataSet[etykiety,]
classifier <- evalwithattr("NaiveBayes",paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,],evalstr)
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
#mDataSet<-DataSetz[etykiety,]
#classifier <- evalwithattr("NaiveBayes",paroutputree,parvectree,mDataSet)
#lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,])
#vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
#mDataSet<-DataSetd[etykiety,]
#classifier <- evalwithattr("NaiveBayes",paroutputree,parvectree,mDataSet)
#lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,])
#vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
#mDataSet<-DataSetzd[etykiety,]
#classifier <- evalwithattr("NaiveBayes",paroutputree,parvectree,mDataSet)
#lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,])
#vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
verr

##########################################################################################################
#REGRESJA LINIOWA
nFunction="lm"
#w mparvec zmienne niezale¿ne do regresji
mparvec=setdiff(names(DataSet),parvecnolm)

##########################################################################################################
#alpha to krytyczna warto¶æ prawdopodobieñstwa p dla statystyk bp i f 
alpha<-0.1;
##########################################################################################################
#dla nleven > 0 le vena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#bptest
nleven<--1; 
#wybieramy dane normalne
mDataSet<-DataSet
lmabc_nobp<-permbesteval("lm", paste(mypathout,nData,"_lmabc_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskorowane
mDataSet<-DataSetz
lmabc_zebp<-permbesteval("lm", paste(mypathout,nData,"_lmabc_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskalowane i zcentrowane
#mDataSet<-DataSetm
#skalowanie likwiduje intercept wspó³czynnik przesuniêcia prostej
#lmabc_msbp<-permbesteval("lm", paste(mypathout,nData,"_lmabc_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-0; 
mDataSet<-DataSet
lmabc_nono<-permbesteval("lm", paste(mypathout,nData,"_lmabc_nono",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetz
lmabc_zeno<-permbesteval("lm", paste(mypathout,nData,"_lmabc_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#mDataSet<-DataSetm
#lmabc_msno<-permbesteval("lm", paste(mypathout,nData,"_lmabc_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-5; 
mDataSet<-DataSet
lmabc_nolt<-permbesteval("lm", paste(mypathout,nData,"_lmabc_nolt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetz
lmabc_zelt<-permbesteval("lm", paste(mypathout,nData,"_lmabc_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#mDataSet<-DataSetm
#lmabc_mslt<-permbesteval("lm", paste(mypathout,nData,"_lmabc_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#Teraz u¿yjemy splinów czyli y~bs(x1)+bs(x2)+...
SPLINE<-"SPLINE";
##########################################################################################################
#bptest
nleven<--1; 
mDataSet<-DataSet
lmbsp_nobp<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
lmbsp_zebp<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_msbp<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-0; 
mDataSet<-DataSet
lmbsp_nono<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_nono",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
lmbsp_zeno<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_msno<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#leven
nleven<-5; 
mDataSet<-DataSet
lmbsp_nolt<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_nolt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
lmbsp_zelt<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_mslt<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#REGRESJA LOGISTYCZNA
nFunction="polr"
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.9))
parvectree=setdiff(names(DataSet),parvecnotree)
evalstr<-"Hess=TRUE"
mDataSet<-DataSetd[etykiety,]
polra_nodi<-permbesteval("polr", paste(mypathout,nData,"_polra_nodi",sep=""),mDataSet, paroutputree, parvectree, nleven, alpha, evalstr)
mDataSet<-DataSetzd[etykiety,]
polra_zedi<-permbesteval("polr", paste(mypathout,nData,"_polra_zedi",sep=""),mDataSet, paroutputree, parvectree, nleven, alpha, evalstr)

#prederror(regression,paroutputree,mparvec,DataSetzd[-etykiety,],evalstr)
#nFunction<-"polr"; fname<-paste(mypathout,nData,"_polra_nodi",sep=""); moutput<-paroutputree;parvec<-parvectree; EvalString<-evalstr;
#nFunction="lm";fname<-paste(mypathout,nData,"_lmbsp_zelt",sep="");mDataSet<-DataSetz



