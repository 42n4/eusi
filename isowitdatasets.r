# skrypt do zajêæ ISO WIT Kruskal, lm, rpart
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


##########################################################################################################
if(nData=="Glass"){
	#operacje kosmetyczne poprawiaj±ce dane, przystosowuj±ce
	#usuwanie takich samych wierszy ze zbioru Glass, a dok³adnie jednego z nich
	#(jak siê pojawi± dwa takie same wiersze to wyrzuca b³±d isoMDS, 
	# dlatego na samym pocz±tku usuwane s± niektóre wiersze z Glass)
	if(nData=="Glass") DataSet<-DataSet[-c(40),]
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
	#vecfactorzesc=rep(1,nrow(DataSet))
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
 	#te atrybuty, co s± faktorami, zmiennymi jako¶ciowymi z kategoriami, etykietami
	parvecfactor=c("group","event","tumor")
	#atrybut pojedyñczy do zescorowania po jego dyskretnych warto¶ciach
	vecfactorzesc=c("group")
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
latt2jpg(DataSet[,parvec],DataSetd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_norm",sep=""))
latt2jpg(DataSetz[,parvec],DataSetzd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_zesc",sep=""))
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
verr<-c();vmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.8))
classifier<-evalwithattr(rpart,paroutputree,parvectree,DataSet[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,])
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(rpart,paroutputree,parvectree,DataSetz[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,])
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(rpart,paroutputree,parvectree,DataSetd[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,])
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(rpart,paroutputree,parvectree,DataSetzd[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,])
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
zapisz_rpart(vmod[[1]],paste(mypathout,nData,"_rpart_nrm",sep=""))
zapisz_rpart(vmod[[2]],paste(mypathout,nData,"_rpart_zsc",sep=""))
zapisz_rpart(vmod[[3]],paste(mypathout,nData,"_rpart_nrd",sep=""))
zapisz_rpart(vmod[[4]],paste(mypathout,nData,"_rpart_zsd",sep=""))
verr
##########################################################################################################
#generujemy drzewa J48 dla parvectree
verr<-c();vmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.8))
classifier<-evalwithattr(J48,paroutputree,parvectree,DataSet[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,])
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(J48,paroutputree,parvectree,DataSetz[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,])
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(J48,paroutputree,parvectree,DataSetd[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,])
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(J48,paroutputree,parvectree,DataSetzd[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,])
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(J48,"Type",parvectree,DataSetms[etykiety,])
lres<-prederror(classifier,paroutputree,parvectree,DataSetms[-etykiety,])
vmod[[5]]<-classifier;verr<-c(verr,lres$perror)
zapisz_weka(vmod[[1]],paste(mypathout,nData,"_wkJ48_nrm",sep=""))
zapisz_weka(vmod[[2]],paste(mypathout,nData,"_wkJ48_zsc",sep=""))
zapisz_weka(vmod[[3]],paste(mypathout,nData,"_wkJ48_nrd",sep=""))
zapisz_weka(vmod[[4]],paste(mypathout,nData,"_wkJ48_zsd",sep=""))
zapisz_weka(vmod[[5]],paste(mypathout,nData,"_wkJ48_mes",sep=""))
verr
##########################################################################################################
#generujemy svm dla parvectree
verr<-c();vmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
#svmextra<-'kernel="linear"'
#svmextra<-'kernel="sigmoid"'
svmextra<-'kernel="polynomial"'
#svmextra<-'kernel="radial"'
classifier<-evalwithattr(svm,paroutputree,parvectree,DataSet[etykiety,],svmextra)
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,],svmextra)
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(svm,paroutputree,parvectree,DataSetz[etykiety,],svmextra)
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,],svmextra)
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(svm,paroutputree,parvectree,DataSetd[etykiety,],svmextra)
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,],svmextra)
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
classifier<-evalwithattr(svm,paroutputree,parvectree,DataSetzd[etykiety,],svmextra)
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,],svmextra)
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
verr

##########################################################################################################
#ipredknn
verr<-c();vmod<-list()
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
mDataSet<-DataSet[etykiety,]
classifier <- evalwithattr(ipredknn,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,])
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetz[etykiety,]
classifier <- evalwithattr(ipredknn,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,])
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetd[etykiety,]
classifier <- evalwithattr(ipredknn,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,])
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetzd[etykiety,]
classifier <- evalwithattr(ipredknn,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,])
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
verr

##########################################################################################################
#lda
verr<-c();vmod<-list()
evalstr<-"LDA"
parvectree=setdiff(names(DataSet),parvecnotree)
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.7))
mDataSet<-DataSet[etykiety,]
classifier <- evalwithattr(lda,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSet[-etykiety,],evalstr)
vmod[[1]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetz[etykiety,]
classifier <- evalwithattr(lda,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetz[-etykiety,],evalstr)
vmod[[2]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetd[etykiety,]
classifier <- evalwithattr(lda,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetd[-etykiety,],evalstr)
vmod[[3]]<-classifier;verr<-c(verr,lres$perror)
mDataSet<-DataSetzd[etykiety,]
classifier <- evalwithattr(lda,paroutputree,parvectree,mDataSet)
lres<-prederror(classifier,paroutputree,parvectree,DataSetzd[-etykiety,],evalstr)
vmod[[4]]<-classifier;verr<-c(verr,lres$perror)
verr

##########################################################################################################
#NaiveBayes,qda

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
