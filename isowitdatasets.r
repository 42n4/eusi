# skrypt do zaj�� ISO WIT Kruskal, lm, rpart
# TODO: test, testowa�
# Licence LGPL  
# Author: Piotr W�siewicz
########################################################################################################

#te �cie�ki mypath i mypathout musz� istnie� w systemie
#to �cie�ka do plik�w wykonywalnych z R pow�oki za pomoc� source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"
#to �cie�ka do plik�w graficznych uzyskiwanych za pomoc� funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")
dir.create(mypathout, showWarnings = TRUE, recursive = TRUE, mode = "0755")
#Sys.chmod(paths, mode = "0755")


source(paste(mypath,"isowitfunkcje.r",sep=""))

nData<-"Glass"
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,"meatDataEN.csv",sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))
#library(mlbench)
data(list=nData)

DataSet<-get(nData)

#operacje kosmetyczne poprawiaj�ce dane, przystosowuj�ce
#usuwanie takich samych wierszy ze zbioru Glass, a dok�adnie jednego z nich
#(jak si� pojawi� dwa takie same wiersze to wyrzuca b��d isoMDS, 
# dlatego na samym pocz�tku usuwane s� niekt�re wiersze z Glass)
if(nData=="Glass") DataSet<-DataSet[-c(40),]


#te atrybuty, co s� faktorami, zmiennymi jako�ciowymi z kategoriami, etykietami
if(nData=="Glass") parvecfactor=c("Type")
#w parnokruskal to co zostanie nie wys�ane do isoMDS(daisy) np. parnokruskal=c("Type")
if(nData=="Glass") parnokruskal=c()
#atrybuty w plotMDS do kolorowania powsta�ych punkt�w 
if(nData=="Glass") parvecol<-names(DataSet)
#zmienne, kt�re nie wchodz� do drzewa m.in. jego li�cie, target, cel optymalizacji drzewa
if(nData=="Glass") paroutputree=c("Type")
#zmienne zale�ne i inne zb�dne w regresji
if(nData=="Glass") parvecnolm=c("Type","RI")
#wybieramy zmienn� zale�n�, target dla regresji, zwykle zmiennoprzecinkowy
if(nData=="Glass") moutput<-"RI"


#te atrybuty, kt�re s� zmiennymi ilo�ciowymi
parvec=setdiff(names(DataSet),parvecfactor)

#potem faktoryzuj� na wszelki wypadek kolumny, kt�re s� etykietami, maj� ju� zdyskretyzowane warto�ci; ich wyb�r jest w wektorze parvecfactor
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))

#najpierw defaktoryzuj� i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi, tak na wszelki wypadek, gdyby csv �le si� wczyta� (w przypadku zbior�w data() to tylko �wiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0 i sd=1
DataSetms<-scale_for(DataSet,parvec,TRUE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do sd=1
DataSets<-scale_for(DataSet,parvec,FALSE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0
DataSetm<-scale_for(DataSet,parvec,TRUE,FALSE)

#zetskoruj� wybrane kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,c("Type"))

#dyskretyzuj� kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzuj� tak�e nie zeskorowane warto�ci zwyk�ego zbioru wczytanego na pocz�tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)

#Kruskal dla normalnych danych
#generuj� z daisy r�nice mi�dzy wierszami podanego zbioru i wprowadzam do isoMDS rzutuj�cego na dwa wymiary k=2 
parveckruskal=setdiff(names(DataSet),parnokruskal)
nDataSets<-KruskelMDS(DataSet, parveckruskal, 2)

# uzyskany zbi�r punkt�w koloruj� r�nymi atrybutami podstawianymi do wzorzec i uzyskuj� wizualizacj� cech
fname<-paste(mypathout,nData,"_norm_nrmdis",sep="")
wzorzec1=DataSet$Type
#pdm=1:length(unique(DataSet$Type))
#org=sort(unique(DataSet$Type))
#wzorzec1=2
plotMDS.for.chosen(fname, nDataSets, DataSetd, parvecol, wzorzec1) 

#Kruskal dla zeskorowanych danych
parveckruskal=setdiff(names(DataSet),parnokruskal)
nDataSets<-KruskelMDS(DataSetz, parveckruskal, 2)

fname<-paste(mypathout,nData,"_zesc_zscdis",sep="")
wzorzec1=DataSet$Type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvecol, wzorzec1) 

#generujemy drzewa rpart dla parvectree
parvectree=setdiff(names(DataSet),paroutputree)
t01<-evalwithattr(rpart,"Type",parvectree,DataSet)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_nrm",sep=""))
t01<-evalwithattr(rpart,"Type",parvectree,DataSetz)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_zsc",sep=""))
t01<-evalwithattr(rpart,"Type",parvectree,DataSetd)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_nrd",sep=""))
t01<-evalwithattr(rpart,"Type",parvectree,DataSetzd)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_zsd",sep=""))

##########################################################################################################
#w mparvec zmienne niezale�ne do regresji
mparvec=setdiff(names(DataSet),parvecnolm)

##########################################################################################################
#alpha to krytyczna warto�� prawdopodobie�stwa p dla statystyk bp i f 
alpha<-0.1;
##########################################################################################################
#dla nleven > 0 le vena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#bptest
nleven<--1; 
#wybieramy dane normalne
mDataSet<-DataSet
lmabc_nobp<-permregres(paste(mypathout,nData,"_lmabc_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskorowane
mDataSet<-DataSetz
lmabc_zebp<-permregres(paste(mypathout,nData,"_lmabc_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskalowane i zcentrowane
mDataSet<-DataSetm
#skalowanie likwiduje intercept wsp�czynnik przesuni�cia prostej
lmabc_msbp<-permregres(paste(mypathout,nData,"_lmabc_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#brak test�w wariancji
nleven<-0; 
mDataSet<-DataSet
lmabc_nono<-permregres(paste(mypathout,nData,"_lmabc_nono",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetz
lmabc_zeno<-permregres(paste(mypathout,nData,"_lmabc_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetm
lmabc_msno<-permregres(paste(mypathout,nData,"_lmabc_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#brak test�w wariancji
nleven<-5; 
mDataSet<-DataSet
lmabc_nolt<-permregres(paste(mypathout,nData,"_lmabc_nolt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetz
lmabc_zelt<-permregres(paste(mypathout,nData,"_lmabc_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetm
lmabc_mslt<-permregres(paste(mypathout,nData,"_lmabc_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#Teraz u�yjemy splin�w czyli y~bs(x1)+bs(x2)+...
SPLINE<-TRUE;
##########################################################################################################
#alpha to krytyczna warto�� prawdopodobie�stwa p dla statystyk bp i f 
alpha<-0.1
##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#bptest
nleven<--1; 
#wybieramy dane normalne
mDataSet<-DataSet
lmbsp_nobp<-permregres(paste(mypathout,nData,"_lmbsp_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#wybieramy dane zeskorowane
mDataSet<-DataSetz
lmbsp_zebp<-permregres(paste(mypathout,nData,"_lmbsp_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#wybieramy dane zeskalowane i zcentrowane
mDataSet<-DataSetm
#skalowanie likwiduje intercept wsp�czynnik przesuni�cia prostej
lmbsp_msbp<-permregres(paste(mypathout,nData,"_lmbsp_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#brak test�w wariancji
nleven<-0; 
mDataSet<-DataSet
lmbsp_nono<-permregres(paste(mypathout,nData,"_lmbsp_nono",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
lmbsp_zeno<-permregres(paste(mypathout,nData,"_lmbsp_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetm
lmbsp_msno<-permregres(paste(mypathout,nData,"_lmbsp_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#leven
nleven<-5; 
mDataSet<-DataSet
lmbsp_nolt<-permregres(paste(mypathout,nData,"_lmbsp_nolt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
lmbsp_zelt<-permregres(paste(mypathout,nData,"_lmbsp_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetm
lmbsp_mslt<-permregres(paste(mypathout,nData,"_lmbsp_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)


#bp<-bptest(RI~Si,data=DataSet)
#rs2<-summary(m01)[c("r.squared", "adj.r.squared")] 
#get("permutations","package:gtools")(9,9,mparvec)
#etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.5))
#t01<-evalwithattr(rpart,"Type",mparvec,DataSet)
#oceny1=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%mparvec)])
#oceny2=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%mparvec)],"class")
#table(predicted=oceny2, real=DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])
#pred1<-prediction(oceny1,DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])

