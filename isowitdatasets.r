#zbi�r do zaj�� ISO

#te �cie�ki mypath i mypathout musz� istnie� w systemie
#to �cie�ka do plik�w wykonywalnych z R pow�oki za pomoc� source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"
#to �cie�ka do plik�w graficznych uzyskiwanych za pomoc� funkcji plot i innych
mypathout<-paste(mypath,"obrazki/",sep="")

source(paste(mypath,"isowitfunkcje.r",sep=""))

nData<-"Glass"
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,"meatDataEN.csv",sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))
#library(mlbench)
data(list=nData)

DataSet<-get(nData)

#usuwanie takich samych wierszy ze zbioru Glass, a dok�adnie jednego z nich
if(nData=="Glass") DataSet<-DataSet[-c(40),]
if(nData=="Glass") parvecfactor=c("Type")
if(nData=="Glass") parvec=setdiff(names(DataSet),parvecfactor)


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


#generuj� z daisy r�nice mi�dzy wierszami podanego zbioru i wprowadzam do isoMDS rzutuj�cego na dwa wymiary k=2 
#(jak si� pojawi� dwa takie same wiersze to wyrzuca b��d, dlatego na samym pocz�tku usuwa�em wiersze z Glass)
#parvecfact=c("Type")
if(nData=="Glass") parvecfact=c()
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSet, parvec, 2)

# uzyskany zbi�r punkt�w koloruj� r�nymi atrybutami podstawianymi do wzorzec i uzyskuj� wizualizacj� cech
if(nData=="Glass") parvec<-names(DataSet)
fname<-paste(mypathout,nData,"_norm_nrmdis",sep="")
wzorzec1=DataSet$Type
#pdm=1:length(unique(DataSet$Type))
#org=sort(unique(DataSet$Type))
#wzorzec1=2
plotMDS.for.chosen(fname, nDataSets, DataSetd, parvec, wzorzec1) 


#parvecfact=c("Type")
if(nData=="Glass") parvecfact=c()
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSetz, parvec, 2)

if(nData=="Glass") parvec<-names(DataSet)
fname<-paste(mypathout,nData,"_zesc_zscdis",sep="")
wzorzec1=DataSet$Type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvec, wzorzec1) 

if(nData=="Glass") parvecout=c("Type")
parvec=setdiff(names(DataSet),parvecout)
t01<-evalwithattr(rpart,"Type",parvec,DataSet)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_nrm"))
t01<-evalwithattr(rpart,"Type",parvec,DataSetz)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_zsc"))
t01<-evalwithattr(rpart,"Type",parvec,DataSetd)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_nrd"))
t01<-evalwithattr(rpart,"Type",parvec,DataSetzd)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_zsd"))

##########################################################################################################

#zbi�r zmiennych niezale�nych (teoretycznie)
if(nData=="Glass") parvecout=c("Type","RI")
parvec=setdiff(names(DataSet),parvecout)
#wybieramy zmienn� zale�n�
if(nData=="Glass") moutput<-"RI"

##########################################################################################################
#alpha to krytyczna warto�� prawdopodobie�stwa p dla statystyk bp i f 
alpha<-0.1
##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#bptest
nleven<--1; 
#wybieramy dane normalne
mDataSet<-DataSet
nobp<-permregres(paste(mypathout,nData,"_lmno_nobp"),mDataSet, moutput, parvec, nleven, alpha)
#wybieramy dane zeskorowane
mDataSet<-DataSetz
zebp<-permregres(paste(mypathout,nData,"_lmno_zebp"),mDataSet, moutput, parvec, nleven, alpha)
#wybieramy dane zeskalowane i zcentrowane
mDataSet<-DataSetm
#skalowanie likwiduje intercept wsp�czynnik przesuni�cia prostej
msbp<-permregres(paste(mypathout,nData,"_lmno_msbp"),mDataSet, moutput, parvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#brak test�w wariancji
nleven<-0; 
mDataSet<-DataSet
nono<-permregres(paste(mypathout,nData,"_lmno_nono"),mDataSet, moutput, parvec, nleven, alpha)
mDataSet<-DataSetz
zeno<-permregres(paste(mypathout,nData,"_lmno_zeno"),mDataSet, moutput, parvec, nleven, alpha)
mDataSet<-DataSetm
msno<-permregres(paste(mypathout,nData,"_lmno_msno"),mDataSet, moutput, parvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo�� przedzia��w, dla nleven < 0 bptest, dla nleven == 0 brak test�w wariancji
#brak test�w wariancji
nleven<-3; 
mDataSet<-DataSet
nolt<-permregres(paste(mypathout,nData,"_lmno_nolt"),mDataSet, moutput, parvec, nleven, alpha)
mDataSet<-DataSetz
zelt<-permregres(paste(mypathout,nData,"_lmno_zelt"),mDataSet, moutput, parvec, nleven, alpha)
mDataSet<-DataSetm
mslt<-permregres(paste(mypathout,nData,"_lmno_mslt"),mDataSet, moutput, parvec, nleven, alpha)


#bp<-bptest(RI~Si,data=DataSet)
#rs2<-summary(m01)[c("r.squared", "adj.r.squared")] 
#get("permutations","package:gtools")(9,9,parvec)
#etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.5))
#t01<-evalwithattr(rpart,"Type",parvec,DataSet)
#oceny1=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%parvec)])
#oceny2=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%parvec)],"class")
#table(predicted=oceny2, real=DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])
#pred1<-prediction(oceny1,DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])

