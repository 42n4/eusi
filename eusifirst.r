#zbi�r do zaj�� ISO

#te �cie�ki mypath i mypathout musz� istnie� w systemie
#to �cie�ka do plik�w wykonywalnych z R pow�oki za pomoc� source("...")
mypath<-"/home/pwas/workspaceusi/sir/"
#to �cie�ka do plik�w graficznych uzyskiwanych za pomoc� funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")

source(paste(mypath,"eusifunkcje.r",sep=""))

nData<-"Pacjenci"
nfile<-"pacjenci.csv"
#wczytywanie zbioru treningowego
assign(nData,read.csv(paste("file://",mypath,nfile,sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))
#library(mlbench)
#data(list=nData)

DataSet<-get(nData)
DataSet<-DataSet[-c(51),]

parvecfact=c("name","type","Amf","THC","alk","opiaty","ps")
#potem faktoryzuj� na wszelki wypadek kolumny, kt�re s� etykietami, maj� ju� zdyskretyzowane warto�ci; ich wyb�r jest w wektorze parvec
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfact))


parvec=setdiff(names(DataSet),parvecfact)
#najpierw defaktoryzuj� i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi, tak na wszelki wypadek, gdyby csv �le si� wczyta� (w przypadku zbior�w data() to tylko �wiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#parvec=setdiff(names(DataSet),c("type","number","name"))
#zetskoruj� wybrane kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,c("type"))

#if(nData=="Glass") parvec=setdiff(names(DataSet),c("Type"))
#dyskretyzuj� kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#if(nData=="Glass") parvec=setdiff(names(DataSet),c("Type"))
#dyskretyzuj� tak�e nie zeskorowane warto�ci zwyk�ego zbioru wczytanego na pocz�tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


#if(nData=="Glass") parvec=setdiff(names(DataSet),c("Type"))
#generuj� z daisy r�nice mi�dzy wierszami podanego zbioru i wprowadzam do isoMDS rzutuj�cego na dwa wymiary k=2 (jak si� pojawi� dwa takie same wiersze to wyrzuca b��d, dlatego na samym pocz�tku usuwa�em wiersze z Glass
#parvecfact=c("name","type","Amf","THC","alk","opiaty","ps")
parvecfact=c()
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSet, parvec, 2)

# uzyskany zbi�r punkt�w koloruj� r�nymi atrybutami podstawianymi do wzorzec i uzyskuj� wizualizacj� cech
parvec<-names(DataSetz)
fname<-paste(mypathout,nData,"_norm_nrmdis",sep="")
plotMDS.for.chosen(fname, nDataSets, DataSetd, parvec, 2) 


parvecfact=c()
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSetz, parvec, 2)

parvec<-names(DataSetz)
fname<-paste(mypathout,nData,"_zesc_zscdis",sep="")
wzorzec1=DataSet$type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvec, wzorzec1) 
