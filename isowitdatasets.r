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
library(mlbench)
data(list=nData)

DataSet<-get(nData)

#usuwanie takich samych wierszy ze zbioru Glass, a dok�adnie jednego z nich
if(nData=="Glass") DataSet<-DataSet[-c(40),]
if(nData=="Glass") parvecfactor=c("Type")
if(nData=="Glass") parvec=setdiff(names(DataSet),parvecfactor)


#potem faktoryzuj� na wszelki wypadek kolumny, kt�re s� etykietami, maj� ju� zdyskretyzowane warto�ci; ich wyb�r jest w wektorze parvec
#DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))
#DataSet<-unfactorto(DataSet, which(names(DataSet) %in% parvec))
indata<-DataSet
varcon<-which(names(DataSet) %in% parvec)
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

for(i in 1:ncol(DataSet)){
	if(i%in%which(names(DataSet) %in% parvec))
		DataSet[,i]<-as.numeric(DataSet[,i])
}

#najpierw defaktoryzuj� i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi, tak na wszelki wypadek, gdyby csv �le si� wczyta� (w przypadku zbior�w data() to tylko �wiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#zetskoruj� wybrane kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,c("Type"))

#dyskretyzuj� kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzuj� tak�e nie zeskorowane warto�ci zwyk�ego zbioru wczytanego na pocz�tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


#generuj� z daisy r�nice mi�dzy wierszami podanego zbioru i wprowadzam do isoMDS rzutuj�cego na dwa wymiary k=2 (jak si� pojawi� dwa takie same wiersze to wyrzuca b��d, dlatego na samym pocz�tku usuwa�em wiersze z Glass
parvecfact=c("Type")
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSet, parvec, 2)

# uzyskany zbi�r punkt�w koloruj� r�nymi atrybutami podstawianymi do wzorzec i uzyskuj� wizualizacj� cech
parvec<-names(DataSet)
fname<-paste(mypathout,nData,"_norm_nrmdis",sep="")
wzorzec1=DataSet$Type
#pdm=1:length(unique(DataSet$Type))
#org=sort(unique(DataSet$Type))
#wzorzec1=2
plotMDS.for.chosen(fname, nDataSets, DataSetd, parvec, wzorzec1) 


parvecfact=c("Type")
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSet, parvec, 2)

parvec<-names(DataSet)
fname<-paste(mypathout,nData,"_norm_zscdis",sep="")
wzorzec1=DataSet$Type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvec, wzorzec1) 
