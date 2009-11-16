#zbiór do zajêæ ISO

#te ¶cie¿ki mypath i mypathout musz± istnieæ w systemie
#to ¶cie¿ka do plików wykonywalnych z R pow³oki za pomoc± source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/pwasiewi/"
#to ¶cie¿ka do plików graficznych uzyskiwanych za pomoc± funkcji plot i innych
mypathout<-paste(mypath,"obrazki/",sep="")

source(paste(mypath,"isowitfunkcje.r",sep=""))

nData<-"Glass"
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,"meatDataEN.csv",sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))
library(mlbench)
data(list=nData)

DataSet<-get(nData)

#usuwanie takich samych wierszy ze zbioru Glass, a dok³adnie jednego z nich
if(nData=="Glass") DataSet<-DataSet[-c(40),]
if(nData=="Glass") parvecfactor=c("Type")
if(nData=="Glass") parvec=setdiff(names(DataSet),parvecfactor)


#potem faktoryzujê na wszelki wypadek kolumny, które s± etykietami, maj± ju¿ zdyskretyzowane warto¶ci; ich wybór jest w wektorze parvecfactor
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))

#najpierw defaktoryzujê i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi, tak na wszelki wypadek, gdyby csv ¼le siê wczyta³ (w przypadku zbiorów data() to tylko æwiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#zetskorujê wybrane kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,c("Type"))

#dyskretyzujê kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzujê tak¿e nie zeskorowane warto¶ci zwyk³ego zbioru wczytanego na pocz±tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


#generujê z daisy ró¿nice miêdzy wierszami podanego zbioru i wprowadzam do isoMDS rzutuj±cego na dwa wymiary k=2 
#(jak siê pojawi± dwa takie same wiersze to wyrzuca b³±d, dlatego na samym pocz±tku usuwa³em wiersze z Glass)
#parvecfact=c("Type")
if(nData=="Glass") parvecfact=c()
parvec=setdiff(names(DataSet),parvecfact)
nDataSets<-KruskelMDS(DataSet, parvec, 2)

# uzyskany zbiór punktów kolorujê ró¿nymi atrybutami podstawianymi do wzorzec i uzyskujê wizualizacjê cech
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
nDataSets<-KruskelMDS(DataSet, parvec, 2)

if(nData=="Glass") parvec<-names(DataSet)
fname<-paste(mypathout,nData,"_norm_zscdis",sep="")
wzorzec1=DataSet$Type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvec, wzorzec1) 

t01<-rpart(Type~RI+Na+Mg+Al+Si+K+Ca+Ba+Fe,data=DataSet)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_nrm"))
t01<-rpart(Type~RI+Na+Mg+Al+Si+K+Ca+Ba+Fe,data=DataSetz)
zapisz_rpart(t01,paste(mypathout,nData,"_rpart_zsc"))





