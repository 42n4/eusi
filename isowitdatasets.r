#zbiór do zajêæ ISO

#te ¶cie¿ki mypath i mypathout musz± istnieæ w systemie
#to ¶cie¿ka do plików wykonywalnych z R pow³oki za pomoc± source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"
#to ¶cie¿ka do plików graficznych uzyskiwanych za pomoc± funkcji plot i innych
mypathout<-paste(mypath,"obrazki/",sep="")

source(paste(mypath,"isowitfunkcje.r",sep=""))

nData<-"Glass"
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,"meatDataEN.csv",sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))
#library(mlbench)
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

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0 i sd=1
DataSetms<-scale_for(DataSet,parvec,TRUE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do sd=1
DataSets<-scale_for(DataSet,parvec,FALSE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0
DataSetm<-scale_for(DataSet,parvec,TRUE,FALSE)

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

#zbiór zmiennych niezale¿nych (teoretycznie)
if(nData=="Glass") parvecout=c("Type","RI")
parvec=setdiff(names(DataSet),parvecout)
#wybieramy zmienn± zale¿n±
if(nData=="Glass") moutput<-"RI"
#wybieramy dane normalne
mDataSet<-DataSet
#najlepsza regresja liniowa bez logarytmów i innych - jedna,dwie,trzy itd. zmienne niezale¿ne w permutacjach
#okazuje siê, ¿e r2 dla powy¿ej 3 zmiennych nie zwiêksza siê znacz±co dla zbioru Glass
#dla numvar=1 rysuje wykres punktów i regresji
n<-permregres(mDataSet, moutput, parvec)

#zbiór zmiennych niezale¿nych (teoretycznie)
if(nData=="Glass") parvecout=c("Type","RI")
parvec=setdiff(names(DataSet),parvecout)
#wybieramy zmienn± zale¿n±
if(nData=="Glass") moutput<-"RI"
#wybieramy dane zeskorowane
mDataSet<-DataSetz
#najlepsza regresja liniowa bez logarytmów i innych - jedna,dwie,trzy itd. zmienne niezale¿ne w permutacjach
#okazuje siê, ¿e dla danych zeskorowanych r2 dla powy¿ej 4 zmiennych nie zwiêksza siê znacz±co dla zbioru Glass
#zatem jest poprawa w stosunku do normalnych danych
#dla numvar=1 rysuje wykres punktów i regresji
z<-permregres(mDataSet, moutput, parvec)

#zbiór zmiennych niezale¿nych (teoretycznie)
if(nData=="Glass") parvecout=c("Type","RI")
parvec=setdiff(names(DataSet),parvecout)
#wybieramy zmienn± zale¿n±
if(nData=="Glass") moutput<-"RI"
#wybieramy dane zeskalowane i zcentrowane
mDataSet<-DataSetm
#najlepsza regresja liniowa bez logarytmów i innych - jedna,dwie,trzy itd. zmienne niezale¿ne w permutacjach
#okazuje siê, ¿e dla danych zeskorowanych r2 dla powy¿ej 4 zmiennych nie zwiêksza siê znacz±co dla zbioru Glass
#zatem jest poprawa w stosunku do normalnych danych
#dla numvar=1 rysuje wykres punktów i regresji
ms<-permregres(mDataSet, moutput, parvec)


#bp<-bptest(RI~Si,data=DataSet)
#rs2<-summary(m01)[c("r.squared", "adj.r.squared")] 
#get("permutations","package:gtools")(9,9,parvec)
#etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.5))
#t01<-evalwithattr(rpart,"Type",parvec,DataSet)
#oceny1=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%parvec)])
#oceny2=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%parvec)],"class")
#table(predicted=oceny2, real=DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])
#pred1<-prediction(oceny1,DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])

