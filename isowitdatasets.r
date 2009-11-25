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

if(nData=="Glass") parvecout=c("Type","RI")
parvec=setdiff(names(DataSet),parvecout)
if(nData=="Glass") moutput<-"RI"

#najlepsza regresja liniowa bez logarytm�w i innych - jedna,dwie,trzy itd. zmienne niezale�ne w permutacjach
#okazuje si�, �e r2 dla powy�ej 3 zmiennych nie zwi�ksza si� znacz�co dla zbioru Glass
#dla numvar=1 rysuje wykres punkt�w i regresji
for(numvar in 1:(length(parvec)-1)){
	perm<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
	ilist<-lmwithattr(DataSet,moutput,parvec,numvar)
	if(numvar<10)lbnv<-paste("lb0",numvar,sep="")else lbnv<-paste("lb",numvar,sep="")
	i<-ilist[[1]]; assign(lbnv,ilist[[2]]);
	if(i){
		if(numvar<10)mnv<-paste("m0",numvar,sep="")else mnv<-paste("m",numvar,sep="")
		if(numvar<10)smnv<-paste("sm0",numvar,sep="")else smnv<-paste("sm",numvar,sep="")
		assign(mnv,evalwithattr(lm,moutput,paste(perm[i,],collapse="+"),DataSet));
		assign(smnv,summary(get(mnv)))
		get(smnv)
		Vif(get(mnv))
	}
	if(numvar==1){
		plot(eval(parse(text=paste(moutput,"~",perm[i,]))),data=DataSet)
		abline(lsfit(eval(parse(text=paste("DataSet$",perm[i,]))), eval(parse(text=paste("DataSet$",moutput)))), col="red", lwd=3)
	}
}
#bp<-bptest(RI~Si,data=DataSet)
#rs2<-summary(m01)[c("r.squared", "adj.r.squared")] 


#get("permutations","package:gtools")(9,9,parvec)
#etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.5))
#t01<-evalwithattr(rpart,"Type",parvec,DataSet)
#oceny1=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%parvec)])
#oceny2=predict(t01,newdata=DataSet[-etykiety,which(names(DataSet)%in%parvec)],"class")
#table(predicted=oceny2, real=DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])
#pred1<-prediction(oceny1,DataSet[-etykiety,which(names(DataSet)%in%c("Type"))])

