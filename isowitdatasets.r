# skrypt do zajêæ ISO WIT: dane testowe
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

##########################################################################################################
#nData<-"Glass"
#nData<-"nihills"
#nData<-"photocar"
#nData<-"meatspec"

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
if(nData=="meatspec"){
	#usuwanie do Kruskala
	#DataSet<-DataSet[-c(13,25,32,33,34,36,37,67,64,77,52),]
	parvecfactor=c("")
	vecfactorzesc=c("")
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("fat")
	paroutputree=c("fat")
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("fat")
	moutput<-"fat"
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


