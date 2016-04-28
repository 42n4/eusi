# skrypt do zajęć EU SI: dane testowe
# TODO: test, testować
# Licence LGPL  
# Author: Piotr Wąsiewicz
########################################################################################################

##########################################################################################################
nData<-"glass"
#nData<-"nihills"
#nData<-"photocar"
#nData<-"meatspec"
#nData<-"skoliozaNIL1.csv"
#nData<-"narchecked.csv"

##########################################################################################################
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,nData,sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))

data(list=nData)
DataSet<-get(nData)

##########################################################################################################
if(nData=="glass"){
	#te atrybuty, co są faktorami, zmiennymi jakościowymi z kategoriami, etykietami
	etykiety=c("Type")
	#kruskal
	#w parnokruskal to co zostanie nie wysłane do isoMDS(daisy) np. parnokruskal=c("Type")
	not2kruskal=c()
	#atrybut pojedyńczy do zescorowania po jego dyskretnych wartościach
	vecfactorzesc=c("Type")
	#atrybuty w plotMDS do kolorowania powstałych punktów 
	zmiennain<-names(DataSet)
	#zmienne drzewa
	#zmienne, które nie wchodzą do drzewa m.in. jego liście, target, cel optymalizacji drzewa
	not2tree=c("Type")
	#liść drzewa, etykieta
	liscie=c("Type")
	#zmienne zależne i inne zbędne w regresji
	zalezne=c("Type","RI")
	#wybieramy zmienną zależną, target dla regresji, zwykle zmiennoprzecinkowy
	zmiennaout<-"RI"
}

##########################################################################################################

if(nData=="nihills"){
	etykiety=c()
	#kruskal
	not2kruskal=c()
	zmiennain<-names(DataSet)
	#zmienne drzewa
	not2tree=c('time')
	liscie=c('time')
	#zmienne zależne i inne zbędne w regresji
	zalezne=c("time")
	zmiennaout<-"time"
} 

##########################################################################################################
if(nData=="photocar"){
	etykiety=c("group","event","tumor")
	vecfactorzesc=c("event")
	#kruskal
	not2kruskal=c()
	zmiennain<-names(DataSet)
	#zmienne drzewa
	not2tree=c("group")
	liscie=c("group")
	#zmienne zależne i inne zbędne w regresji
	#parvecnolm=c("time","group","tumor","event")
	zalezne=c("time")
	zmiennaout<-"time"
}
##########################################################################################################
if(nData=="meatspec"){
	etykiety=c("")
	vecfactorzesc=c("")
	#kruskal
	not2kruskal=c()
	zmiennain<-names(DataSet)
	#zmienne drzewa
	not2tree=c("fat")
	liscie=c("fat")
	#zmienne zależne i inne zbędne w regresji
	zalezne=c("fat")
	zmiennaout<-"fat"
}

##########################################################################################################
if(nData=="skoliozaNIL1.csv"){
	etykiety=c()
	vecfactorzesc=c()
	#kruskal
	not2kruskal=c()
	zmiennain<-names(DataSet)
	#zmienne drzewa
	not2tree=c("NI")
	liscie=c("NI")
	#zmienne zależne i inne zbędne w regresji
	zalezne=c("NI")
	zmiennaout<-"NI"
}
