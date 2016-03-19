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
	parvecfactor=c("Type")
	#kruskal
	#w parnokruskal to co zostanie nie wysłane do isoMDS(daisy) np. parnokruskal=c("Type")
	parnokruskal=c()
	#atrybut pojedyńczy do zescorowania po jego dyskretnych wartościach
	vecfactorzesc=c("Type")
	#atrybuty w plotMDS do kolorowania powstałych punktów 
	parvecol<-names(DataSet)
	#zmienne drzewa
	#zmienne, które nie wchodzą do drzewa m.in. jego liście, target, cel optymalizacji drzewa
	parvecnotree=c("Type")
	#liść drzewa, etykieta
	paroutputree=c("Type")
	#zmienne zależne i inne zbędne w regresji
	parvecnolm=c("Type","RI")
	#wybieramy zmienną zależną, target dla regresji, zwykle zmiennoprzecinkowy
	moutput<-"RI"
}

##########################################################################################################

if(nData=="nihills"){
	parvecfactor=c()
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c('time')
	paroutputree=c('time')
	#zmienne zależne i inne zbędne w regresji
	parvecnolm=c("time")
	moutput<-"time"
} 

##########################################################################################################
if(nData=="photocar"){
	parvecfactor=c("group","event","tumor")
	vecfactorzesc=c("event")
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("group")
	paroutputree=c("group")
	#zmienne zależne i inne zbędne w regresji
	#parvecnolm=c("time","group","tumor","event")
	parvecnolm=c("time")
	moutput<-"time"
}
##########################################################################################################
if(nData=="meatspec"){
	parvecfactor=c("")
	vecfactorzesc=c("")
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("fat")
	paroutputree=c("fat")
	#zmienne zależne i inne zbędne w regresji
	parvecnolm=c("fat")
	moutput<-"fat"
}

##########################################################################################################
if(nData=="skoliozaNIL1.csv"){
	parvecfactor=c()
	vecfactorzesc=c()
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("NI")
	paroutputree=c("NI")
	#zmienne zależne i inne zbędne w regresji
	parvecnolm=c("NI")
	moutput<-"NI"
}
